{
    BOX80 - Z80 Virtual Machine
    Copyright (C)2023 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

unit usio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uxml, DOM, ucircular;

const
  SIO_FIFO_SIZE = 3;

type
  TSIOInterruptProc = procedure(_b: byte) of object;
  TSIOTransmitProc = procedure(_b: byte) of object;

  TSIOchanneldes = (scdA,scdB); // SIO channel designator

  TSIO = class; // Preliminary designation

  TSIOreadArray  = array[0..2] of byte;
  TSIOwriteArray = array[0..7] of byte;

  TSIOchannel = class(TObject)
    private
      FDesignator: TSIOchanneldes;
//    FControl:      byte;
      FRxData:       byte;
      FTxData:       byte;
      FOnTransmit:   TSIOTransmitProc;
      FParent:       TSIO;
      FRegRead:      TSIOreadArray;
      FRegWrite:     TSIOwriteArray;
      FCircular:     TCircularBuffer;
      HasRxData:     boolean;
      function  GetControl: byte;
      function  GetData: byte;
      function  PullFromFIFO: boolean;
      procedure SetControl(_b: byte);
      procedure SetData(_b: byte);
      procedure SetReceived(_b: byte);
      procedure SetRXempty;
      procedure SetRXavailable;
      procedure SetTXempty;
      procedure SetTXfull;
    protected
      procedure Init;
    public
      constructor Create(_parent: TSIO; _designator: TSIOchanneldes);
      destructor Destroy; override;
      function  BufCapacity: byte;
      procedure IncomingChar(_b: byte);
      function  IsRXbusy: boolean;
      function  IsTXbusy: boolean;
      procedure ReadFromXml(doc: TXMLDocument; const _prefix: string);
      function  RTS: boolean;
      procedure WriteToXml(doc: TXMLDocument; const _prefix: string);
      property Control: byte read GetControl write SetControl;
      property Data: byte    read GetData    write SetData;
      property OnTransmit: TSIOTransmitProc  write FOnTransmit;
      property Received: byte read FRXdata write SetReceived;
      property RegRead: TSIOreadArray    read FRegRead;
      property RegWrite: TSIOwriteArray  read FRegWrite;
  end;

  TSIO = class(TObject)
    private
      FChannelA:  TSIOchannel;
      FChannelB:  TSIOchannel;
      FOnInterrupt: TSIOInterruptProc;
    public
      constructor Create;
      destructor Destroy; override;
      procedure ClockRX;
      procedure Init;
      procedure TriggerInterrupt;
      property ChannelA: TSIOchannel read FChannelA write FChannelA;
      property ChannelB: TSIOchannel read FChannelB write FChannelB;
      property OnInterrupt: TSIOInterruptProc write FOnInterrupt;
  end;

implementation


const
  SIO_TX_EMPTY = $04;
  SIO_RX_AVAILABLE = $01;
  SIO_NOT_TX_EMPTY = (SIO_TX_EMPTY xor $FF);
  SIO_NOT_RX_AVAILABLE = (SIO_RX_AVAILABLE xor $FF);

//-----------------------------------------------------------------------------
//
//  TSIOchannel code
//
//-----------------------------------------------------------------------------

constructor TSIOchannel.Create(_parent: TSIO; _designator: TSIOchanneldes);
begin
  inherited Create;
  FParent     := _parent;
  FDesignator := _designator;
  FCircular   := TCircularBuffer.Create(SIO_FIFO_SIZE);
  Init;
  // @@@@@ Perform any register initialisation here
end;

destructor TSIOchannel.Destroy;
begin
  FreeAndNil(FCircular);
  inherited Destroy;
end;

function TSIOchannel.BufCapacity: byte;
var b: byte;
begin
  FCircular.DoCmd(CB_CMD_CAPACITY,b);
  Result := b;
end;

function TSIOchannel.GetData: byte;
var b: byte;
begin
  Result := FRXdata;
  HasRxData := False;
  FCircular.DoCmd(CB_CMD_CONTAINS,b);
  if b = 0 then
    SetRXempty;
end;

function TSIOchannel.GetControl: byte;
var index: integer;
begin
  index := FRegWrite[0] and $07; // Get the index to read from
  if (index > 2) then
    raise Exception.Create('Attempt to read from unimplemented SIO register');
  Result := FRegRead[index];
  if (index > 0) then
    FRegWrite[0] := FRegWrite[0] and $F8; // Set index back to 0 for next cmd
end;

procedure TSIOchannel.IncomingChar(_b: byte);
var next_ptr: integer;
begin
  {
  if FIFOfull then
    begin
      siodebugObj.LogInfo('TSIOchannel.IncomingChar()','Exit due to overflow');
      Exit; // FIFO overflow - we shouldn't be sending it characters
    end;
  }
  if not FCircular.DoCmd(CB_CMD_WRITE,_b) then
    raise Exception.Create('Overrun on buffer write');
  SetRXavailable;
end;

procedure TSIOchannel.Init;
var i: integer;
    b: byte;
begin
  for i := 0 to 7 do
    FRegWrite[i] := 0;
  for i := 0 to 2 do
    FRegRead[i] := 0;
  // Set some other stuff up
  SetTXempty;
  SetRXempty;
  FCircular.DoCmd(CB_CMD_RESET,b);
end;

function TSIOchannel.IsRXbusy: boolean;
begin
  Result := (FRegRead[0] and $01) <> 0;
end;

function TSIOchannel.IsTXbusy: boolean;
begin
  Result := (FRegRead[0] and $02) = 0;
end;

function TSIOchannel.PullFromFIFO: boolean;
begin
  Result := False;
  if HasRxData then
    Exit;
  HasRxData := HasRxData or FCircular.DoCmd(CB_CMD_READ,FRxData);
  Result := HasRxData;
end;

procedure TSIOchannel.ReadFromXml(doc: TXMLDocument; const _prefix: string);
var r: integer;
    node: TDOMnode;
begin
  node := doc.DocumentElement.FindNode('sio' + _prefix{%H-});
  for r := 0 to 2 do
    GetXmlByteP(node,'read' + IntToStr(r),@RegRead[r]);
  for r := 0 to 7 do
    GetXmlByteP(node,'write' + IntToStr(r),@RegWrite[r]);
end;

function TSIOchannel.RTS: boolean;
begin
  Result := (FRegWrite[5] and $02) <> 0;
end;

procedure TSIOchannel.SetControl(_b: byte);
var index: integer;
begin
  index := FRegWrite[0] and $07; // Get the index to write to
  FRegWrite[index] := _b;
  if (index > 0) then
    FRegWrite[0] := FRegWrite[0] and $F8; // Set index back to 0 for next cmd
end;

procedure TSIOchannel.SetData(_b: byte);
var reg_no: byte;
begin
  reg_no := FRegWrite[0] and $07;
  if reg_no = 0 then
    begin
    if Assigned(FOnTransmit) then
      begin
        FTxData := _b;
        SetTXfull;
        FOnTransmit(_b);
        SetTXempty;
      end
    end
  else
    FRegWrite[reg_no] := _b;
end;

procedure TSIOchannel.SetReceived(_b: byte);
begin
  {
  while FIFOfull do
    Sleep(10);
  while IsRxBusy do
      Sleep(20);
  FRXdata := _b;
  SetRXavailable;
  // Finally trigger interrupt
  FParent.TriggerInterrupt;
  }
end;

procedure TSIOchannel.SetRXempty;
begin
  FRegRead[0] := FRegRead[0] and SIO_NOT_RX_AVAILABLE; // Clear bit 1
end;

procedure TSIOchannel.SetRXavailable;
begin
  FRegRead[0] := FRegRead[0] or SIO_RX_AVAILABLE; // Set bit 1
end;

procedure TSIOchannel.SetTXempty;
begin
  FRegRead[0] := FRegRead[0] or SIO_TX_EMPTY; // Set bit 2
end;

procedure TSIOchannel.SetTXfull;
begin
  FRegRead[0] := FRegRead[0] and SIO_NOT_TX_EMPTY; // Clear bit 2
end;

procedure TSIOchannel.WriteToXml(doc: TXMLDocument; const _prefix: string);
var r: integer;
    node: TDOMnode;
begin
  node := doc.CreateElement('sio' + _prefix{%H-});
  for r := 0 to 2 do
    PutXmlByte(node,'read' + IntToStr(r),RegRead[r]);
  for r := 0 to 7 do
    PutXmlByte(node,'write' + IntToStr(r),RegWrite[r]);
  doc.ChildNodes[0].AppendChild(node)
end;

//-----------------------------------------------------------------------------
//
//  TSIO code
//
//-----------------------------------------------------------------------------

constructor TSIO.Create;
begin
  inherited Create;
  FChannelA := TSIOchannel.Create(Self,scdA);
  FChannelB := TSIOchannel.Create(Self,scdB);
  Init;
end;

destructor TSIO.Destroy;
begin
  FreeAndNil(FChannelB);
  FreeAndNil(FChannelA);
  inherited Destroy;
end;

procedure TSIO.ClockRX;
begin
  if FChannelA.PullFromFIFO then
    TriggerInterrupt;
  if FChannelB.PullFromFIFO then
    TriggerInterrupt;
end;

procedure TSIO.Init;
begin
  FChannelA.Init;
  FChannelB.Init;
end;

procedure TSIO.TriggerInterrupt;
begin
  // @@@@@ Trigger the interrupt here
  if Assigned(FOnInterrupt) then
    FOnInterrupt(FChannelB.FRegWrite[2]);
end;

end.

