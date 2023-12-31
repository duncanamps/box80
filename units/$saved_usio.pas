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
  Classes, SysUtils, uxml, DOM,
{$IFDEF DEBUG_SIO}
  SyncObjs,
{$ENDIF}
  ucircular;

const
  SIO_FIFO_SIZE = 8192;

type
  TSIOCanInterruptFunc = function(): boolean of object;
  TSIOInterruptProc = procedure(_b: byte) of object;
  TSIOTransmitFunc = function(_b: byte): boolean of object;

  TSIOchanneldes = (scdA,scdB); // SIO channel designator

  TSIO = class; // Preliminary designation

  TSIOreadArray  = array[0..2] of byte;
  TSIOwriteArray = array[0..7] of byte;

  {$IFDEF DEBUG_SIO}
  TSIOdebugObject = class(TObject)
    private
      FCS: TCriticalSection;
      FIndent: integer;
      FStream: TFileStream;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Log(_locus: string; _msg: string);
      procedure Log(_locus: string; const _fmt: string; const _args: array of const);
      procedure LogE(_locus: string; _msg: string);
      procedure LogX(_locus: string; _msg: string);
  end;
  {$ENDIF}

  TSIOchannel = class(TObject)
    private
      FDesignator: TSIOchanneldes;
//    FControl:      byte;
      FRxData:       byte;
      FTxData:       byte;
      FOnTransmit:   TSIOTransmitFunc;
      FParent:       TSIO;
      FRegRead:      TSIOreadArray;
      FRegWrite:     TSIOwriteArray;
      FCircular:     TCircularBuffer;
      HasRxData:     boolean;
      procedure AttemptWrite;
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
      property OnTransmit: TSIOTransmitFunc  write FOnTransmit;
      property Received: byte read FRXdata write SetReceived;
      property RegRead: TSIOreadArray    read FRegRead;
      property RegWrite: TSIOwriteArray  read FRegWrite;
  end;

  TSIO = class(TObject)
    private
      FChannelA:        TSIOchannel;
      FChannelB:        TSIOchannel;
      FInterruptNeeded: boolean;
      FOnCanInterrupt:  TSIOCanInterruptFunc;
      FOnInterrupt:     TSIOInterruptProc;
    public
      constructor Create;
      destructor Destroy; override;
      procedure ClockRX;
      procedure Reset;
      procedure TriggerInterrupt;
      property ChannelA: TSIOchannel read FChannelA write FChannelA;
      property ChannelB: TSIOchannel read FChannelB write FChannelB;
      property OnCanInterrupt: TSIOCanInterruptFunc write FOnCanInterrupt;
      property OnInterrupt:    TSIOInterruptProc    write FOnInterrupt;
  end;

implementation


const
  SIO_TX_EMPTY = $04;
  SIO_RX_AVAILABLE = $01;
  SIO_NOT_TX_EMPTY = (SIO_TX_EMPTY xor $FF);
  SIO_NOT_RX_AVAILABLE = (SIO_RX_AVAILABLE xor $FF);



//-----------------------------------------------------------------------------
//
//  TSIOdebugObject code
//
//-----------------------------------------------------------------------------

{$IFDEF DEBUG_SIO}

var
    siodebug: TSIOdebugObject;

constructor TSIOdebugObject.Create;
begin
  inherited Create;
  FCS := TCriticalSection.create;
  FStream := TFileStream.Create('C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\test_files\debug\siodebug.log',fmCreate);
  FIndent := 0;
end;

destructor TSIOdebugObject.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TSIOdebugObject.Log(_locus: string; _msg: string);

  procedure WriteStr(_s: string);
  begin
    if FIndent = 0 then
      _s := _s + #13 + #10
    else
      _s := Space(FIndent*2) + _s + #13 + #10;
    FStream.Write(_s[1],Length(_s));
  end;

begin

  WriteStr(_locus + ': ' + _msg);
end;

procedure TSIOdebugObject.Log(_locus: string; const _fmt: string; const _args: array of const);
begin
  Log(_locus,Format(_fmt,_args));
end;

procedure TSIOdebugObject.LogE(_locus: string; _msg: string);
begin
  FCS.Enter;
  Log(_locus,_msg);
  Inc(FIndent);
end;

procedure TSIOdebugObject.LogX(_locus: string; _msg: string);
begin
  Dec(FIndent);
  Log(_locus,_msg);
  FCS.Leave;
end;

{$ENDIF} // IFDEF DEBUG_SIO



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

procedure TSIOchannel.AttemptWrite;
begin // Only call if we have a character in TX buffer to send
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.AttemptWrite()','Enter');
  {$ENDIF}
  if FOnTransmit(FTxData) then
    SetTXempty;
  {$IFDEF DEBUG_SIO}
  siodebug.LogX('TSIOchannel.AttemptWrite()','Exit');
  {$ENDIF}
end;

function TSIOchannel.BufCapacity: byte;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.BufCapacity()','Enter');
  {$ENDIF}
  Result := FCircular.Capacity;
  {$IFDEF DEBUG_SIO}
  siodebug.Log('TSIOchannel.BufCapacity()','Value = %d',[Result]);
  siodebug.LogX('TSIOchannel.BufCapacity()','Exit');
  {$ENDIF}
end;

function TSIOchannel.GetData: byte;
var b: byte;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.GetData()','Enter');
  {$ENDIF}
  Result := FRXdata;
  HasRxData := False;
  FCircular.DoCmd(CB_CMD_CONTAINS,b);
  if b = 0 then
    SetRXempty;
  {$IFDEF DEBUG_SIO}
  siodebug.Log('TSIOchannel.GetData()','Result = %d ($%2.2X)',[Result,Result]);
  siodebug.LogX('TSIOchannel.GetData()','Exit');
  {$ENDIF}
end;

function TSIOchannel.GetControl: byte;
var index: integer;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.GetControl()','Enter');
  {$ENDIF}
  index := FRegWrite[0] and $07; // Get the index to read from
  if (index > 2) then
    raise Exception.Create('Attempt to read from unimplemented SIO register');
  Result := FRegRead[index];
  if (index > 0) then
    FRegWrite[0] := FRegWrite[0] and $F8; // Set index back to 0 for next cmd
  {$IFDEF DEBUG_SIO}
  siodebug.Log('TSIOchannel.GetControl()','Index = %d, Result = %d ($%2.2X)',[index,Result,Result]);
  siodebug.LogX('TSIOchannel.GetControl()','Exit');
  {$ENDIF}
end;

procedure TSIOchannel.IncomingChar(_b: byte);
var next_ptr: integer;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.IncomingChar()','Enter');
  siodebug.Log('TSIOchannel.IncomingChar()','Byte to write is %d (%2.2X)',[_b,_b]);
  siodebug.Log('TSIOchannel.IncomingChar()','SIO receive capacity is %d',[FCircular.Capacity]);
  {$ENDIF}
  if FCircular.Capacity = 0 then
    begin
      raise Exception.Create('IncomingChar() buffer overflow');
      Exit; // FIFO overflow - we shouldn't be sending it characters
    end;
  if not FCircular.DoCmd(CB_CMD_WRITE,_b) then
    raise Exception.Create('Overrun on buffer write');
  SetRXavailable;
  {$IFDEF DEBUG_SIO}
  siodebug.LogX('TSIOchannel.IncomingChar()','Exit');
  {$ENDIF}
end;

procedure TSIOchannel.Init;
var i: integer;
    b: byte;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.IncomingInit()','Enter');
  {$ENDIF}
  for i := 0 to 7 do
    FRegWrite[i] := 0;
  for i := 0 to 2 do
    FRegRead[i] := 0;
  // Set some other stuff up
  SetTXempty;
  SetRXempty;
  FCircular.DoCmd(CB_CMD_RESET,b);
  {$IFDEF DEBUG_SIO}
  siodebug.LogX('TSIOchannel.IncomingInit()','Exit');
  {$ENDIF}
end;

function TSIOchannel.IsRXbusy: boolean;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.IsRxBusy()','Enter');
  {$ENDIF}
  Result := (FRegRead[0] and SIO_RX_AVAILABLE) <> 0;
  {$IFDEF DEBUG_SIO}
  siodebug.Log('TSIOchannel.IsRxBusy()','Result = %s',[BoolToStr(Result)]);
  siodebug.LogX('TSIOchannel.IsRxBusy()','Exit');
  {$ENDIF}
end;

function TSIOchannel.IsTXbusy: boolean;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.IsTxBusy()','Enter');
  {$ENDIF}
  Result := (FRegRead[0] and SIO_TX_EMPTY) = 0;
  {$IFDEF DEBUG_SIO}
  siodebug.Log('TSIOchannel.IsTxBusy()','Result = %s',[BoolToStr(Result)]);
  siodebug.LogX('TSIOchannel.IsTxBusy()','Exit');
  {$ENDIF}
end;

function TSIOchannel.PullFromFIFO: boolean;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.PullFromFIFO()','Enter');
  {$ENDIF}
  Result := False;
  if (not HasRxData) and RTS then
    begin
      HasRxData := HasRxData or FCircular.DoCmd(CB_CMD_READ,FRxData);
      Result := HasRxData;
    end;
  {$IFDEF DEBUG_SIO}
  siodebug.Log('TSIOchannel.PullFromFIFO()','Result = %s',[BoolToStr(Result)]);
  siodebug.LogX('TSIOchannel.PullFromFIFO()','Exit');
  {$ENDIF}
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
        {
        FOnTransmit(_b);
        SetTXempty;
        }
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
  {$IFDEF DEBUG_SIO}
  SIOdebug := TSIOdebugObject.Create;
  {$ENDIF}
  FChannelA := TSIOchannel.Create(Self,scdA);
  FChannelB := TSIOchannel.Create(Self,scdB);
  Reset;
end;

destructor TSIO.Destroy;
begin
  FreeAndNil(FChannelB);
  FreeAndNil(FChannelA);
  {$IFDEF DEBUG_SIO}
  FreeAndNil(SIOdebug);
  {$ENDIF}
  inherited Destroy;
end;

procedure TSIO.ClockRX;
begin
  // SIO writes to "screen"
  if FChannelA.IsTxBusy then
    FChannelA.AttemptWrite;
  if FChannelB.IsTxBusy then
    FChannelB.AttemptWrite;
  // SIO reads from "keyboard"
  if not FInterruptNeeded then
    begin
      FChannelA.PullFromFIFO;
      FChannelB.PullFromFIFO;
    end;
  if FChannelA.HasRxData or FChannelB.HasRxData then
    FInterruptNeeded := True;
  if FInterruptNeeded and
     Assigned(FOnCanInterrupt) and
     FOnCanInterrupt() then
    begin
      FInterruptNeeded := False;
      TriggerInterrupt;
    end;
end;

procedure TSIO.Reset;
begin
  FChannelA.Init;
  FChannelB.Init;
  FInterruptNeeded := False;
end;

procedure TSIO.TriggerInterrupt;
begin
  // @@@@@ Trigger the interrupt here
  if Assigned(FOnInterrupt) then
    FOnInterrupt(FChannelB.FRegWrite[2]);
end;

end.

