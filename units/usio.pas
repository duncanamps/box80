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
  SIO_RXBUF_SIZE = 32;
  SIO_FIFO_SIZE = 3;

type
  TSIOCanInterruptFunc = function(): boolean of object;
  TSIOInterruptProc = procedure(_b: byte) of object;
  TSIOTransmitFunc = function(_b: byte): boolean of object;
  TSIOCanTransmitFunc = function: boolean of object;

  TSIOchanneldes = (scdA,scdB); // SIO channel designator

  TSIO = class; // Preliminary designation

  TSIOreadArray  = array[0..2] of byte;
  TSIOwriteArray = array[0..7] of byte;

  TSIOchannel = class(TObject)
    private
      // SIO general items
      FDesignator:       TSIOchanneldes;
      FParent:           TSIO;
      FRegRead:          TSIOreadArray;
      FRegWrite:         TSIOwriteArray;
      // SIO receive items
      FCircular:         TCircularBuffer; // For serial input from keyboard
      FFIFO:             array[0..SIO_FIFO_SIZE-1] of byte;
      FFIFOcontains:     integer;
      FHasRxData:        boolean;
      FRxData:           byte;
      // SIO transmit items
      FOnCanTransmit:    TSIOCanTransmitFunc;
      FOnTransmit:       TSIOTransmitFunc;
      FTxData:           byte;

      function  GetControl: byte;
      function  GetData: byte;
      procedure SetControl(_b: byte);
      procedure SetData(_b: byte);
      procedure SetHasRxData(_v: boolean);
      procedure SetTXempty;
      procedure SetTXfull;

      {
      procedure SetRXavailable;
      procedure SetRXempty;
      HasRxData:     boolean;
      procedure AttemptWrite;
      function  PullFromFIFO: boolean;
      procedure SetReceived(_b: byte);

      }
    protected
      function  CanIdle: boolean;
      procedure FetchInput;
      procedure Init;
      function  IsTxEmpty: boolean;
      procedure PumpOutput;
    public
      constructor Create(_parent: TSIO; _designator: TSIOchanneldes);
      destructor Destroy; override;
      function  BufCapacity: integer;
      function  BufPercent:  double;
      procedure IncomingChar(_b: byte);
      procedure ReadFromXml(doc: TXMLDocument; const _prefix: string);
      function  RTS: boolean;
      procedure WriteToXml(doc: TXMLDocument; const _prefix: string);
      property Control:    byte read GetControl write SetControl;
      property Data:       byte    read GetData    write SetData;
      property HasRxData:  boolean read FHasRxData write SetHasRxData;
      property OnCanTransmit: TSIOCanTransmitFunc write FOnCanTransmit;
      property OnTransmit: TSIOTransmitFunc  write FOnTransmit;
      property RegRead:    TSIOreadArray    read FRegRead;
      property RegWrite:   TSIOwriteArray  read FRegWrite;
      {
      function  IsRXbusy: boolean;
      function  IsTXbusy: boolean;
      property Received: byte read FRXdata write SetReceived;
      }
  end;

  TSIO = class(TObject)
    private
      FChannelA:             TSIOchannel;
      FChannelB:             TSIOchannel;
      FInterruptOutstanding: boolean;
      FOnCanInterrupt:       TSIOCanInterruptFunc;
      FOnInterrupt:          TSIOInterruptProc;
      function CanIdle:      boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AcknowledgeInterrupt;
      function  CanInterrupt: boolean;
      procedure Process;
      procedure Reset;
      procedure TriggerInterrupt;
      {
      procedure ClockRX;
      }
      property ChannelA: TSIOchannel read FChannelA write FChannelA;
      property ChannelB: TSIOchannel read FChannelB write FChannelB;
      property OnCanInterrupt: TSIOCanInterruptFunc write FOnCanInterrupt;
      property OnInterrupt:    TSIOInterruptProc    write FOnInterrupt;
  end;

implementation


const
  SIO_STATUS_TX_EMPTY = $04;
  SIO_STATUS_RX_AVAILABLE = $01;
  SIO_STATUS_NOT_TX_EMPTY = (SIO_STATUS_TX_EMPTY xor $FF);
  SIO_STATUS_NOT_RX_AVAILABLE = (SIO_STATUS_RX_AVAILABLE xor $FF);



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
  FCircular   := TCircularBuffer.Create(SIO_RXBUF_SIZE);
  Init;
  // @@@@@ Perform any register initialisation here
end;

destructor TSIOchannel.Destroy;
begin
  FreeAndNil(FCircular);
  inherited Destroy;
end;

function TSIOchannel.BufCapacity: integer;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.BufCapacity()','Enter');
  {$ENDIF}
  Result := FCircular.Remaining;
  {$IFDEF DEBUG_SIO}
  siodebug.Log('TSIOchannel.BufCapacity()','Value = %d',[Result]);
  siodebug.LogX('TSIOchannel.BufCapacity()','Exit');
  {$ENDIF}
end;

function TSIOchannel.BufPercent: double;
begin
  Result := FCircular.PercentUsed;
end;

function TSIOchannel.CanIdle: boolean;
var rxidle: boolean;
    txidle: boolean;
begin
  // Returns true if there is absolutely nothing to do on the TX or RX side
  rxidle := (not HasRxData) and (FFIFOcontains = 0) and (FCircular.Contains = 0);
  txidle := IsTxEmpty;
  Result := rxidle and txidle;
end;

procedure TSIOchannel.FetchInput;
var i: integer;
    b: integer;
begin
  // Is there anything in the FIFO we can move into RxData
  if (not HasRxData) and (FFIFOcontains > 0) then
    begin
      FRxData := FFIFO[0];
      for i := 1 to SIO_FIFO_SIZE-1 do
        FFIFO[i-1] := FFIFO[i];
      Dec(FFIFOcontains);
      HasRxData := True;
    end;
  // Check if there is stuff in the circular buffer we can pump into the FIFO
  while RTS and (FFIFOcontains < SIO_FIFO_SIZE) and (FCircular.Contains > 0) do
    begin
      FCircular.DoCmd(CB_CMD_READ,b{%H-});
      FFIFO[FFIFOcontains] := Byte(b);
      Inc(FFIFOcontains);
    end;
  // Check if we can flag an interrupt
  if HasRxData and (not FParent.FInterruptOutstanding) then
    FParent.TriggerInterrupt();
end;

function TSIOchannel.GetData: byte;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.GetData()','Enter');
  {$ENDIF}
  Result := FRXdata;
  HasRxData := False;
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
var {%H-}ichar:    integer;
begin
  {$IFDEF DEBUG_SIO}
  siodebug.LogE('TSIOchannel.IncomingChar()','Enter');
  siodebug.Log('TSIOchannel.IncomingChar()','Byte to write is %d (%2.2X)',[_b,_b]);
  siodebug.Log('TSIOchannel.IncomingChar()','SIO receive capacity is %d',[FCircular.Capacity]);
  {$ENDIF}
  if FCircular.Remaining = 0 then
    begin
      raise Exception.Create('IncomingChar() buffer overflow');
      Exit; // Buffer overflow - we shouldn't be sending it characters
    end;
  ichar := _b;
  if not FCircular.DoCmd(CB_CMD_WRITE,ichar) then
    raise Exception.Create('Overrun on buffer write');
//SetRXavailable;
  {$IFDEF DEBUG_SIO}
  siodebug.LogX('TSIOchannel.IncomingChar()','Exit');
  {$ENDIF}
end;

procedure TSIOchannel.Init;
var i: integer;
    b: integer;
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
  FCircular.DoCmd(CB_CMD_RESET,b{%H-});
  FFIFOcontains := 0;
  HasRxData := False;
  {$IFDEF DEBUG_SIO}
  siodebug.LogX('TSIOchannel.IncomingInit()','Exit');
  {$ENDIF}
end;

function TSIOchannel.IsTXempty: boolean;
begin
  Result := (FRegRead[0] and SIO_STATUS_TX_EMPTY) <> 0;
end;

procedure TSIOchannel.PumpOutput;
begin
  if (not IsTxEmpty) and Assigned(FOnCanTransmit) and FOnCanTransmit() then
    if Assigned(FOnTransmit) then
      begin
        FOnTransmit(FTxData);
        SetTxEmpty;
      end;
end;

procedure TSIOchannel.ReadFromXml(doc: TXMLDocument; const _prefix: string);
var r: integer;
    node: TDOMnode;
begin
  Init;
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
      FTxData := _b;
      SetTXfull;
    end
  else
    FRegWrite[reg_no] := _b;
end;

procedure TSIOchannel.SetHasRxData(_v: boolean);
begin
  FHasRxData := _v;
  // Set SIO flags
  if _v then
    FRegRead[0] := FRegRead[0] or SIO_STATUS_RX_AVAILABLE
  else
    FRegRead[0] := FRegRead[0] and SIO_STATUS_NOT_RX_AVAILABLE;
end;

procedure TSIOchannel.SetTXempty;
begin
  FRegRead[0] := FRegRead[0] or SIO_STATUS_TX_EMPTY; // Set bit 2
end;

procedure TSIOchannel.SetTXfull;
begin
  FRegRead[0] := FRegRead[0] and SIO_STATUS_NOT_TX_EMPTY; // Clear bit 2
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

function TSIO.CanIdle: boolean;
begin
  Result := FChannelA.CanIdle and FChannelB.CanIdle;
end;

function TSIO.CanInterrupt: boolean;
begin
  if Assigned(FOnCanInterrupt) then
    Result := FOnCanInterrupt()
  else
    Result := False;
end;

procedure TSIO.Process;
begin
  ChannelA.FetchInput;
  ChannelB.FetchInput;
  ChannelA.PumpOutput;
  ChannelB.PumpOutput;
end;

procedure TSIO.Reset;
begin
  FChannelA.Init;
  FChannelB.Init;
  FInterruptOutstanding := False;
end;

procedure TSIO.AcknowledgeInterrupt;
begin
  FInterruptOutstanding := False;
end;

procedure TSIO.TriggerInterrupt;
begin
  if Assigned(FOnCanInterrupt) and Assigned(FOnInterrupt) then
    begin
      FInterruptOutstanding := True;
      FOnInterrupt(FChannelB.FRegWrite[2]);
    end;
end;


end.

