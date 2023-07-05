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
  Classes, SysUtils, uxml, DOM, XMLWrite, XMLRead;

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
//    FControl:    byte;
      FData:       byte;
      FOnTransmit: TSIOTransmitProc;
      FParent:     TSIO;
      FReceived:   byte;
      FRegRead:    TSIOreadArray;
      FRegWrite:   TSIOwriteArray;
      function GetControl: byte;
      function GetData: byte;
      procedure SetControl(_b: byte);
      procedure SetData(_b: byte);
      procedure SetReceived(_b: byte);
      procedure SetTXfull;
      procedure SetTXempty;
    protected
      procedure Init;
    public
      constructor Create(_parent: TSIO; _designator: TSIOchanneldes);
      procedure ReadFromXml(doc: TXMLDocument; const _prefix: string);
      property Control: byte read GetControl write SetControl;
      property Data: byte    read GetData    write SetData;
      property OnTransmit: TSIOTransmitProc  write FOnTransmit;
      property Received: byte read FReceived write SetReceived;
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
      procedure Init;
      procedure TriggerInterrupt;
      property ChannelA: TSIOchannel read FChannelA write FChannelA;
      property ChannelB: TSIOchannel read FChannelB write FChannelB;
      property OnInterrupt: TSIOInterruptProc write FOnInterrupt;
  end;

implementation


//-----------------------------------------------------------------------------
//
//  TSIOchannel code
//
//-----------------------------------------------------------------------------

constructor TSIOchannel.Create(_parent: TSIO; _designator: TSIOchanneldes);
begin
  FParent     := _parent;
  FDesignator := _designator;
  // @@@@@ Perform any register initialisation here
end;

function TSIOchannel.GetData: byte;
begin
  Result := FReceived;
  FRegRead[0] := FRegRead[0] and $7E; // Mask received char available bit
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

procedure TSIOchannel.Init;
var i: integer;
begin
  for i := 0 to 7 do
    FRegWrite[i] := 0;
  for i := 0 to 2 do
    FRegRead[i] := 0;
  SetTXempty;
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

procedure TSIOchannel.SetControl(_b: byte);
var index: integer;
begin
  index := FRegWrite[0] and $07; // Get the index to write to
  FRegWrite[index] := _b;
  if (index > 0) then
    FRegWrite[0] := FRegWrite[0] and $F8; // Set index back to 0 for next cmd
end;

procedure TSIOchannel.SetData(_b: byte);
begin
  FData := _b;
  if ((FRegWrite[0] and $07) = 0) and Assigned(FOnTransmit) then
    begin
      SetTXfull;
      FOnTransmit(_b);
      SetTXempty;
    end;
end;

procedure TSIOchannel.SetReceived(_b: byte);
begin
  FReceived := _b;
  FRegRead[0] := FRegRead[0] or $01; // Set received char available bit
  // Finally trigger interrupt
  FParent.TriggerInterrupt;
end;

procedure TSIOchannel.SetTXempty;
begin
  FRegRead[0] := FRegRead[0] or $02; // Set bit 2
end;

procedure TSIOchannel.SetTXfull;
begin
  FRegRead[0] := FRegRead[0] and $FD; // Clear bit 2
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

