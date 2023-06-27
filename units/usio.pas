unit usio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSIOInterruptProc = procedure(_b: byte) of object;
  TSIOTransmitProc = procedure(_b: byte) of object;

  TSIOchanneldes = (scdA,scdB); // SIO channel designator

  TSIO = class; // Preliminary designation

  TSIOchannel = class(TObject)
    private
      FDesignator: TSIOchanneldes;
//    FControl:    byte;
      FData:       byte;
      FOnTransmit: TSIOTransmitProc;
      FParent:     TSIO;
      FReceived:   byte;
      FRegRead:    array[0..2] of byte;
      FRegWrite:   array[0..7] of byte;
      function GetControl: byte;
      function GetData: byte;
      procedure SetControl(_b: byte);
      procedure SetData(_b: byte);
      procedure SetReceived(_b: byte);
    public
      constructor Create(_parent: TSIO; _designator: TSIOchanneldes);
      property Control: byte read GetControl write SetControl;
      property Data: byte    read GetData    write SetData;
      property OnTransmit: TSIOTransmitProc  write FOnTransmit;
      property Received: byte read FReceived write SetReceived;
  end;

  TSIO = class(TObject)
    private
      FChannelA:  TSIOchannel;
      FChannelB:  TSIOchannel;
      FOnInterrupt: TSIOInterruptProc;
    public
      constructor Create;
      destructor Destroy; override;
      procedure TriggerInterrupt;
      property ChannelA: TSIOchannel read FChannelA write FChannelA;
      property ChannelB: TSIOchannel read FChannelB write FChannelB;
      property OnInterrupt: TSIOInterruptProc write FOnInterrupt;
  end;

var
  SIO: TSIO;


implementation

uses
  uprocessor;


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
    FOnTransmit(_b);
end;

procedure TSIOchannel.SetReceived(_b: byte);
begin
  FReceived := _b;
  FRegRead[0] := FRegRead[0] or $01; // Set received char available bit
  // Finally trigger interrupt
  FParent.TriggerInterrupt;
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
end;

destructor TSIO.Destroy;
begin
  FreeAndNil(FChannelB);
  FreeAndNil(FChannelA);
  inherited Destroy;
end;

procedure TSIO.TriggerInterrupt;
begin
  // @@@@@ Trigger the interrupt here
  processor_interrupt(FChannelB.FRegWrite[2]);
end;

initialization
  SIO := TSIO.Create;

finalization
  FreeAndNil(SIO);

end.

