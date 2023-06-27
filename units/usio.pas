unit usio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSIOInterruptProc = procedure(_b: byte) of object;
  TSIOTransmitProc = procedure(_b: byte) of object;

  TSIO = class(TObject)
    private
      FControlA:  byte;
      FControlB:  byte;
      FDataA:     byte;
      FDataB:     byte;
      FReceivedA: byte;
      FReceivedB: byte;
      FOnInterrupt: TSIOInterruptProc;
      FOnTransmitA: TSIOTransmitProc;
      FOnTransmitB: TSIOTransmitProc;
      function GetDataA: byte;
      function GetDataB: byte;
      procedure SetControlA(_b: byte);
      procedure SetControlB(_b: byte);
      procedure SetDataA(_b: byte);
      procedure SetDataB(_b: byte);
      procedure SetReceivedA(_b: byte);
      procedure SetReceivedB(_b: byte);
    public
      property ControlA: byte write SetControlA;
      property ControlB: byte write SetControlB;
      property DataA: byte    read GetDataA   write SetDataA;
      property DataB: byte    read GetDataB   write SetDataB;
      property OnInterrupt: TSIOInterruptProc write FOnInterrupt;
      property OnTransmitA: TSIOTransmitProc write FOnTransmitA;
      property OnTransmitB: TSIOTransmitProc write FOnTransmitB;
      property ReceivedA: byte read FReceivedA write SetReceivedA;
      property ReceivedB: byte read FReceivedB write SetReceivedB;
  end;

var
  SIO: TSIO;


implementation

function TSIO.GetDataA: byte;
begin
  Result := 0;
end;

function TSIO.GetDataB: byte;
begin
  Result := 0;
end;

procedure TSIO.SetControlA(_b: byte);
begin
  FControlA := _b;
end;

procedure TSIO.SetControlB(_b: byte);
begin
  FControlB := _b;
end;

procedure TSIO.SetDataA(_b: byte);
begin
  FDataA := _b;
  if ((FControlA and $07) = 0) and Assigned(FOnTransmitA) then
    FOnTransmitA(_b);
  FControlA := FControlA and ($07 xor $FF);
end;

procedure TSIO.SetDataB(_b: byte);
begin
  FDataB := _b;
  if ((FControlB and $07) = 0) and Assigned(FOnTransmitB) then
    FOnTransmitB(_b);
  FControlB := FControlB and ($07 xor $FF);
end;

procedure TSIO.SetReceivedA(_b: byte);
begin
  FReceivedA := _b;
  if Assigned(FOnInterrupt) then
    FOnInterrupt($6E);
end;

procedure TSIO.SetReceivedB(_b: byte);
begin
  FReceivedB := _b;
  if Assigned(FOnInterrupt) then
    FOnInterrupt($6E);
end;

initialization
  SIO := TSIO.Create;

finalization
  FreeAndNil(SIO);

end.

