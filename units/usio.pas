unit usio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSIOTransmitProc = procedure(_b: byte) of object;

  TSIO = class(TObject)
    private
      FControlA:  byte;
      FControlB:  byte;
      FDataA:     byte;
      FDataB:     byte;
      FOnTransmitA: TSIOTransmitProc;
      FOnTransmitB: TSIOTransmitProc;
      function GetDataA: byte;
      function GetDataB: byte;
      procedure SetControlA(_b: byte);
      procedure SetControlB(_b: byte);
      procedure SetDataA(_b: byte);
      procedure SetDataB(_b: byte);
    public
      property ControlA: byte write SetControlA;
      property ControlB: byte write SetControlB;
      property DataA: byte    read GetDataA   write SetDataA;
      property DataB: byte    read GetDataB   write SetDataB;
      property OnTransmitA: TSIOTransmitProc write FOnTransmitA;
      property OnTransmitB: TSIOTransmitProc write FOnTransmitB;
  end;

var
  SIO: TSIO;


implementation

function TSIO.GetDataA: byte;
begin
end;

function TSIO.GetDataB: byte;
begin
end;

procedure TSIO.SetControlA(_b: byte);
begin
end;

procedure TSIO.SetControlB(_b: byte);
begin
end;

procedure TSIO.SetDataA(_b: byte);
begin
  FDataA := _b;
end;

procedure TSIO.SetDataB(_b: byte);
begin
  FDataB := _b;
end;

initialization
  SIO := TSIO.Create;

finalization
  FreeAndNil(SIO);

end.

