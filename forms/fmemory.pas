unit fmemory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uprocessor;

type

  { TfrmMemory }

  TfrmMemory = class(TForm)
    Memo1: TMemo;
  private
    FActive: boolean;
    FProcessor: TProcessor;
    FStartAddr: Word;
    procedure SetProcessor(_proc: TProcessor);
    procedure SetStartAddr(_addr: word);
  public
    property Processor: TProcessor read FProcessor write SetProcessor;
    property StartAddr: Word read FStartAddr write SetStartAddr;
    procedure Refresh;
  end;

var
  frmMemory: TfrmMemory;

implementation

{$R *.lfm}

procedure TfrmMemory.Refresh;
var row: integer;
    col: integer;
    s: string;
    ptr: Word;
begin
  if not FActive then
    Exit;
  ptr := FStartAddr;
  for row := 0 to 16 do
    begin
      s := IntToHex(ptr,4) + ':';
      for col := 0 to 15 do
        begin
          s := s + ' ' + IntToHex(FProcessor.RAM[ptr],2);
          {$RANGECHECKS OFF}
          Inc(ptr);
          {$RANGECHECKS ON}
        end;
      if row = Memo1.Lines.Count then
        Memo1.Lines.Add(s)
      else
        Memo1.Lines[row] := s;
    end;
end;

procedure TfrmMemory.SetProcessor(_proc: TProcessor);
begin
  FProcessor := _proc;
  FActive := True;
end;

procedure TfrmMemory.SetStartAddr(_addr: Word);
begin
  FStartAddr := _addr;
  Refresh;
end;


end.

