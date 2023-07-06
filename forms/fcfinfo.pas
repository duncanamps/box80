unit fcfinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uprocessor;

type

  { TfrmCFinfo }

  TfrmCFinfo = class(TForm)
    btnOK: TButton;
    Memo1: TMemo;
  private

  public
    procedure ShowInfo(_proc: TProcessor);
  end;

var
  frmCFinfo: TfrmCFinfo;

implementation

{$R *.lfm}

procedure TfrmCFinfo.ShowInfo(_proc: TProcessor);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('CF name: ' + _proc.CFlash.Filename);
  if Assigned(_proc.CFlash.Stream) then
    begin
      Memo1.Lines.Add('Size: ' + Format('%.1f',[_proc.CFlash.Stream.Size / 1024.0 / 1024.0]) + ' MB');
    end;
  Self.ShowModal;
end;

end.

