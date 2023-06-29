unit fterminal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uterminal;

type

  { TfrmTerminal }

  TfrmTerminal = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTerminal: TTerminal;
  public

  end;

var
  frmTerminal: TfrmTerminal;

implementation

{$R *.lfm}

{ TfrmTerminal }

procedure TfrmTerminal.FormCreate(Sender: TObject);
begin
  FTerminal := TTerminal.Create(Self,80,25);
  FTerminal.Parent := Self;
  FTerminal.Top := 0;
  FTerminal.Left := 0;
  FTerminal.Width := 814;
  FTerminal.Height := 490;
//FTerminal.Align := alClient;
  FTerminal.Color := TColor($00002800);
  FTerminal.Font.Color := clLime;
  FTerminal.Font.Name := 'Lucida Sans Typewriter';
  FTerminal.Font.Size := 10;
end;

procedure TfrmTerminal.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTerminal);
end;

end.

