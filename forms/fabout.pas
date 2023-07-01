unit fabout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmHelpAbout }

  TfrmHelpAbout = class(TForm)
    btnOK: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Image1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private

  public

  end;

var
  frmHelpAbout: TfrmHelpAbout;

implementation

{$R *.lfm}

{ TfrmHelpAbout }

procedure TfrmHelpAbout.Image1Click(Sender: TObject);
begin

end;

procedure TfrmHelpAbout.Label2Click(Sender: TObject);
begin

end;

end.

