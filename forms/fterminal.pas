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

unit fterminal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uterminal,
  uprocessor;

type

  { TfrmTerminal }

  TfrmTerminal = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure Timer1Timer(Sender: TObject);
  private
    FProcessor: TProcessor;
    FTerminal: TTerminal;
  public
    procedure Init;
    procedure WriteChar(_ch: char);
    property Processor: TProcessor write FProcessor;
    property Terminal: TTerminal read FTerminal;
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

procedure TfrmTerminal.FormKeyPress(Sender: TObject; var Key: char);
begin
  // Form keypress routine
  // Put the key in the simulator SIO and trigger an interrupt
  if Assigned(FProcessor) then
    FProcessor.ChannelReceiveA(Ord(key));
end;

procedure TfrmTerminal.Init;
begin
  FTerminal.Init;
end;

procedure TfrmTerminal.Timer1Timer(Sender: TObject);
begin
  FTerminal.CursorLit := not FTerminal.CursorLit;
  FTerminal.Invalidate;
end;

procedure TfrmTerminal.WriteChar(_ch: char);
begin
  FTerminal.WriteChar(_ch);
end;

end.

