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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, uterminal, uprocessor;

CONST TERM_W = 80;
      TERM_H = 25;

type

  { TfrmTerminal }

  TfrmTerminal = class(TForm)
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure Timer1Timer(Sender: TObject);
  private
    big_number: int64;
    FProcessor: TProcessor;
    FTerminal: TTerminal;
  public
    SIOApct: double;
    SIOBpct: double;
    function  HasScreenCapacity: boolean;
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
  FTerminal := TTerminal.Create(Self,TERM_W,TERM_H);
  FTerminal.Parent := Self;
  FTerminal.Top := 0;
  FTerminal.Left := 0;
  FTerminal.Width := 814;
  FTerminal.Height := 490;
  FTerminal.Align := alClient;
  FTerminal.Color := TColor($00000000);
  FTerminal.Font.Color := clWhite;
  FTerminal.Font.Name := 'Consolas';
  FTerminal.Font.Size := 11;
end;

procedure TfrmTerminal.FormActivate(Sender: TObject);
var req_width, req_height: integer;
begin
  req_width  := FTerminal.Margin * 2 + (FTerminal.Canvas.TextWidth('WM')+2) * TERM_W div 2;
  req_height := FTerminal.Margin * 2 + FTerminal.Canvas.TextHeight('Wg') * TERM_H;
  if FTerminal.Width <> req_width then
    Width := Width + req_width - FTerminal.Width;
  if FTerminal.Height <> req_height then
    Height := Height + req_height - FTerminal.Height;
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

function TfrmTerminal.HasScreenCapacity: boolean;
begin
  Result := FTerminal.ScreenCapacity > 1;
end;

procedure TfrmTerminal.Init;
begin
  FTerminal.Init;
end;

procedure TfrmTerminal.Timer1Timer(Sender: TObject);
begin
  Inc(big_number);
  if big_number and $07 = 0 then
    begin
      FTerminal.CursorLit := not FTerminal.CursorLit;
      StatusBar1.SimpleText := Format('Write buffer = %6.2f%% : SIO A buffer = %6.2f%% : SIO B buffer = %6.2f%%',[FTerminal.PercentFull,SIOApct,SIOBpct]);;
    end;
  FTerminal.ProcessChars;
  FTerminal.Invalidate;
end;

procedure TfrmTerminal.WriteChar(_ch: char);
begin
  FTerminal.WriteChar(_ch);
end;

end.

