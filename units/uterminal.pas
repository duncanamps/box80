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

unit uterminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, ucircular,
  graphics, DOM;

const
  MAX_TERMINAL_COLS = 132;
  MAX_TERMINAL_ROWS = 50;
  MAX_TERM_BUF = 2048;

type
  TAOB = array of byte;
  PByte = ^byte;

  TTerminal = class(TCustomControl)
    protected
      FBuffer:     TCircularBuffer;
      FCharHeight: integer;
      FCharWidth:  integer;
      FCols:       integer;        // Number of columns
      FCursorCol:  integer;        // Cursor column position
      FCursorRow:  integer;        // Cursor row position
      FScreen:     TAOB;           // The screen memory
      FCursorLit:   boolean;
      FCursorXAddr: PByte;
      FCursorYAddr: PByte;
      FScreenAddr:  PByte;
      FSavedScreen: TAOB;
      FSavedX:      Byte;
      FSavedY:      Byte;
      FMargin:     integer;        // Margin in pixels around the screen edge
      FRows:       integer;        // Number of rows
      FLogStream:  TFileStream;
      function  ColToX(_col: integer): integer;
      procedure Paint; override;
      function  RowToY(_row: integer): integer;
    public
      constructor Create(AOwner: TComponent; _cols: integer; _rows: integer); reintroduce;
      destructor Destroy; override;
      procedure Init;
      procedure ProcessChars;
      procedure RefreshScreen;
      function  ScreenChanged: boolean;
      procedure CmdBS;        // Character #8
      procedure CmdHT;        // Character #9
      procedure CmdLF;        // Character #10
      procedure CmdFF;        // Character #12
      procedure CmdCR;        // Character #13
      procedure CmdScrollUp;  // Scroll the screen up by one line
      function  PercentFull: integer;
      procedure ReadFromXml(doc: TXmlDocument);
      function  ScreenCapacity: integer;
      procedure WriteChar(_ch: char);
      procedure WriteChar2(_ch: char);
      procedure WriteString(_s: string);
      procedure WriteToXml(doc: TXmlDocument);
      property  CursorCol: integer   read FCursorCol  write FCursorCol;
      property  CursorRow: integer   read FCursorRow  write FCursorRow;
      property  CursorLit:   boolean read FCursorLit   write FCursorLit;
      property  Margin:    integer   read FMargin;
      property  Screen:    TAOB      read FScreen     write FScreen;
      property  Cols:      integer   read FCols       write FCols;
      property  Rows:      integer   read FRows       write FRows;
  end;

implementation

uses
  uxml;

constructor TTerminal.Create(AOwner: TComponent; _cols: integer; _rows: integer);
begin
  inherited Create(AOwner);
  FBuffer := TCircularBuffer.Create(MAX_TERM_BUF);
  FCols := _cols;
  FRows := _rows;
  FMargin := 6;
  if (FCols < 1) or (FCols > MAX_TERMINAL_COLS) then
    raise Exception.Create('Illegal number of columns when creating terminal');
  if (FRows < 1) or (FRows > MAX_TERMINAL_ROWS) then
    raise Exception.Create('Illegal number of rows when creating terminal');
  SetLength(FScreen,FRows*FCols);
  FLogStream := TFileStream.Create('C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\test_files\validation\terminal.log',fmCreate);
  Init;
end;

destructor TTerminal.Destroy;
begin
  FreeAndNil(FLogStream);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TTerminal.CmdBS;
begin
  if (FCursorRow > 0) or (FCursorCol > 0) then
    begin
      Dec(FCursorCol);
      if FCursorCol < 0 then
        begin
          FCursorCol := FCols-1;
          Dec(FCursorRow);
        end;
    end;
end;

procedure TTerminal.CmdHT;
begin
  repeat
    WriteChar(' ');
  until (FCursorCol mod 8) = 0;
end;

procedure TTerminal.CmdLF;
begin
  Inc(FCursorRow);
  while FCursorRow >= FRows do
    begin
      CmdScrollUp;
      Dec(FCursorRow);
    end;
end;

procedure TTerminal.CmdFF;
var i: integer;
begin
  for i := 0 to FCols*FRows-1 do
    FScreen[i] := Ord(' ');
  FCursorCol := 0;
  FCursorRow := 0;
  Invalidate;
end;

procedure TTerminal.CmdCR;
begin
  FCursorCol := 0;
end;

procedure TTerminal.CmdScrollUp;
var i,j: integer;
begin
  // Move lines
  for i := 1 to FRows-1 do
    for j := 0 to FCols-1 do
      FScreen[(i-1)*FCols+j] := FScreen[i*FCols+j];
  // Blank last line
  for j := 0 to FCols-1 do
    FScreen[(FRows-1)*FCols+j] := Ord(' ');
end;

function TTerminal.ColToX(_col: integer): integer;
begin
  ColToX := FCharWidth * _col + FMargin;
end;

procedure TTerminal.Init;
var i: integer;
    b: byte;
begin
  FCursorCol := 0;
  FCursorRow := 0;
  for i := 0 to Length(FScreen)-1 do
    begin
      b := Random(95)+32;
      FScreen[i] := b;
    end;
end;

procedure TTerminal.Paint;
var i,j: integer;
    s: string;
    reversal: TColor;
begin
  // Set up some variable we need
  FCharHeight := Canvas.TextHeight('Wg');
  FCharWidth := Canvas.TextWidth('W');
  s := '';
  SetLength(s,FCols);
  // Do background
  Canvas.Brush.Color := Color;
  Canvas.FillRect(GetClientRect);
  // Now do all of the text
  for i := 0 to FRows-1 do
    begin
      for j := 0 to FCols-1 do
        s[j+1] := Chr(FScreen[i*FCols+j]);
      Canvas.TextOut(ColToX(0),RowToY(i),s);
    end;
  // Reverse the cursor
  if FCursorLit then
    begin
      reversal := Color xor $00FFFFFF;
      Canvas.CopyMode := cmSrcInvert;
      try
        Canvas.Brush.Color := reversal;
        Canvas.FillRect(ColToX(FCursorCol),
                        RowToY(FCursorRow),
                        ColToX(FCursorCol+1)-1,
                        RowToY(FCursorRow+1)-1
                        );
      finally
        Canvas.CopyMode := cmSrcCopy;
      end;
    end;
  // Finally...
  inherited Paint;
end;

function TTerminal.PercentFull: integer;
var b: byte;
begin
  FBuffer.DoCmd(CB_CMD_PCTFULL,b);
  Result := b;
end;

procedure TTerminal.ProcessChars;
var got_one: boolean;
    b:       byte;
begin
  repeat
    got_one := FBuffer.DoCmd(CB_CMD_READ,b);
    if got_one then
      WriteChar2(Chr(b));
  until not got_one;
end;

procedure TTerminal.ReadFromXml(doc: TXmlDocument);
var node: TDOMnode;
    p:    PByte;
    i,j:  integer;
    s:    string;
begin
  node := doc.DocumentElement.FindNode('terminal');
  CursorCol := GetXmlByte(node,'cursor_col');
  CursorRow := GetXmlByte(node,'cursor_row');
  p := @Screen[0];
  for i := 0 to 24 do
    begin
      s := GetXmlString(node,'terminal_' + IntToHex(i,2));
      for j := 0 to 79 do
        begin
          p^ := StrToInt('$' + Copy(s,1+j*2,2));
          Inc(p);
        end;
    end;
end;

procedure TTerminal.RefreshScreen;
begin
  if ScreenChanged then
    Invalidate;
end;

function TTerminal.RowToY(_row: integer): integer;
begin
  RowToY := FCharHeight * _row + FMargin;
end;

function TTerminal.ScreenCapacity: integer;
var b: byte;
begin
  if not FBuffer.DoCmd(CB_CMD_CAPACITY,b) then
    raise Exception.Create('Failed to read terminal buffer capacity');
  Result := b;
end;

function TTerminal.ScreenChanged: boolean;
var i: integer;
    p: PByte;
begin
  Result := (FCursorXAddr^ <> FSavedX) or (FCursorYAddr^ <> FSavedY);
  p := FScreenAddr;
  i := 0;
  if not Result then
    while i < 80*25 do
      if p^ <> FSavedScreen[i] then
        begin
          Result := True;
          break;
        end
      else
        begin
          Inc(i);
          Inc(p);
        end;
end;

procedure TTerminal.WriteChar(_ch: char);
var b: byte;
begin
  b := Ord(_ch);
  if not FBuffer.DoCmd(CB_CMD_WRITE,b) then
    raise Exception.Create('Failed to write to terminal buffer');
end;

procedure TTerminal.WriteChar2(_ch: char);
begin
  FLogStream.WriteByte(Ord(_ch));
  case _ch of
    #8:  CmdBS;
    #9:  CmdHT;
    #10: CmdLF;
    #12: CmdFF;
    #13: CmdCR;
    otherwise
      if _ch >= ' ' then
        begin
          FScreen[FCursorRow*FCols+FCursorCol] := Ord(_ch);
          Inc(FCursorCol);
          if (FCursorCol >= FCols) then
            begin
              CmdCR;
              CmdLF;
            end;
        end;
  end;
  Invalidate;
end;

procedure TTerminal.WriteString(_s: string);
var i: integer;
begin
  for i := 1 to Length(_s) do
    WriteChar(_s[i]);
end;

procedure TTerminal.WriteToXml(doc: TXmlDocument);
var node: TDOMnode;
    node_line: TDOMnode;
    node_text: TDOMnode;
    p:    PByte;
    i,j:  integer;
    s:    string;
begin
  node := doc.CreateElement('terminal');
  PutXmlByte(node,'cursor_col',CursorCol);
  PutXmlByte(node,'cursor_row',CursorRow);
  p := @Screen[0];
  for i := 0 to 24 do
    begin
      s := '';
      node_line := doc.CreateElement('terminal_' + IntToHex(i,2){%H-});
      s := GetXmlString(node,'terminal_' + IntToHex(i,2));
      for j := 0 to 79 do
        begin
          s := s + IntToHex(p^,2);
          Inc(p);
        end;
      node_text := doc.CreateTextNode(s{%H-});
      node_line.AppendChild(node_text);
      node.AppendChild(node_line);
    end;
  doc.ChildNodes[0].AppendChild(node);
end;

end.

