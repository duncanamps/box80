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
  MAX_TERM_BUF = 16384;

  SGR_ATTR_BOLD      = $8000;
  SGR_ATTR_ITALIC    = $4000;
  SGR_ATTR_UNDERLINE = $2000;
  SGR_ATTR_STRIKEOUT = $1000;

type
  TAOB = array of byte;
  TAOW = array of word;

  TCSIstate = (csiNone,csiEsc,csiBracket,csiParameter,csiIntermediate,csiFinal);

  PByte = ^byte;

  // Attributes are stored in a word
  //
  //  Bit 15 Bold
  //      14 Italic
  //      13 Underline
  //      12 Strikeout
  //      --------------------------------
  //      11 RED bit foreground MSB
  //      10 RED bit foreground LSB
  //       9 GREEN bit foreground MSB
  //       8 GREEN bit foreground LSB
  //       7 BLUE bit foreground MSB
  //       6 BLUE bit foreground LSB
  //      --------------------------------
  //       5 RED bit background MSB
  //       4 RED bit background LSB
  //       3 GREEN bit background MSB
  //       2 GREEN bit background LSB
  //       1 BLUE bit background MSB
  //       0 BLUE bit background LSB

  TTerminal = class(TCustomControl)
    protected
      FAttrib:           Word;           // The current attribute
      FAttribs:          TAOW;           // The attribute memory
      FAttribBold:       boolean;
      FAttribUnderlined: boolean;
      FBuffer:           TCircularBuffer;
      FCharHeight:       integer;
      FCharWidth:        integer;
      FCSIstate:         TCSIstate;      // State of the CSI control functions
      FCSIparameter:     string;
      FCSIintermediate:  string;
      FCSIfinal:         string;
      FCSIparams:        TStringList;
      FCols:             integer;        // Number of columns
      FCursorCol:        integer;        // Cursor column position
      FCursorRow:        integer;        // Cursor row position
      FScreen:           TAOB;           // The screen memory
      FCursorLit:        boolean;
      FCursorXAddr:      PByte;
      FCursorYAddr:      PByte;
      FScreenAddr:       PByte;
      FSavedScreen:      TAOB;
      FSavedX:           Byte;
      FSavedY:           Byte;
      FMargin:           integer;        // Margin in pixels around the screen edge
      FRows:             integer;        // Number of rows
      FLogStream:        TFileStream;
      function  AttribScale(_src: Word; _boost: boolean): DWord;
      function  AttribBackgroundAsTColor(_attr: Word): TColor;
      function  AttribForegroundAsTColor(_attr: Word): TColor;
      function  AttribAsFontStyle(_attr: Word): TFontStyles;
      procedure ClearRegion(_row1, _col1, _row2, _col2: integer);
      function  ColToX(_col: integer): integer;
      function  CSIparam(_index: integer): integer;
      function  CSIparamS(_index: integer): string;
      procedure CursorBack(_amt: integer);
      procedure CursorDown(_amt: integer);
      procedure CursorForward(_amt: integer);
      procedure CursorHorizAbsolute(_amt: integer);
      procedure CursorNextLine(_amt: integer);
      procedure CursorPosition(_row, _col: integer);
      procedure CursorPreviousLine(_amt: integer);
      procedure CursorUp(_amt: integer);
      procedure DeleteLine(_amt: integer);
      procedure EraseInDisplay(_amt: integer);
      procedure EraseInLine(_amt: integer);
      procedure InitCSI;
      procedure InsertLine(_amt: integer);
      procedure Paint; override;
      procedure ProcessCSI;
      procedure ParseCSIparams;
      function  RowToY(_row: integer): integer;
      procedure ScrollDown(_amt: integer);
      procedure ScrollRegionDown(_first,_last,_amt: integer);
      procedure ScrollRegionUp(_first,_last,_amt: integer);
      procedure ScrollUp(_amt: integer);
      procedure SelectGraphicRendition(_parm: integer);
      procedure SGRbold;
      procedure SGRresetattr;
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
      procedure CmdESC;       // Character #27
      procedure CmdCSI(_ch: char); // Handle CSI sequences
      function  PercentFull: double;
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
  SetLength(FAttribs,FRows*FCols);
  FLogStream := TFileStream.Create('C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\test_files\validation\terminal.log',fmCreate);
  FCSIparams := TStringList.Create;
  FCSIparams.Delimiter := ';';
  Init;
end;

destructor TTerminal.Destroy;
begin
  FreeAndNil(FCSIparams);
  FreeAndNil(FLogStream);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TTerminal.AttribAsFontStyle(_attr: Word): TFontStyles;
begin
  Result := [];
  if (_attr and SGR_ATTR_BOLD) <> 0 then
    Result := Result + [fsBold];
  if (_attr and SGR_ATTR_ITALIC) <> 0 then
    Result := Result + [fsItalic];
  if (_attr and SGR_ATTR_UNDERLINE) <> 0 then
    Result := Result + [fsUnderline];
  if (_attr and SGR_ATTR_STRIKEOUT) <> 0 then
    Result := Result + [fsStrikeOut];
end;

function TTerminal.AttribBackgroundAsTColor(_attr: Word): TColor;
var inr, ing, inb: Word;
    otr, otg, otb: DWord;
begin
  inr := (_attr and $0030) shr 4;
  ing := (_attr and $000C) shr 2;
  inb := _attr and $0003;
  otr := AttribScale(inr,False);
  otg := AttribScale(ing,False);
  otb := AttribScale(inb,False);
  Result := (otr shl 16) or (otg shl 8) or (otb);
end;

function TTerminal.AttribForegroundAsTColor(_attr: Word): TColor;
var inr, ing, inb: Word;
    otr, otg, otb: DWord;
    boost: boolean;
begin
  inr := (_attr and $0c00) shr 10;
  ing := (_attr and $0300) shr 8;
  inb := (_attr and $00C0) shr 6;
  boost := (_attr and SGR_ATTR_BOLD) <> 0;
  otr := AttribScale(inr,boost);
  otg := AttribScale(ing,boost);
  otb := AttribScale(inb,boost);
  Result := (otr shl 16) or (otg shl 8) or (otb);
end;

function TTerminal.AttribScale(_src: Word; _boost: boolean): DWord;
begin
  Result := 0;
  if _boost then
    case _src of
       0: Result := 0;
       1: Result := $B0;
       2: Result := $F0;
       3: Result := $FF;
    end
  else // Unboosted
    case _src of
       0: Result := 0;
       1: Result := $80;
       2: Result := $C0;
       3: Result := $FF;
    end;
end;

procedure TTerminal.ClearRegion(_row1, _col1, _row2, _col2: integer);
var addr1, addr2, i: integer;
begin
  addr1 := _row1 * FCols + _col1;
  addr2 := _row2 * FCols + _col2;
  for i := addr1 to addr2 do
    FScreen[i] := $20;
  for i := addr1 to addr2 do
    FAttribs[i] := FAttrib;
end;

procedure TTerminal.CmdBS;
begin
  CursorBack(1);
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
      ScrollUp(1);
      Dec(FCursorRow);
    end;
end;

procedure TTerminal.CmdFF;
begin
  EraseInDisplay(2);
end;

procedure TTerminal.CmdCR;
begin
  FCursorCol := 0;
end;

procedure TTerminal.CmdESC;
begin
  InitCSI;
  FCSIstate := csiESC;
end;

procedure TTerminal.CmdCSI(_ch: char);
begin
  case FCSIstate of
    csiESC:
      if _ch = '[' then
        FCSIstate := csiBracket
      else
        FCSIstate := csiNone; // Invalid sequence, abandon
    csiBracket:
      if _ch in ['0'..'?'] then
        FCSIparameter := FCSIparameter + _ch
      else if _ch in [' '..'/'] then
        begin
          FCSIintermediate := FCSIintermediate + _ch;
          FCSIstate := csiIntermediate;
        end
      else if _ch in ['@'..'~'] then
        begin
          FCSIfinal := FCSIfinal + _ch;
          FCSIstate := csiFinal;
        end
      else
        FCSIstate := csiNone; // Invalid sequence, abandon
    csiIntermediate:
      if _ch in [' '..'/'] then
        FCSIintermediate := FCSIintermediate + _ch
      else if _ch in ['@'..'~'] then
        begin
          FCSIfinal := FCSIfinal + _ch;
          FCSIstate := csiFinal;
        end
      else
        FCSIstate := csiNone; // Invalid sequence, abandon
  end;
  if FCSIstate = csiFinal then
    begin
      ProcessCSI;
      FCSIstate := csiNone;
    end;
end;

function TTerminal.ColToX(_col: integer): integer;
begin
  ColToX := FCharWidth * _col + FMargin;
end;

function TTerminal.CSIparam(_index: integer): integer;
var s: string;
begin
  s := CSIparamS(_index);
  if s = '' then
    s := '-1';
  Result := -1;
  try
    Result := StrToInt(s);
  except
    ; // Silent exception, send back -1 due to invalid number etc.
  end;
end;

function TTerminal.CSIparamS(_index: integer): string;
begin
  if _index >= FCSIparams.Count then
    Result := ''
  else
    Result := FCSIparams[_index];
end;

procedure TTerminal.CursorBack(_amt: integer);
begin
  if _amt <= 0 then
    _amt := 1;
  Dec(FCursorCol,_amt);
  while FCursorCol < 0 do
    begin
      if FCursorRow = 0 then
        FCursorCol := 0
      else
        begin
          FCursorCol := FCursorCol + FCols;
          Dec(FCursorRow);
        end;
    end;
end;

procedure TTerminal.CursorDown(_amt: integer);
begin
  if _amt <= 0 then
    _amt := 1;
  Inc(FCursorRow,_amt);
  if FCursorRow >= FRows then
    FCursorRow := FRows-1;
end;

procedure TTerminal.CursorForward(_amt: integer);
begin
  if _amt <= 0 then
    _amt := 1;
  Inc(FCursorCol,_amt);
  while FCursorCol >= FCols do
    begin
      if FCursorRow = (FRows-1) then
        FCursorCol := FCols-1
      else
        begin
          FCursorCol := FCursorCol - FCols;
          Inc(FCursorRow);
        end;
    end;
end;

procedure TTerminal.CursorHorizAbsolute(_amt: integer);
var newpos: integer;
begin
  newpos := _amt;
  if newpos <= 0 then
    newpos := 1;
  if newpos > FCols then
    newpos := FCols;
  FCursorCol := newpos - 1;
end;

procedure TTerminal.CursorNextLine(_amt: integer);
begin
  CursorDown(_amt);
  FCursorCol := 0;
end;

procedure TTerminal.CursorPosition(_row, _col: integer);
begin
  if _row < 1 then
    _row := 1;
  if _row > FRows then
    _row := FRows;
  if _col < 1 then
    _col := 1;
  if _col > FCols then
    _col := FCols;
  FCursorRow := _row - 1;
  FCursorCol := _col - 1;
end;

procedure TTerminal.CursorPreviousLine(_amt: integer);
begin
  CursorUp(_amt);
  FCursorCol := 0;
end;

procedure TTerminal.CursorUp(_amt: integer);
begin
  if _amt <= 0 then
    _amt := 1;
  Dec(FCursorRow,_amt);
  if FCursorRow < 0 then
    FCursorRow := 0;
end;

procedure TTerminal.DeleteLine(_amt: integer);
begin
  if _amt < 1 then
    _amt := 1;
  ScrollRegionUp(FCursorRow,FRows-1,_amt);
end;

procedure TTerminal.EraseInDisplay(_amt: integer);
begin
  case _amt of
    -1,0:
      ClearRegion(FCursorRow,FCursorCol,FRows-1,FCols-1);
    1:
      ClearRegion(FCursorRow,FCursorCol,FRows-1,FCols-1);
    2,3:
      begin
        ClearRegion(0,0,FRows-1,FCols-1);
        FCursorCol := 0;
        FCursorRow := 0;
      end;
  end;
  Invalidate;
end;

procedure TTerminal.EraseInLine(_amt: integer);
begin
  case _amt of
     -1,0:
       ClearRegion(FCursorRow,FCursorCol,FCursorRow,FCols-1);
     1:
       ClearRegion(FCursorRow,0,FCursorRow,FCursorCol);
     2:
       ClearRegion(FCursorRow,0,FCursorRow,FCols-1);
  end;
end;

procedure TTerminal.Init;
begin
  InitCSI;
  SGRresetattr;
  CmdFF;
  WriteString('box80: Z80 Virtual Machine and ANSI terminal' + #13 + #10);
  WriteString('Copyright (C)2023 Duncan Munro' + #13 + #10);
  WriteString('Licensed with GNU GPL V3 or later' + #13 + #10);
end;

procedure TTerminal.InitCSI;
begin
  FCSIstate := csiNone;
  FCSIparameter    := '';
  FCSIintermediate := '';
  FCSIfinal        := '';
end;

procedure TTerminal.InsertLine(_amt: integer);
begin
  if _amt < 1 then
    _amt := 1;
  ScrollRegionDown(FCursorRow,FRows-1,_amt);
end;

procedure TTerminal.Paint;
var i,j: integer;
    s: string;
    reversal: TColor;
    r: TRect;
    _attrib: Word;
begin
  // Set up some variable we need
  Canvas.Font.Style := [fsBold];
  FCharHeight := Canvas.TextHeight('Wg');
  FCharWidth := Canvas.TextWidth('W') + 2;
  s := ' ';
  // Now do all of the text
  for i := 0 to FRows-1 do
    begin
      r.Top := RowToY(i);
      r.Bottom := RowToY(i+1);
      for j := 0 to FCols-1 do
        begin
          // Do background
          _attrib := FAttribs[i*FCols+j];
          r.Left := ColToX(j);
          r.Right := ColToX(j+1);
          Canvas.Brush.Color := AttribBackgroundAsTColor(_attrib);
          Canvas.FillRect(r);
          s := Chr(FScreen[i*FCols+j]);
          Canvas.Font.Color := AttribForegroundAsTColor(_attrib);
          Canvas.Font.Style := AttribAsFontStyle(_attrib);
          // Slight left shift on character to fit the box better
          Canvas.TextOut(ColToX(j)-2,RowToY(i),s);
        end;
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

procedure TTerminal.ParseCSIparams;
begin
  FCSIparams.Clear;
  FCSIparams.DelimitedText := FCSIparameter;
end;

function TTerminal.PercentFull: double;
begin
  Result := FBuffer.PercentUsed;
end;

procedure TTerminal.ProcessChars;
var got_one: boolean;
    b:       integer;
begin
  repeat
    got_one := FBuffer.DoCmd(CB_CMD_READ,b{%H-});
    if got_one then
      WriteChar2(Chr(b));
  until not got_one;
end;

procedure TTerminal.ProcessCSI;
var final: char;
begin
  ParseCSIparams;
  final := #0;
  if FCSIfinal <> '' then
    final := FCSIfinal[1];
  case final of
    'A': CursorUp(CSIparam(0));
    'B': CursorDown(CSIparam(0));
    'C': CursorForward(CSIparam(0));
    'D': CursorBack(CSIparam(0));
    'E': CursorNextLine(CSIparam(0));
    'F': CursorPreviousLine(CSIparam(0));
    'G': CursorHorizAbsolute(CSIparam(0));
    'H': CursorPosition(CSIparam(0),CSIparam(1));
    'J': EraseInDisplay(CSIparam(0));
    'K': EraseInLine(CSIparam(0));
    'L': InsertLine(CSIparam(0));
    'M': DeleteLine(CSIparam(0));
    'S': ScrollUp(CSIparam(0));
    'T': ScrollDown(CSIparam(0));
    'f': CursorPosition(CSIparam(0),CSIparam(1));
    'm': SelectGraphicRendition(CSIparam(0));
  end;
end;

procedure TTerminal.ReadFromXml(doc: TXmlDocument);
var node: TDOMnode;
    p:    PByte;
    i,j:  integer;
    s:    string;
    w:    PWord;
begin
  node := doc.DocumentElement.FindNode('terminal');
  CursorCol := GetXmlByte(node,'cursor_col');
  CursorRow := GetXmlByte(node,'cursor_row');
  // Read the screen data
  p := @Screen[0];
  for i := 0 to FRows-1 do
    begin
      s := GetXmlString(node,'screen_' + IntToHex(i,2));
      for j := 0 to FCols-1 do
        begin
          p^ := StrToInt('$' + Copy(s,1+j*2,2));
          Inc(p);
        end;
    end;
  // Read the attributes
  w := @FAttribs[0];
  for i := 0 to FRows-1 do
    begin
      s := GetXmlString(node,'attributes_' + IntToHex(i,2));
      for j := 0 to FCols-1 do
        begin
          w^ := StrToInt('$' + Copy(s,1+j*4,4));
          Inc(w);
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

procedure TTerminal.ScrollDown(_amt: integer);
begin
  ScrollRegionDown(0,FRows-1,_amt);
end;

procedure TTerminal.ScrollRegionDown(_first,_last,_amt: integer);
var i,j: integer;
begin
  if _amt < 1 then
    _amt := 1;
  if _amt >= (_last - _first + 1) then
    _amt := _last - _first;
  // Move lines
  for i := _last downto _first+_amt do
    for j := 0 to FCols-1 do
      begin
        FScreen[i*FCols+j] := FScreen[(i-_amt)*FCols+j];
        FAttribs[i*FCols+j] := FAttribs[(i-_amt)*FCols+j];
      end;
  // Blank vacated lines
  for i := _first to _first+_amt-1 do
    for j := 0 to FCols-1 do
      begin
        FScreen[i*FCols+j] := Ord(' ');
        FAttribs[i*FCols+j] := FAttrib;
      end;
end;

procedure TTerminal.ScrollRegionUp(_first,_last,_amt: integer);
var i,j: integer;
begin
  if _amt < 1 then
    _amt := 1;
  if _amt >= (_last - _first + 1) then
    _amt := _last - _first;
  // Move lines
  for i := _amt+_first to _last do
    for j := 0 to FCols-1 do
      begin
        FScreen[(i-_amt)*FCols+j] := FScreen[i*FCols+j];
        FAttribs[(i-_amt)*FCols+j] := FAttribs[i*FCols+j];
      end;
  // Blank vacated lines
  for i := _last-_amt+1 to _last do
    for j := 0 to FCols-1 do
      begin
        FScreen[i*FCols+j] := Ord(' ');
        FAttribs[i*FCols+j] := FAttrib;
      end;
end;

procedure TTerminal.ScrollUp(_amt: integer);
begin
  ScrollRegionUp(0,FRows-1,_amt);
end;

procedure TTerminal.SelectGraphicRendition(_parm: integer);
begin
  case _parm of
    0: SGRresetattr;
    1: SGRbold;
    2: ; // SGRfaint;
    3: ; // SGRitalic;
    4: ; // SGRunderline;
    5: ; // SGRslowblink;
    6: ; // SGRrapidblink;
    7: ; // SGRreverse;
    8: ; // SGRconceal;
    9: ; // SGRstrikeout;
    10..20: ; // SGRfontselect
    21: ; // SGRdoubleunderline
    22: ; // SGRnormalintensity (not bold or faint)
    23: ; // SGRnormalstroke (not italic etc.)
    24: ; // SGRnotunderlined
    25: ; // SGRnotblinking
    26: ; // SGRproportional
    27: ; // SGRnotreversed
    28: ; // SGRreveal
    29: ; // SGRnotstrikeout
    30..39: ; // SGRsetforegroundcolour
    40..49: ; // SGRsetbackgroundcolour
    50: ; // SGRdisableproportionalspacing
    otherwise
      ; // Ignore
  end;
end;

function TTerminal.ScreenCapacity: integer;
var b: integer;
begin
  if not FBuffer.DoCmd(CB_CMD_CAPACITY,b{%H-}) then
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

procedure TTerminal.SGRbold;
begin
  FAttrib := FAttrib or SGR_ATTR_BOLD;
end;

procedure TTerminal.SGRresetattr;
begin
  FAttrib := $0A80; // No bold, underline, text is white on black
end;

procedure TTerminal.WriteChar(_ch: char);
var b: integer;
begin
  b := Ord(_ch);
  if not FBuffer.DoCmd(CB_CMD_WRITE,b) then
    raise Exception.Create('Failed to write to terminal buffer');
end;

procedure TTerminal.WriteChar2(_ch: char);
begin
  FLogStream.WriteByte(Ord(_ch));
  if FCSIstate <> csiNone then
    CmdCSI(_ch)
  else
    case _ch of
      #8:  CmdBS;
      #9:  CmdHT;
      #10: CmdLF;
      #12: CmdFF;
      #13: CmdCR;
      #27: CmdESC;
      otherwise
        if _ch >= ' ' then
          begin
            FScreen[FCursorRow*FCols+FCursorCol] := Ord(_ch);
            FAttribs[FCursorRow*FCols+FCursorCol] := FAttrib;
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
    w:    PWord;
begin
  node := doc.CreateElement('terminal');
  PutXmlByte(node,'cursor_col',CursorCol);
  PutXmlByte(node,'cursor_row',CursorRow);
  // Output the screen data
  p := @Screen[0];
  for i := 0 to FRows-1 do
    begin
      s := '';
      node_line := doc.CreateElement('screen_' + IntToHex(i,2){%H-});
      s := GetXmlString(node,'screen_' + IntToHex(i,2));
      for j := 0 to FCols-1 do
        begin
          s := s + IntToHex(p^,2);
          Inc(p);
        end;
      node_text := doc.CreateTextNode(s{%H-});
      node_line.AppendChild(node_text);
      node.AppendChild(node_line);
    end;
  // Output the attribute data
  W := @FAttribs[0];
  for i := 0 to FRows-1 do
    begin
      s := '';
      node_line := doc.CreateElement('attributes_' + IntToHex(i,2){%H-});
      s := GetXmlString(node,'attributes_' + IntToHex(i,2));
      for j := 0 to FCols-1 do
        begin
          s := s + IntToHex(w^,4);
          Inc(w);
        end;
      node_text := doc.CreateTextNode(s{%H-});
      node_line.AppendChild(node_text);
      node.AppendChild(node_line);
    end;
  doc.ChildNodes[0].AppendChild(node);
end;

end.

