unit uterminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls,
{$IFDEF USE_SCREEN_MEM}
  bgrabitmappack,
  BGRABitmapTypes, BGRABitmap,
{$ENDIF}
  graphics;

const
  MAX_TERMINAL_COLS = 132;
  MAX_TERMINAL_ROWS = 50;

type
  TAOB = array of byte;
  PByte = ^byte;

  TTerminal = class(TCustomControl)
    protected
      FCharHeight: integer;
      FCharWidth:  integer;
      FCols:       integer;        // Number of columns
{$IFDEF USE_SCREEN_MEM}
      FBitmap:     TBitmap;
      FBitmapRev:  TBitmap;
      FCursorLit:   boolean;
      FCursorXAddr: PByte;
      FCursorYAddr: PByte;
      FScreenAddr:  PByte;
      FSavedScreen: TAOB;
      FSavedX:      Byte;
      FSavedY:      Byte;
{$ELSE}
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
{$ENDIF}
      FMargin:     integer;        // Margin in pixels around the screen edge
      FRows:       integer;        // Number of rows
      function  ColToX(_col: integer): integer;
      procedure Paint; override;
      function  RowToY(_row: integer): integer;
    public
      constructor Create(AOwner: TComponent; _cols: integer; _rows: integer); reintroduce;
      destructor Destroy; override;
      procedure RefreshScreen;
      function  ScreenChanged: boolean;
{$IFDEF USE_SCREEN_MEM}
      property CursorLit:   boolean read FCursorLit   write FCursorLit;
      property CursorXAddr: PByte   read FCursorXAddr write FCursorXAddr;
      property CursorYAddr: PByte   read FCursorYAddr write FCursorYAddr;
      property ScreenAddr:  PByte   read FScreenAddr  write FScreenAddr;
{$ELSE}
      procedure CmdBS;        // Character #8
      procedure CmdHT;        // Character #9
      procedure CmdLF;        // Character #10
      procedure CmdFF;        // Character #12
      procedure CmdCR;        // Character #13
      procedure CmdScrollUp;  // Scroll the screen up by one line
      procedure WriteChar(_ch: char);
      procedure WriteString(_s: string);
      property  CursorCol: integer   read FCursorCol  write FCursorCol;
      property  CursorRow: integer   read FCursorRow  write FCursorRow;
      property  CursorLit:   boolean read FCursorLit   write FCursorLit;
      property  Screen:    TAOB      read FScreen     write FScreen;
{$ENDIF}
      property  Cols:      integer   read FCols       write FCols;
      property  Rows:      integer   read FRows       write FRows;
  end;

implementation

constructor TTerminal.Create(AOwner: TComponent; _cols: integer; _rows: integer);
var i: integer;
    b: byte;
begin
  inherited Create(AOwner);
{$IFDEF USE_SCREEN_MEM}
  FBitmap := TBitmap.Create;
  FBitmap.LoadFromResourceName(HInstance,'CHARGEN_X2');
  FBitmapRev := TBitmap.Create;
  FBitmapRev.LoadFromResourceName(HInstance,'CHARGEN_X2_REV');
  SetLength(FSavedScreen,_cols * _rows);
  FSavedX := $FF;
  FSavedY := $FF;
{$ELSE}
  FCursorCol := 0;
  FCursorRow := 0;
{$ENDIF}
  FCols := _cols;
  FRows := _rows;
  FMargin := 6;
  if (FCols < 1) or (FCols > MAX_TERMINAL_COLS) then
    raise Exception.Create('Illegal number of columns when creating terminal');
  if (FRows < 1) or (FRows > MAX_TERMINAL_ROWS) then
    raise Exception.Create('Illegal number of rows when creating terminal');
{$IFNDEF USE_SCREEN_MEM}
  SetLength(FScreen,FRows*FCols);
  for i := 0 to Length(FScreen)-1 do
    begin
      b := Random(95)+32;
      FScreen[i] := b;
    end;
{$ENDIF}
end;

destructor TTerminal.Destroy;
begin
{$IFDEF USE_SCREEN_MEM}
  FBitmapRev.Free;
  FBitmap.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFNDEF USE_SCREEN_MEM}
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
{$ENDIF}

function TTerminal.ColToX(_col: integer): integer;
begin
  ColToX := FCharWidth * _col + FMargin;
end;

{$IFDEF USE_SCREEN_MEM}
procedure TTerminal.Paint;
var i,j: integer;
    x,y: integer;
    s: string;
    reversal: TColor;
    b:  TBitmap;
    bg: TBGRABitmap;
    stretched: TBGRABitmap;
    p: PByte;
    idx: integer;
    srcrect, dstrect: TRect;
    localscreen: TAOB;
begin
  SetLength(localscreen,80*25);
  b := TBitmap.Create;
  b.Width := 80*16;
  b.Height := 25*16;
  // Grab screen first
  p := FScreenAddr;
  for i := 0 to 80*25-1 do
    begin
      localscreen[i] := p^;
      FSavedScreen[i] := p^;  // And save
      Inc(p);
    end;
  FSavedX := FCursorXAddr^;
  FSavedY := FCursorYAddr^;
  bg := TBGRABitmap.Create(80*16,25*16);
  try
    p := FScreenAddr;
    for y := 0 to 24 do
      begin
        for x := 0 to 79 do
          begin
            idx := p^;
            srcrect.Top  := (idx div 16) * 16;
            srcrect.Left := (idx mod 16) * 16;
            dstrect.Top := y * 16;
            dstrect.Left := x * 16;
            srcrect.Width := 16;
            dstrect.Width := 16;
            srcrect.Height := 16;
            dstrect.Height := 16;
            if (FCursorLit) and
               (FCursorXAddr^ = x) and
               (FCursorYAddr^ = y) then
              b.Canvas.CopyRect(dstrect,FBitmapRev.Canvas,srcrect)
            else
              b.Canvas.CopyRect(dstrect,FBitmap.Canvas,srcrect);
            Inc(p);
          end;
      end;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0,0,Width,Height));
    bg.Canvas.CopyRect(Rect(0,0,80*16,25*16),b.Canvas,Rect(0,0,80*16,25*16));
    stretched := bg.Resample(Width-2*FMargin,Height-2*FMargin) as TBGRABitmap;
    stretched.Draw(Canvas,FMargin,FMargin);
    stretched.Free;
  finally
    bg.Free;
    b.Free;
  end;
  // Finally...
  inherited Paint;
end;
{$ELSE}
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
      reversal := Color xor $0000FF00;
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
{$ENDIF}

procedure TTerminal.RefreshScreen;
begin
  if ScreenChanged then
    Invalidate;
end;

function TTerminal.RowToY(_row: integer): integer;
begin
  RowToY := FCharHeight * _row + FMargin;
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

{$IFNDEF USE_SCREEN_MEM}
procedure TTerminal.WriteChar(_ch: char);
begin
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
end;

procedure TTerminal.WriteString(_s: string);
var i: integer;
begin
  for i := 1 to Length(_s) do
    WriteChar(_s[i]);
end;
{$ENDIF}


end.

