unit fbox80;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls,
{$ifdef unix}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
{$endif}
   uprocessor, uterminal;

const
  MONITOR_COM = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\imported\g_searle\source\monitor.bin';


type

  { TfrmBox80 }

  TfrmBox80 = class(TForm)
    btnInit: TButton;
    btnStep: TButton;
    btnRun: TButton;
    btnStop: TButton;
    edtT: TEdit;
    edtuS: TEdit;
    edtA_: TEdit;
    edtA: TEdit;
    edtIR: TEdit;
    edtIX: TEdit;
    edtIY: TEdit;
    edtAtPC: TEdit;
    edtSP: TEdit;
    edtPC: TEdit;
    edtHL_: TEdit;
    edtDE_: TEdit;
    edtBC_: TEdit;
    edtHL: TEdit;
    edtDE: TEdit;
    edtBC: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    labF6: TLabel;
    labF5: TLabel;
    labF4: TLabel;
    labF3: TLabel;
    labF2: TLabel;
    labF1: TLabel;
    labF0: TLabel;
    labF_6: TLabel;
    labF_5: TLabel;
    labF_4: TLabel;
    labF_3: TLabel;
    labF_2: TLabel;
    labF_1: TLabel;
    labF_0: TLabel;
    labF7: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    labF_7: TLabel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure btnInitClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure Timer1Timer(Sender: TObject);
  private
    CancelRequested: boolean;
    FProcessor: TProcessor;
    FTerminal: TTerminal;
    FTimerClicks: integer;
    last: TRegisterSet;
    procedure HandleSIOtransmitA(_b: byte);
    procedure ShowRegisters;
    procedure Status(const _msg: string);
    procedure Status(const _fmt: string; const _args: array of const);
  public

  end;

var
  frmBox80: TfrmBox80;

implementation

{$R *.lfm}


{ TfrmBox80 }

procedure TfrmBox80.btnInitClick(Sender: TObject);
var strm: TFileStream;
begin
  FProcessor.Init;
  // Load Grant Searle monitor ROM to $0000
  strm := TFileStream.Create(MONITOR_COM,fmOpenRead);
  try
    // Attempt to read 32K
    FProcessor.ReadFromStream(strm,0,32768);
  finally
    FreeAndNil(strm);
  end;
  ShowRegisters;
end;

{$DEFINE REAL_SPEED}
procedure TfrmBox80.btnRunClick(Sender: TObject);
{$IFDEF REAL_SPEED}
const FPS = 20;                    // Frames per second for terminal refresh
      INST_BATCH = 100000;
      REFR_CYCLE = (1/FPS) / 86400.0; // Seconds between terminal refresh
      DISP_CYCLE = 0.25 / 86400.0;    // Seconds between register updates
{$ELSE}
const INST_BATCH = 50000000;
      DISP_BATCH = 50000000;
{$ENDIF}
var saved_pc: word;
    good: boolean;
    instructions: int64;
    instcount:    int64;
    start_time: TDateTime;
    disp_last:  TDateTime;
    refr_last:  TDateTime;
    start_t:    int64;  // Starting T states
    t_elapsed:  int64;  // T states since start
    elapsed:    double; // Elapsed time in reality
    exp_time:   double; // Expected time in seconds since start
begin
  btnInit.Enabled := False;
  btnRun.Enabled := False;
  btnStop.Enabled := True;
  CancelRequested := False;
  try
    good := True;
    instructions := 0;
    instcount := 0;
    start_time := Now();
    disp_last := start_time;
    refr_last := start_time;
    start_t := FProcessor.TStates;
    while good do
      begin
        saved_pc := FProcessor.PC;
        good := FProcessor.ExecuteInto;
        Inc(instructions);
        Inc(instcount);
        if instcount > INST_BATCH then
          begin
            // Add 1mS worth of T states
            instcount := 0;
            t_elapsed := FProcessor.TStates - start_t;
            exp_time := t_elapsed / 4.0 / FProcessor.CPUspeed;
{$IFDEF REAL_SPEED}
            repeat
              elapsed := (Now() - start_time) * 86400.0;
              if elapsed < exp_time then
                Sleep(1);
            until elapsed >= exp_time;
{$ELSE}
            elapsed := (Now() - start_time) * 86400.0;
{$ENDIF}
            if (Now() - disp_last) >= DISP_CYCLE then
              begin
                disp_last := disp_last + DISP_CYCLE;
                ShowRegisters;
                if elapsed > 0.0 then
                  Status('MHz = %8.3f, MIPS = %8.3f',[t_elapsed / elapsed / 4.0 / 1000000.0 + 0.0005,instructions / elapsed / 1000000.0]);
              end;
            if (Now() - refr_last) >= REFR_CYCLE then
              begin
                refr_last := refr_last + REFR_CYCLE;
                Application.ProcessMessages;
                if CancelRequested then
                  good := False
              end;
          end;
      end;
    if efIllegal in FProcessor.ErrorFlag then
      MessageDlg('Error','Illegal instruction at ' + IntToHex(saved_pc),mtError,[mbOK],0);
    if efHalt in FProcessor.ErrorFlag then
      MessageDlg('Information','Processor halted at ' + IntToHex(saved_pc),mtInformation,[mbOK],0);
    ShowRegisters;
  finally
    btnStop.Enabled := False;
    btnRun.Enabled := True;
    btnInit.Enabled := True;
  end;
end;

procedure TfrmBox80.btnStepClick(Sender: TObject);
var saved_pc: word;
begin
  saved_pc := FProcessor.PC;
  if not FProcessor.ExecuteInto then
    MessageDlg('Error','Illegal instruction at ' + IntToHex(saved_pc),mtError,[mbOK],0);
  ShowRegisters;
end;

procedure TfrmBox80.btnStopClick(Sender: TObject);
begin
  CancelRequested := True;
  btnStop.Enabled := False;
end;

procedure TfrmBox80.FormCreate(Sender: TObject);
begin
  FTerminal := TTerminal.Create(Self,80,25);
  FTerminal.Parent := Self;
  FTerminal.Top := 64;
  FTerminal.Left := 240;
  FTerminal.Width := 814;
  FTerminal.Height := 490;
//  FTerminal.Align := alClient;
  FTerminal.Color := TColor($00002800);
  FTerminal.Font.Color := clLime;
  FTerminal.Font.Name := 'Lucida Sans Typewriter';
  FTerminal.Font.Size := 10;
  FTimerClicks := 0;
  FProcessor := TProcessor.Create;
  FProcessor.OnTransmitA := @HandleSIOtransmitA;
end;

procedure TfrmBox80.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProcessor);
  FreeAndNil(FTerminal);
end;

procedure TfrmBox80.FormKeyPress(Sender: TObject; var Key: char);
begin
  // Form keypress routine
  // Put the key in the simulator SIO and trigger an interrupt
  FProcessor.ChannelReceiveA(Ord(key));
  frmBox80.SetFocus;
end;

procedure TfrmBox80.Timer1Timer(Sender: TObject);
begin
  Inc(FTimerClicks);
  if FTimerClicks >= 8 then
    FTimerClicks := 0;
  FTerminal.CursorLit := (FTimerClicks < 5);
  if (FTimerClicks = 0) or (FTimerClicks = 5) then
    begin
      FTerminal.Invalidate;
      Application.ProcessMessages;
    end;
end;

procedure TfrmBox80.HandleSIOtransmitA(_b: byte);
begin
  FTerminal.WriteChar(Chr(_b));
  FTimerClicks := 0;
end;

procedure TfrmBox80.ShowRegisters;
var i: integer;
    addr: Word;
    rs:   TRegisterSet;
    s: string;

  procedure SetFont(_ctrl: TControl; _selected: boolean);
  begin
    if _selected then
      begin
        _ctrl.Font.Style := _ctrl.Font.Style + [fsBold];
        _ctrl.Font.Color := clDefault;
      end
    else
      begin
        _ctrl.Font.Style := _ctrl.Font.Style - [fsBold];
        _ctrl.Font.Color := clInactiveCaption;
      end;
  end;

  procedure SetBack(_ctrl: TControl; _selected: boolean);
  begin
    if _selected then
      _ctrl.Color := clMenuHighlight
    else
      _ctrl.Color := clGradientInactiveCaption;
  end;

  procedure ShowBit(_lab: TLabel; _src: Word; _last: Word; _bit: integer);
  var mask: Word;
  begin
    mask := 1 shl _bit;
    SetFont(_lab,(_src and mask) <> 0);
    SetBack(_lab,((_src xor _last) and mask) <> 0);
  end;

  procedure ShowByte(_edt: TEdit; _src: Byte; _last: Byte);
  begin
    _edt.Text := IntToHex(_src,2);
    SetBack(_edt,(_src xor _last) <> 0);
  end;

  procedure ShowWord(_edt: TEdit; _src: Word; _last: Word);
  begin
    _edt.Text := IntToHex(_src,4);
    SetBack(_edt,(_src xor _last) <> 0);
  end;

begin
  rs := FProcessor.RegisterSet;
  ShowByte(edtA, rs.registers[regAF]  shr 8,last.registers[regAF]  shr 8);
  ShowByte(edtA_,rs.registers[regAF_] shr 8,last.registers[regAF_] shr 8);
  ShowBit(labF7,rs.registers[regAF],last.registers[regAF],7);
  ShowBit(labF6,rs.registers[regAF],last.registers[regAF],6);
  ShowBit(labF5,rs.registers[regAF],last.registers[regAF],5);
  ShowBit(labF4,rs.registers[regAF],last.registers[regAF],4);
  ShowBit(labF3,rs.registers[regAF],last.registers[regAF],3);
  ShowBit(labF2,rs.registers[regAF],last.registers[regAF],2);
  ShowBit(labF1,rs.registers[regAF],last.registers[regAF],1);
  ShowBit(labF0,rs.registers[regAF],last.registers[regAF],0);
  ShowBit(labF_7,rs.registers[regAF_],last.registers[regAF_],7);
  ShowBit(labF_6,rs.registers[regAF_],last.registers[regAF_],6);
  ShowBit(labF_5,rs.registers[regAF_],last.registers[regAF_],5);
  ShowBit(labF_4,rs.registers[regAF_],last.registers[regAF_],4);
  ShowBit(labF_3,rs.registers[regAF_],last.registers[regAF_],3);
  ShowBit(labF_2,rs.registers[regAF_],last.registers[regAF_],2);
  ShowBit(labF_1,rs.registers[regAF_],last.registers[regAF_],1);
  ShowBit(labF_0,rs.registers[regAF_],last.registers[regAF_],0);
  ShowWord(edtBC,rs.registers[regBC],last.registers[regBC]);
  ShowWord(edtDE,rs.registers[regDE],last.registers[regDE]);
  ShowWord(edtHL,rs.registers[regHL],last.registers[regHL]);
  ShowWord(edtBC_,rs.registers[regBC_],last.registers[regBC_]);
  ShowWord(edtDE_,rs.registers[regDE_],last.registers[regDE_]);
  ShowWord(edtHL_,rs.registers[regHL_],last.registers[regHL_]);
  ShowWord(edtIR,rs.registers[regIR],last.registers[regIR]);
  ShowWord(edtIX,rs.registers[regIX],last.registers[regIX]);
  ShowWord(edtIY,rs.registers[regIY],last.registers[regIY]);
  ShowWord(edtSP,rs.registers[regSP],last.registers[regSP]);
  ShowWord(edtPC,rs.registers[regPC],last.registers[regPC]);
  // Show what's at the program counter address
  addr := rs.registers[regPC];
  s := '';
  for i := 0 to 4 do
    begin
      if s <> '' then
        s := s + ' ';
      s := s + IntToHex(FProcessor.RAM[addr]);
      if addr = $FFFF then
        addr := 0
      else
        addr := addr + 1;
    end;
  edtAtPC.Text := s;
  // Update T and uS
  edtT.Text := Format('%-12.0n',[double(FProcessor.TStates)]);
  edtuS.Text := Format('%-12.2n',[FProcessor.TStates / FProcessor.CpuSpeed * 1000000.0 / 4.0]);
  // Update the "last" variables
  last := rs;
end;

procedure TfrmBox80.Status(const _msg: string);
begin
  StatusBar1.SimpleText := _msg;
end;

procedure TfrmBox80.Status(const _fmt: string; const _args: array of const);
begin
  Status(Format(_fmt,_args));
end;

end.

