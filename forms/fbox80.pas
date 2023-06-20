unit fbox80;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uprocessor;

const
  MONITOR_BIN = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\g_searle\source\monitor.bin';


type

  { TfrmBox80 }

  TfrmBox80 = class(TForm)
    btnInit: TButton;
    btnStep: TButton;
    btnRun: TButton;
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
    procedure btnInitClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
  private
    last_af:  Word;
    last_bc:  Word;
    last_de:  Word;
    last_hl:  Word;
    last_af_: Word;
    last_bc_: Word;
    last_de_: Word;
    last_hl_: Word;
    last_ir:  Word;
    last_ix:  Word;
    last_iy:  Word;
    last_sp:  Word;
    last_pc:  Word;
    procedure ShowRegisters;
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
  processor_init;
  // Load Grant Searle monitor ROM to $0000
  strm := TFileStream.Create(MONITOR_BIN,fmOpenRead);
  try
    // Attempt to read 32K
    strm.Read(ramarray[0],32768);
  finally
    FreeAndNil(strm);
  end;
  ShowRegisters;
end;

procedure TfrmBox80.btnRunClick(Sender: TObject);
var saved_pc: word;
    good: boolean;
begin
  good := True;
  while good do
    begin
      saved_pc := pc;
      good := processor_execute;
    end;
  MessageDlg('Error','Illegal instruction at ' + IntToHex(saved_pc),mtError,[mbOK],0);
  ShowRegisters;
end;

procedure TfrmBox80.btnStepClick(Sender: TObject);
var saved_pc: word;
begin
  saved_pc := pc;
  if not processor_execute then
    MessageDlg('Error','Illegal instruction at ' + IntToHex(saved_pc),mtError,[mbOK],0);
  ShowRegisters;
end;


procedure TfrmBox80.ShowRegisters;
var i: integer;
    addr: Word;
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
  ShowByte(edtA, af  shr 8,last_af  shr 8);
  ShowByte(edtA_,af_ shr 8,last_af_ shr 8);
  ShowBit(labF7,af,last_af,7);
  ShowBit(labF6,af,last_af,6);
  ShowBit(labF5,af,last_af,5);
  ShowBit(labF4,af,last_af,4);
  ShowBit(labF3,af,last_af,3);
  ShowBit(labF2,af,last_af,2);
  ShowBit(labF1,af,last_af,1);
  ShowBit(labF0,af,last_af,0);
  ShowWord(edtBC,bc,last_bc);
  ShowWord(edtDE,de,last_de);
  ShowWord(edtHL,hl,last_hl);
  ShowWord(edtBC_,bc_,last_bc_);
  ShowWord(edtDE_,de_,last_de_);
  ShowWord(edtHL_,hl_,last_hl_);
  ShowWord(edtIR,ir,last_ir);
  ShowWord(edtIX,ix,last_ix);
  ShowWord(edtIY,iy,last_iy);
  ShowWord(edtSP,sp,last_sp);
  ShowWord(edtPC,pc,last_pc);
  // Show what's at the program counter address
  addr := pc;
  s := '';
  for i := 0 to 4 do
    begin
      if s <> '' then
        s := s + ' ';
      s := s + IntToHex(ramarray[addr]);
      if addr = $FFFF then
        addr := 0
      else
        addr := addr + 1;
    end;
  edtAtPC.Text := s;
  // Update T and uS
  edtT.Text := IntToStr(t_states);
  edtuS.Text := Format('%12.2f',[t_states / cpu_speed * 1000000.0]);
  // Update the "last" variables
  last_af  := af;
  last_bc  := bc;
  last_de  := de;
  last_hl  := hl;
  last_af_ := af_;
  last_bc_ := bc_;
  last_de_ := de_;
  last_hl_ := hl_;
  last_ir  := ir;
  last_ix  := ix;
  last_iy  := iy;
  last_sp  := sp;
  last_pc  := pc;
end;

end.

