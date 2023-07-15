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

unit fbox80;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, ActnList, Buttons, XMLPropStorage, uprocessor,
  generics.collections, uglobals;

const
{$IFDEF LINUX}
  MONITOR_BIN = '/home/duncan/Dropbox/dev/lazarus/computing/z80/box80/imported/g_searle/source/monitor.bin';
  BASIC_BIN   = '/home/duncan/Dropbox/dev/lazarus/computing/z80/box80/imported/g_searle/source/basic.bin';
{$ENDIF}
{$IFDEF WINDOWS}
  MONITOR_BIN  = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\imported\g_searle\source\monitor.bin';
  BASIC_BIN    = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\imported\g_searle\source\basic.bin';
//ZEXDOC_BIN   = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\test_files\validation\test_z80.com';
  CPM22_BIN    = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\imported\g_searle\source\cpm22.bin';
  CBIOS128_BIN = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\imported\g_searle\source\cbios64.bin';
  PUTSYS_BIN   = 'C:\Users\Duncan Munro\Dropbox\dev\lazarus\computing\z80\box80\imported\g_searle\source\putsys.bin';
{$ENDIF}


type

  TMRUObject = class(TObject)
    public
      Filename: string;
  end;

  TMRU = class(specialize TObjectList<TMRUObject>)
    private
      menu_array: array[0..MAXIMUM_MRU-1] of TMenuItem;
    public
      constructor Create;
      procedure AddMRU(const _filename: string);
      function  IndexOf(const _s: string): integer; reintroduce;
      procedure Load;
      procedure Save;
      procedure Sync;
  end;

  { TfrmBox80 }

  TfrmBox80 = class(TForm)
    actDebugRun: TAction;
    actDebugStepInto: TAction;
    actDebugStepOver: TAction;
    actDebugStop: TAction;
    actFileExit: TAction;
    actHelpAbout: TAction;
    actHelpWebsite: TAction;
    actHelpUserManual: TAction;
    actHelpLicence: TAction;
    actFileCFattach: TAction;
    actFileCFcreate64: TAction;
    actFileCFcreate128: TAction;
    actFileSaveAs: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileCFinfo: TAction;
    actVmLoadTPA: TAction;
    actVMWarmReset: TAction;
    actTerminalLoadFile: TAction;
    actVMCPU5000: TAction;
    actVMCPU3333: TAction;
    actVMCPU2000: TAction;
    actVMCPUmax: TAction;
    actVMCPU1000: TAction;
    actVMCPU00: TAction;
    actVMCPU60: TAction;
    actVMCPU500: TAction;
    actVMCPU184: TAction;
    actVMCPU73: TAction;
    ActionList1: TActionList;
    actVMCPU100: TAction;
    actVMCPU200: TAction;
    actVMCPU25: TAction;
    actVMCPU333: TAction;
    actVMCPU40: TAction;
    actVMCPU80: TAction;
    actVMColdReset: TAction;
    dlgOpenTPAfile: TOpenDialog;
    edtA: TEdit;
    edtAtPC: TEdit;
    edtA_: TEdit;
    edtBC: TEdit;
    edtBC_: TEdit;
    edtDE: TEdit;
    edtDE_: TEdit;
    edtHL: TEdit;
    edtHL_: TEdit;
    edtIR: TEdit;
    edtIX: TEdit;
    edtIY: TEdit;
    edtPC: TEdit;
    edtSP: TEdit;
    edtT: TEdit;
    edtuS: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    labMHz: TLabel;
    labMIPS: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    labF0: TLabel;
    labF1: TLabel;
    labF2: TLabel;
    labF3: TLabel;
    labF4: TLabel;
    labF5: TLabel;
    labF6: TLabel;
    labF7: TLabel;
    labF_0: TLabel;
    labF_1: TLabel;
    labF_2: TLabel;
    labF_3: TLabel;
    labF_4: TLabel;
    labF_5: TLabel;
    labF_6: TLabel;
    labF_7: TLabel;
    labStatus: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    FileMRU06: TMenuItem;
    FileMRU05: TMenuItem;
    FileMRU04: TMenuItem;
    FileMRU03: TMenuItem;
    FileMRU02: TMenuItem;
    FileMRU01: TMenuItem;
    FileMRU00: TMenuItem;
    FileMRU09: TMenuItem;
    FIleMRU08: TMenuItem;
    FileMRU07: TMenuItem;
    miVMSep2: TMenuItem;
    miVMLoadTPA: TMenuItem;
    miWarmReset: TMenuItem;
    miTerminalLoad: TMenuItem;
    miTerminal: TMenuItem;
    miFileCFsep1: TMenuItem;
    MenuItem8: TMenuItem;
    miFileSepMRU: TMenuItem;
    miFileOpen: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    menuFileCFcreate: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miFileSep2: TMenuItem;
    miFileSaveAs: TMenuItem;
    miFileSave: TMenuItem;
    miFileSep1: TMenuItem;
    miFileCF: TMenuItem;
    miVMCPUSep1: TMenuItem;
    miHelpLicence: TMenuItem;
    miHelpUserManual: TMenuItem;
    miHelpWebsite: TMenuItem;
    miHelpAbout: TMenuItem;
    miVMCPUmax: TMenuItem;
    miHelpSep1: TMenuItem;
    miHelp: TMenuItem;
    miVMCPU1000: TMenuItem;
    miVMCPU60: TMenuItem;
    miVMCPU00: TMenuItem;
    miVMCPU500: TMenuItem;
    miVMCPU184: TMenuItem;
    miVMCPU73: TMenuItem;
    miDebug: TMenuItem;
    miDebugRun: TMenuItem;
    miDebugStepInto: TMenuItem;
    miDebugStepOver: TMenuItem;
    miDebugStop: TMenuItem;
    miFile: TMenuItem;
    miFileExit: TMenuItem;
    miVM: TMenuItem;
    miVMCPU: TMenuItem;
    miVMCPU100: TMenuItem;
    miVMCPU200: TMenuItem;
    miVMCPU25: TMenuItem;
    miVMCPU333: TMenuItem;
    miVMCPU40: TMenuItem;
    miVMCPU80: TMenuItem;
    miVMReset: TMenuItem;
    miVMsep1: TMenuItem;
    dlgOpenVM: TOpenDialog;
    dlgOpenCF: TOpenDialog;
    OpenDialog1: TOpenDialog;
    dlgOpenTerm: TOpenDialog;
    pnlWatch: TPanel;
    pnlDisassembler: TPanel;
    pnlRegisters: TPanel;
    pnlFileEnd: TPanel;
    pnlButton: TPanel;
    pnlFileEnd1: TPanel;
    dlgCreateCF: TSaveDialog;
    dlgCreateVM: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    config: TXMLPropStorage;
    procedure actFileCFattachExecute(Sender: TObject);
    procedure actFileCFcreate128Execute(Sender: TObject);
    procedure actFileCFcreate64Execute(Sender: TObject);
    procedure actFileCFinfoExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actHelpLicenceExecute(Sender: TObject);
    procedure actHelpUserManualExecute(Sender: TObject);
    procedure actHelpWebsiteExecute(Sender: TObject);
    procedure actTerminalLoadFileExecute(Sender: TObject);
    procedure actVMCPU00Execute(Sender: TObject);
    procedure actVMCPU1000Execute(Sender: TObject);
    procedure actVMCPU100Execute(Sender: TObject);
    procedure actVMCPU184Execute(Sender: TObject);
    procedure actVMCPU2000Execute(Sender: TObject);
    procedure actVMCPU200Execute(Sender: TObject);
    procedure actVMCPU25Execute(Sender: TObject);
    procedure actVMCPU3333Execute(Sender: TObject);
    procedure actVMCPU333Execute(Sender: TObject);
    procedure actVMCPU40Execute(Sender: TObject);
    procedure actVMCPU5000Execute(Sender: TObject);
    procedure actVMCPU500Execute(Sender: TObject);
    procedure actVMCPU60Execute(Sender: TObject);
    procedure actVMCPU73Execute(Sender: TObject);
    procedure actVMCPU80Execute(Sender: TObject);
    procedure actVMCPUmaxExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actVMColdResetExecute(Sender: TObject);
    procedure actDebugRunExecute(Sender: TObject);
    procedure actDebugStepIntoExecute(Sender: TObject);
    procedure actDebugStepOverExecute(Sender: TObject);
    procedure actDebugStopExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actVmLoadTPAExecute(Sender: TObject);
    procedure actVMWarmResetExecute(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure configRestoreProperties(Sender: TObject);
    procedure FileMRU00Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlDisassemblerClick(Sender: TObject);
    procedure pnlWatchClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    CancelRequested: boolean;
    FocusAllowed: boolean;
    FMRU: TMRU;
    FProcessor: TProcessor;
    FTimerClicks: integer;
    FVMName: string;
    last: TRegisterSet;
    localNeedsUpdate: boolean;
    localProcStatus: TProcessorState;
    procedure CreateCF(_cfsize: integer);
    function  GetConfig(_section,_name: string; _default: string = ''): string;
    procedure GrabFocus;
    function  HandleSIOtransmitA(_b: byte): boolean;
    procedure ProcStateChange(_ps: TProcessorState);
    procedure ProcStateUpdate;
    procedure PutConfig(_section,_name,_value: string);
    procedure ReadMonitorImage;
    procedure SetActions;
    procedure SetVmName(const _s: string);
    procedure ShowRegisters;
    procedure Status(const _msg: string);
    procedure Status(const _fmt: string; const _args: array of const);
    property VMName: string read FVMName write SetVMName;
  public

  end;

var
  frmBox80: TfrmBox80;

implementation

{$R *.lfm}

uses
  fterminal, fabout, lclintf, uconfigdefs, uvirtual, FileCtrl, ucflash,
  fcfinfo, usio, fmemory;

{ TfrmBox80 }

procedure TfrmBox80.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmBox80.actFileSaveAsExecute(Sender: TObject);
begin
  dlgCreateVM.InitialDir := GetConfig(SECTION_FOLDERS,CONFIG_FOLDER_VM,'');
  if dlgCreateVM.Execute then
    begin
      VMName := dlgCreateVM.FileName;
      PutConfig(SECTION_FOLDERS,CONFIG_FOLDER_VM,dlgCreateVM.InitialDir);
      actFileSaveExecute(Self);
    end;
end;

procedure TfrmBox80.actFileSaveExecute(Sender: TObject);
begin
  if VMName <> '' then
    begin
      SaveVM(VMName,FProcessor,frmTerminal.Terminal);
      FMRU.AddMRU(VMName);
    end;
end;

procedure TfrmBox80.actVmLoadTPAExecute(Sender: TObject);
var tpaname: string;
    processor_state: TProcessorState;
    stream: TFileStream;
    buf: array[$0100..$D000-1] of byte;
    i,bytes: integer;
begin
  dlgOpenTPAfile.InitialDir := GetConfig(SECTION_FOLDERS,CONFIG_FOLDER_TPA,'');
  if dlgOpenTPAfile.Execute then
    begin
      tpaname := dlgOpenTPAfile.FileName;
      processor_state := FProcessor.ProcessorState;
      FProcessor.ExecuteStop;
      FProcessor.WaitForStop;
      // Do the load
      stream := TFileStream.Create(tpaname,fmOpenRead);
      try
        bytes := stream.Read({%H-}buf[$100],$D000-$0100);
        for i := 0 to bytes-1 do
          FProcessor.WriteRAM(i+$0100,buf[i+$0100]);
      finally
        FreeAndNil(stream);
      end;
      // Start the processor up again if needed
      FProcessor.ProcessorState := processor_state;
      ShowRegisters;
    end;
end;

procedure TfrmBox80.actVMWarmResetExecute(Sender: TObject);
var saved_state: TProcessorState;
begin
  saved_state := FProcessor.ProcessorState;
  if saved_state in [psRunning,psStepOver] then
    begin
      FProcessor.ExecuteStop;
      FProcessor.WaitForStop;
    end;
  FProcessor.PC := 0; // Warm start is at address 0
  ShowRegisters;
  ProcStateUpdate;
  FProcessor.ProcessorState := saved_state;
end;

procedure TfrmBox80.actDebugStopExecute(Sender: TObject);
begin
  FProcessor.ExecuteStop;
end;

procedure TfrmBox80.actDebugRunExecute(Sender: TObject);
begin
  frmTerminal.SetFocus;
  FProcessor.ExecuteRun;
end;

procedure TfrmBox80.actDebugStepIntoExecute(Sender: TObject);
begin
  FProcessor.ExecuteStepInto;
end;

procedure TfrmBox80.actDebugStepOverExecute(Sender: TObject);
begin
  FProcessor.ExecuteStepOver;
end;

procedure TfrmBox80.actVMCPU40Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 4000000;
end;

procedure TfrmBox80.actVMCPU5000Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 500000000;
end;

procedure TfrmBox80.actVMCPU500Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 50000000;
end;

procedure TfrmBox80.actVMCPU60Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 6000000;
end;

procedure TfrmBox80.actVMCPU73Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 7372800;
end;

procedure TfrmBox80.actVMCPU80Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 8000000;
end;

procedure TfrmBox80.actVMCPUmaxExecute(Sender: TObject);
begin
  FProcessor.CPUspeed := MAXIMUM_CPU_SPEED;
end;

procedure TfrmBox80.actFileOpenExecute(Sender: TObject);
begin
  dlgOpenVM.InitialDir := GetConfig(SECTION_FOLDERS,CONFIG_FOLDER_VM,'');
  if dlgOpenVM.Execute and (dlgOpenVM.FileName <> '') then
    begin
      VMName := dlgOpenVM.FileName;
      if LoadVM(VMName,FProcessor,frmTerminal.Terminal) then
        frmTerminal.SetFocus;
      FMRU.AddMRU(VMName);
      ShowRegisters;
    end;
end;

procedure TfrmBox80.actVMColdResetExecute(Sender: TObject);
{
var saved_state: TProcessorState;
}
begin
  // saved_state := FProcessor.ProcessorState;
  FProcessor.WaitForStop;
  FProcessor.Reset(True);
  ReadMonitorImage;
  ShowRegisters;
  ProcStateUpdate;
  frmTerminal.Init;
  // @@@@@ Future use might enable the following, allow running machine to
  // continue to run after reset
  {
  if saved_state = psRunning then
    FProcessor.ProcessorState := saved_state;
  }
end;

procedure TfrmBox80.actVMCPU25Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 2500000;
end;

procedure TfrmBox80.actVMCPU3333Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 333333333;
end;

procedure TfrmBox80.actVMCPU333Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 33333333;
end;

procedure TfrmBox80.actVMCPU100Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 10000000;
end;

procedure TfrmBox80.actVMCPU00Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 32768;
end;

procedure TfrmBox80.actHelpAboutExecute(Sender: TObject);
begin
  frmHelpAbout.ShowModal;
end;

procedure TfrmBox80.actFileCFcreate64Execute(Sender: TObject);
begin
  CreateCF(CF_DEFAULT_SIZE_64);
end;

procedure TfrmBox80.actFileCFinfoExecute(Sender: TObject);
begin
  frmCFinfo.ShowInfo(FProcessor);
end;

procedure TfrmBox80.actFileCFcreate128Execute(Sender: TObject);
begin
  CreateCF(CF_DEFAULT_SIZE_128);
end;

procedure TfrmBox80.actFileCFattachExecute(Sender: TObject);
begin
  dlgOpenCF.InitialDir := GetConfig(SECTION_FOLDERS,CONFIG_FOLDER_CF,'');
  if dlgOpenCF.Execute and (dlgOpenCF.FileName <> '') then
    begin
      PutConfig(SECTION_FOLDERS,CONFIG_FOLDER_CF,dlgOpenCF.InitialDir);
      FProcessor.WaitForStop;
      FProcessor.CFlash.Filename := dlgOpenCF.FileName;
//      ActVmResetExecute(Self);
    end;
end;

procedure TfrmBox80.actHelpLicenceExecute(Sender: TObject);
begin
  OpenUrl('https://www.gnu.org/licenses/gpl-3.0.en.html');
end;

procedure TfrmBox80.actHelpUserManualExecute(Sender: TObject);
begin
  OpenUrl('https://github.com/duncanamps/box80/tree/v0.0/docs/box80_vm_user_manual_0.0.pdf');
end;

procedure TfrmBox80.actHelpWebsiteExecute(Sender: TObject);
begin
  OpenUrl('https://github.com/duncanamps/box80');
end;

procedure TfrmBox80.actTerminalLoadFileExecute(Sender: TObject);
var b: byte;
    strm: TFileStream;
    capacity: integer;
    counter: int64;
begin
  dlgOpenTerm.InitialDir := GetConfig(SECTION_FOLDERS,CONFIG_FOLDER_TEXT,'');
  if dlgOpenTerm.Execute and (dlgOpenTerm.FileName <> '') then
    begin
      frmTerminal.SetFocus;
      PutConfig(SECTION_FOLDERS,CONFIG_FOLDER_TEXT,dlgOpenTerm.InitialDir);
      strm := TFileStream.Create(dlgOpenTerm.Filename,fmOpenRead);
      try
        capacity := 0;
        counter := 0;
        while strm.Position < strm.Size do
          begin
            while capacity = 0 do
              begin
                capacity := FProcessor.SIO.ChannelA.BufCapacity;
                Application.ProcessMessages;
//              sleep(10);
              end;
            Inc(counter);
            if counter and $7F = 0 then
              Application.ProcessMessages;
            Dec(capacity);
            b := strm.ReadByte;
            FProcessor.ChannelReceiveA(b);
          end;
      finally
        FreeAndNil(strm);
      end;
    end;
end;

procedure TfrmBox80.actVMCPU1000Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 100000000;
end;

procedure TfrmBox80.actVMCPU184Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 18432000;
end;

procedure TfrmBox80.actVMCPU2000Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 200000000;
end;

procedure TfrmBox80.actVMCPU200Execute(Sender: TObject);
begin
  FProcessor.CPUspeed := 20000000;
end;

procedure TfrmBox80.btnStopClick(Sender: TObject);
begin
  CancelRequested := True;
end;

procedure TfrmBox80.configRestoreProperties(Sender: TObject);
begin

end;

procedure TfrmBox80.FileMRU00Click(Sender: TObject);
var index: integer;
begin
  with Sender as TMenuItem do
    index := StrToInt(Copy(Caption,2,1));
  VMName := FMRU.Items[index].Filename;
  if LoadVM(VMName,FProcessor,frmTerminal.Terminal) then
    frmTerminal.SetFocus;
  FMRU.AddMRU(VMName);
  ShowRegisters;
end;


procedure TfrmBox80.CreateCF(_cfsize: integer);
var cfext:     string;
    filename:  string;
    saved_cur: TCursor;
begin
  cfext := 'vdi80';
  dlgCreateCF.DefaultExt := '.' + cfext;
  dlgCreateCF.Filter := 'Compact Flash image|*.' + cfext;
  dlgCreateCF.InitialDir := GetConfig(SECTION_FOLDERS,CONFIG_FOLDER_CF,'');
  if dlgCreateCF.Execute then
    begin
      filename := dlgCreateCF.FileName;
      PutConfig(SECTION_FOLDERS,CONFIG_FOLDER_CF,dlgCreateCF.InitialDir);
      saved_cur := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        FProcessor.CFlash.CreateImage(filename,_cfsize);
      finally
        Screen.Cursor := saved_cur;
      end;
    end;
end;

procedure TfrmBox80.FormActivate(Sender: TObject);
begin
  frmTerminal.Processor := FProcessor;
  frmMemory.Processor := FProcessor;
  frmMemory.Visible := True;
  frmMemory.StartAddr := $2000;
  FocusAllowed := True;
end;

procedure TfrmBox80.FormCreate(Sender: TObject);
begin
  // Set up some of the OS specific stuff
  config.FileName := GetAppConfigDir(False) + 'config.xml';
  ForceDirectories(ExtractFilePath(config.FileName));
  // Set up the remaining items
  VMName := '';
  FTimerClicks := 0;
  FProcessor := TProcessor.Create;
  FProcessor.OnStateChange := @ProcStateChange;
  FProcessor.OnTransmitA := @HandleSIOtransmitA;
  FProcessor.CFlash.PortBase := $10;
  FProcessor.Reset(True);
  ReadMonitorImage;
  FProcessor.ProcessorState := psPaused;
  FProcessor.Suspended := False;
  Timer1.Enabled := True;
  ShowRegisters;
  ProcStateUpdate;
  FMRU := TMRU.Create;
  FMRU.Load;
end;

procedure TfrmBox80.FormDestroy(Sender: TObject);
begin
  FMRU.Save;
  FreeAndNil(FMRU);
  Timer1.Enabled := False;
  FProcessor.WaitForStop;
  FProcessor.Terminate;   // Will delete its own instance
  while not FProcessor.Finished do
    Sleep(50);  // Wait until it's gone
end;

procedure TfrmBox80.pnlDisassemblerClick(Sender: TObject);
begin

end;

procedure TfrmBox80.pnlWatchClick(Sender: TObject);
begin

end;

function TfrmBox80.GetConfig(_section,_name: string; _default: string = ''): string;
begin
  Result := config.ReadString(_section + '/' + _name,_default);
end;

procedure TfrmBox80.GrabFocus;
begin
  if FocusAllowed then
    SetFocus;
end;

procedure TfrmBox80.Timer1Timer(Sender: TObject);
begin
  if localNeedsUpdate then
    begin
      ProcStateUpdate;
      ShowRegisters;
      if efIllegal in FProcessor.ErrorFlag then
        MessageDlg('Error','Illegal instruction at ' + IntToHex(FProcessor.SavedPC),mtError,[mbOK],0);
      if efUndocumented in FProcessor.ErrorFlag then
        MessageDlg('Error','Undocumented instruction at ' + IntToHex(FProcessor.SavedPC),mtError,[mbOK],0);
      if efHalt in FProcessor.ErrorFlag then
        MessageDlg('Information','Processor halted at ' + IntToHex(FProcessor.SavedPC),mtInformation,[mbOK],0);
    end
  else if localProcStatus = psRunning then
    ShowRegisters;
  // Update terminal form with SIOA/B percentages full
  frmTerminal.SIOApct := FProcessor.SIO.ChannelA.BufPercent;
  frmTerminal.SIOBpct := FProcessor.SIO.ChannelB.BufPercent;
end;

function TfrmBox80.HandleSIOtransmitA(_b: byte): boolean;
begin
  Result := frmTerminal.HasScreenCapacity;
  if Result then
    frmTerminal.WriteChar(Chr(_b))
  {
  FTerminal.WriteChar(Chr(_b));
  FTimerClicks := 0;
  }
end;

procedure TfrmBox80.ProcStateChange(_ps: TProcessorState);
begin
  if _ps <> localProcStatus then
    begin
      localProcStatus := _ps;
      localNeedsUpdate := True;
    end;
end;

procedure TfrmBox80.ProcStateUpdate;
begin
  localNeedsUpdate := False;
  SetActions;
  case localProcStatus of
    psNone:
      begin
        labStatus.Caption := 'Not Started';
        labStatus.Color := clSilver;
        labStatus.Font.Color := clDefault;
      end;
    psPaused:
      begin
        labStatus.Caption := 'PAUSED';
        labStatus.Color := clOlive;
        labStatus.Font.Color := clWhite;
        Status('Paused');
        GrabFocus;
      end;
    psRunning:
      begin
        labStatus.Caption := 'Running';
        labStatus.Color := clGreen;
        labStatus.Font.Color := clWhite;
        Status('Running');
      end;
    psFault:
      begin
        labStatus.Caption := 'FAULT';
        labStatus.Color := clRed;
        labStatus.Font.Color := clWhite;
        Status(FProcessor.ErrorString);
        GrabFocus;
      end;
    psBreak:
      begin
        labStatus.Caption := 'Break';
        labStatus.Color := clMaroon;
        labStatus.Font.Color := clWhite;
        Status('Breakpoint');
        GrabFocus;
      end;
  end;
end;

procedure TfrmBox80.PutConfig(_section,_name,_value: string);
begin
  config.WriteString(_section + '/' + _name, _value);
end;

procedure TfrmBox80.ReadMonitorImage;
var strm: TFileStream;

  procedure ReadImage(_name: string; _start: integer);
  begin
    strm := TFileStream.Create(_name,fmOpenRead);
    try
      // Attempt to read 32K
      FProcessor.ReadFromStream(strm,_start,strm.Size);
    finally
      FreeAndNil(strm);
    end;
  end;

begin
  ReadImage(MONITOR_BIN,$0000);
//ReadImage(ZEXDOC_BIN,$2000);
  ReadImage(BASIC_BIN,$2000);
  ReadImage(CPM22_BIN,$D000);
  ReadImage(CBIOS128_BIN,$E600);
  ReadImage(PUTSYS_BIN,$5000);
end;

procedure TfrmBox80.SetActions;
var stopped: set of TProcessorState;
begin
  stopped := [psPaused,psFault,psBreak];
  actDebugRun.Enabled        := localProcStatus in stopped;
  actDebugStepInto.Enabled   := localProcStatus in stopped;
  actDebugStepOver.Enabled   := localProcStatus in stopped;
  actDebugStop.Enabled       := localProcStatus in [psRunning];
  actFileCFattach.Enabled    := localProcStatus in stopped;
  actFileCFcreate64.Enabled  := localProcStatus in stopped;
  actFileCFcreate128.Enabled := localProcStatus in stopped;
  actFileOpen.Enabled          := True;
  actFileSave.Enabled          := FVMName <> '';
  actFileSaveAs.Enabled        := True;
end;

procedure TfrmBox80.SetVmName(const _s: string);
begin
  FVMName := _s;
  if _s = '' then
    Caption := '<unnamed>'
  else
    Caption := MinimizeName(FVMName,Self.Canvas,Width-32);
  SetActions;
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
  edtT.Text := Format('%12.0n',[double(FProcessor.TStates)]);
  if FProcessor.CPUspeed = MAXIMUM_CPU_SPEED then
    edtuS.Text := 'Invalid'
  else
    edtuS.Text := Format('%12.2n',[FProcessor.TStates / FProcessor.CpuSpeed * 1000000.0]);
  labMHz.Caption := Format('%7.3f',[FProcessor.PerfMHz]);
  labMIPS.Caption := Format('%7.3f',[FProcessor.PerfMIPS]);
  // Update the "last" variables
  last := rs;
  // Update related screens
{$IFDEF UPDATE_MEMORY}
  if Assigned(frmMemory) then
    frmMemory.Refresh;
{$ENDIF}
end;

procedure TfrmBox80.Status(const _msg: string);
begin
  StatusBar1.SimpleText := _msg;
end;

procedure TfrmBox80.Status(const _fmt: string; const _args: array of const);
begin
  Status(Format(_fmt,_args));
end;



//----------------------------------------------------------------------------
//
//  TMRU code
//
//----------------------------------------------------------------------------

constructor TMRU.Create;
begin
  inherited Create;
  // There's probably a better way of doing this...
  menu_array[0] := frmBox80.FileMRU00;
  menu_array[1] := frmBox80.FileMRU01;
  menu_array[2] := frmBox80.FileMRU02;
  menu_array[3] := frmBox80.FileMRU03;
  menu_array[4] := frmBox80.FileMRU04;
  menu_array[5] := frmBox80.FileMRU05;
  menu_array[6] := frmBox80.FileMRU06;
  menu_array[7] := frmBox80.FileMRU07;
  menu_array[8] := frmBox80.FileMRU08;
  menu_array[9] := frmBox80.FileMRU09;
end;

procedure TMRU.AddMRU(const _filename: string);
var obj: TMRUObject;
begin
  // Check if filename already in list and delete it if so
  if IndexOf(_filename) >= 0 then
    Delete(IndexOf(_filename));
  // Make a space if we need to
  if Count >= MAXIMUM_MRU then
    Delete(MAXIMUM_MRU-1); // Remove the end of the list
  // Now create the new object and add it
  obj := TMRUObject.Create;
  obj.Filename := _filename;
  Insert(0,obj);
  Sync;
end;

function TMRU.IndexOf(const _s: string): integer;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Filename <> _s) do
    Inc(i);
  if i >= Count then
    Result := -1
  else
    Result := i;
end;

procedure TMRU.Load;
var i: integer;
    fn: string;
begin
  for i := MAXIMUM_MRU-1 downto 0 do
    begin
      fn := frmBox80.GetConfig(SECTION_MRU,CONFIG_MRU_PREFIX+IntToStr(i),'');
      if fn <> '' then
        AddMRU(fn);
    end;
  Sync;
end;

procedure TMRU.Sync;
var i: integer;
    prefix: string;
begin
  frmBox80.miFileSepMRU.Visible := (Count > 0);
  for i := 0 to MAXIMUM_MRU-1 do
    if i < Count then
      begin
        if i < 10 then
          prefix := '&' + IntToStr(i) + ' '
        else
          prefix := '';
        menu_array[i].Caption := prefix + MinimizeName(Items[i].Filename,frmBox80.Canvas,frmBox80.Width div 3);
        menu_array[i].Visible := True;
      end
    else
      menu_array[i].Visible := False;
end;

procedure TMRU.Save;
var i: integer;
begin
  for i := 0 to MAXIMUM_MRU-1 do
    if i < Count then
      frmBox80.PutConfig(SECTION_MRU,CONFIG_MRU_PREFIX+IntToStr(i),Items[i].Filename)
    else
      frmBox80.PutConfig(SECTION_MRU,CONFIG_MRU_PREFIX+IntToStr(i),'');
end;

end.

