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

unit uprocessor;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, usio, DOM, ucflash;

const
  FLAG_NEGATIVE  = $80;
  FLAG_ZERO      = $40;
  FLAG_X5        = $20;
  FLAG_HALFCARRY = $10;
  FLAG_X3        = $08;
  FLAG_PV        = $04;
  FLAG_SUBTRACT  = $02;
  FLAG_CARRY     = $01;
  NOT_FLAG_NEGATIVE  = (FLAG_NEGATIVE   xor $FF);
  NOT_FLAG_ZERO      = (FLAG_ZERO       xor $FF);
  NOT_FLAG_X5        = (FLAG_X5         xor $FF);
  NOT_FLAG_HALFCARRY = (FLAG_HALFCARRY  xor $FF);
  NOT_FLAG_X3        = (FLAG_X3         xor $FF);
  NOT_FLAG_PV        = (FLAG_PV         xor $FF);
  NOT_FLAG_SUBTRACT  = (FLAG_SUBTRACT   xor $FF);
  NOT_FLAG_CARRY     = (FLAG_CARRY      xor $FF);
  SIOA_D = 0;
  SIOA_C = 2;
  SIOB_D = 1;
  SIOB_C = 3;
{$IFDEF EXEC_TRACE}
  EXEC_TRACE_SIZE = 32;
{$ENDIF}


type
  PByte = ^Byte;
  PWord = ^Word;

  TBreakpointFlag = (bpTemporary,bpPermanent);
  TBreakpointFlags = set of TBreakpointFlag;

  TErrorFlag = (efIllegal,efUndocumented,efHalt,efBadPortIn,efBadPortOut);
  TErrorFlags = set of TErrorFlag;

  TPortInFunction = function(_portno: Word): byte of object;
  TPortOutProcedure = procedure(_portno: Word; _value: byte) of object;


  TSIOtransmitFunc = function(_value: byte): boolean of object;

  TRAMarray = array[Word] of byte;

  TRegIndex = (regAF,regBC,regDE,regHL,regAF_,regBC_,regDE_,regHL_,regIR,
               regIX,regIY,regSP,regPC);

  TRegisterSet = record
    registers:     array[TRegIndex] of Word;
    int_enabled:   byte;
    int_mode:      byte;
  end;

  TExecProc = procedure of object;

  TProcessorState = (psNone,psPaused,psStepInto,psStepOver,psRunning,psBreak,psFault);

  TStateChangeProc = procedure(_newstate: TProcessorState) of object;

  TProcessor = class(TThread)
    private
      regset:        TRegisterSet;
      ramarray:      TRAMarray;
      bparray:       array[Word] of TBreakPointFlags;
      parity_table:  array[byte] of byte;
      inst_std:      array[byte] of TExecProc;
      inst_cb:       array[byte] of TExecProc;
      inst_ddfd:     array[byte] of TExecProc; // Handles DD and FD prefixes
      inst_ddfdcb:   array[byte] of TExecProc; // Handles DD and FD prefixes followed by CB
      inst_ed:       array[byte] of TExecProc;
      undoc_cb:      array[byte] of boolean;
      undoc_ed:      array[byte] of boolean;
      int_flag:      boolean;
      int_vec:       byte;
      t_states:      int64;
      insts:         int64;
      cpu_speed:     int64;  // In Hz
      error_flag:    TErrorFlags;
      big_counter:   int64;
      opcode:        byte;
      FAllowUndocumented: boolean;
      FCFlash:       TCompactFlashInterface;
      FProcessorState:  TProcessorState;
      run_start_time:         TDateTime;
      run_start_cycles:       int64;
      run_start_instructions: int64;
      FOnTransmitA: TSIOtransmitFunc;
      FOnStateChange: TStateChangeProc;
      FErrorString: string;
      // Procs / funcs
      function  Add16u8s(_word: Word; _byte: byte): Word;
      function  Fetch16: Word; inline;
      function  CanInterrupt: boolean;
      procedure ClearTemporaryBreakpoints;
      function  Fetch8: byte; inline;
      function  FetchIQPDindex: Word; inline;
      function  FetchOpcode: byte; inline;
      function  Flag(_flagmask: byte): boolean; inline;
      function  FlagNegative: boolean; inline;
      function  FlagZero: boolean; inline;
      function  FlagHalfCarry: boolean; inline;
      function  FlagPV: boolean; inline;
      function  FlagSubtract: boolean; inline;
      function  FlagCarry: boolean; inline;
      procedure SetFlag(_flagmask: byte; _value: boolean); inline;
      procedure SetFlagNegative(_value: boolean); inline;
      procedure SetFlagZero(_value: boolean); inline;
      procedure SetFlagHalfCarry(_value: boolean); inline;
      procedure SetFlagPV(_value: boolean); inline;
      procedure SetFlagSubtract(_value: boolean); inline;
      procedure SetFlagCarry(_value: boolean); inline;
      procedure SetFlagNZ53;
      function  GetCFPortBase: byte;
      function  GetRegisterSet: TRegisterSet;
      function  GetPerfMIPS: double;
      function  GetPerfMHz: double;
      function  PopWord: Word; inline;
      function  ProcessPortIn(_port: byte): byte;
      procedure SetCFPortBase(_b: byte);
      procedure ExecAdd(_b: byte; _states: integer; _doadc: boolean = False); inline;
      procedure ExecADDA; inline;
      procedure ExecADDB; inline;
      procedure ExecADDC; inline;
      procedure ExecADDD; inline;
      procedure ExecADDE; inline;
      procedure ExecADDH; inline;
      procedure ExecADDHLr16(_w: Word; _doadc: boolean = False); inline;
      procedure ExecADDHLBC; inline;
      procedure ExecADDHLDE; inline;
      procedure ExecADDHLHL; inline;
      procedure ExecADDHLSP; inline;
      procedure ExecADDimm; inline;
      procedure ExecADDL; inline;
      procedure ExecADDM; inline;
      procedure ExecADDr16r16(_pdst: PWord; _w: Word; _states: integer; _doadc: boolean = False); {inline;}
      procedure ExecADCB; inline;
      procedure ExecADCC; inline;
      procedure ExecADCD; inline;
      procedure ExecADCE; inline;
      procedure ExecADCH; inline;
      procedure ExecADCimm; inline;
      procedure ExecADCL; inline;
      procedure ExecADCM; inline;
      procedure ExecADCA; inline;
      procedure ExecANDA; inline;
      procedure ExecANDB; inline;
      procedure ExecANDC; inline;
      procedure ExecANDD; inline;
      procedure ExecANDE; inline;
      procedure ExecANDH; inline;
      procedure ExecANDL; inline;
      procedure ExecANDM; inline;
      procedure ExecANDimm; inline;
      procedure ExecCALLabs; inline;
      procedure ExecCALLC; inline;
      procedure ExecCALLcond(_mask, _required: byte); inline;
      procedure ExecCALLM; inline;
      procedure ExecCALLNC; inline;
      procedure ExecCALLNZ; inline;
      procedure ExecCALLP; inline;
      procedure ExecCALLPE; inline;
      procedure ExecCALLPO; inline;
      procedure ExecCALLZ; inline;
      procedure ExecCCF; inline;
      procedure ExecCPA; inline;
      procedure ExecCPB; inline;
      procedure ExecCPC; inline;
      procedure ExecCPD; inline;
      procedure ExecCPE; inline;
      procedure ExecCPimm; inline;
      procedure ExecCPH; inline;
      procedure ExecCPL; inline;
      procedure ExecCPL2; inline;
      procedure ExecCPM; inline;
      procedure ExecCb; inline;
      procedure ExecCbBITreg; inline;
      procedure ExecCbBITres; inline;
      procedure ExecCbBITresorset(_set: boolean); inline;
      procedure ExecCbBITset; inline;
      procedure ExecCbShiftRotate; inline;
      procedure ExecDAA; inline;
      procedure ExecDd; inline;
      procedure ExecDdFdADCAIQpdind; inline;
      procedure ExecDdFdADDAIQpdind; inline;
      procedure ExecDdFdADDIQBC; inline;
      procedure ExecDdFdADDIQDE; inline;
      procedure ExecDdFdADDIQIQ; inline;
      procedure ExecDdFdADDIQSP; inline;
      procedure ExecDdFdANDAIQpdind; inline;
      procedure ExecDdFdCb; inline;
      procedure ExecDdFdCbBITIQpdind; inline;
      procedure ExecDdFdCbRESIQpdind; inline;
      procedure ExecDdFdCbSETIQpdind; inline;
      procedure ExecDdFdCbShiftRotate; inline;
      procedure ExecDdFdCPAIQpdind; inline;
      procedure ExecDdFdDECIQ; inline;
      procedure ExecDdFdDECIQpdind; inline;
      procedure ExecDdFdEXSPindIQ; inline;
      procedure ExecDdFdINCIQ; inline;
      procedure ExecDdFdINCIQpdind; inline;
      procedure ExecDdFdJPIQind; inline;
      procedure ExecDdFdLDaddrIQ; inline;
      procedure ExecDdFdLDIQaddr; inline;
      procedure ExecDdFdLDIQimm; inline;
      procedure ExecDdFdLDIQpdindimm; inline;
      procedure ExecDdFdLDIQpdindr8; inline;
      procedure ExecDdFdLDr8IQpdind; inline;
      procedure ExecDdFdLDSPIQ; inline;
      procedure ExecDdFdORAIQpdind; inline;
      procedure ExecDdFdPOPIQ; inline;
      procedure ExecDdFdPUSHIQ; inline;
      procedure ExecDdFdSBCAIQpdind; inline;
      procedure ExecDdFdSUBAIQpdind; inline;
      procedure ExecDdFdXORAIQpdind; inline;
      procedure ExecDEC8(_m: PByte; _states: integer);
      procedure ExecDECA; inline;
      procedure ExecDECB; inline;
      procedure ExecDECBC; inline;
      procedure ExecDECC; inline;
      procedure ExecDECD; inline;
      procedure ExecDECDE; inline;
      procedure ExecDECE; inline;
      procedure ExecDECH; inline;
      procedure ExecDECHL; inline;
      procedure ExecDECL; inline;
      procedure ExecDECM; inline;
      procedure ExecDECSP; inline;
      procedure ExecDI; inline;
      procedure ExecDJNZ; inline;
      procedure ExecEd; inline;
      procedure ExecEdADCHLBC; inline;
      procedure ExecEdADCHLDE; inline;
      procedure ExecEdADCHLHL; inline;
      procedure ExecEdADCHLSP; inline;
      procedure ExecEdCPD; inline;
      procedure ExecEdCPDR; inline;
      procedure ExecEdCPI; inline;
      procedure ExecEdCPIR; inline;
      procedure ExecEdIM0; inline;
      procedure ExecEdIM1; inline;
      procedure ExecEdIM2; inline;
      procedure ExecEdINACind; inline;
      procedure ExecEdINBCind; inline;
      procedure ExecEdINCCind; inline;
      procedure ExecEdIND; inline;
      procedure ExecEdINDCind; inline;
      procedure ExecEdINDR; inline;
      procedure ExecEdINECind; inline;
      procedure ExecEdINHCind; inline;
      procedure ExecEdINI; inline;
      procedure ExecEdINIR; inline;
      procedure ExecEdINLCind; inline;
      procedure ExecEdINFCind; inline;
      procedure ExecEdLDAI; inline;
      procedure ExecEdLDAR; inline;
      procedure ExecEdLDBCind; inline;
      procedure ExecEdLDDEind; inline;
      procedure ExecEdLDHLind; inline;
      procedure ExecEdLDD; inline;
      procedure ExecEdLDDR; inline;
      procedure ExecEdLDI; inline;
      procedure ExecEdLDIA; inline;
      procedure ExecEdLDIR; inline;
      procedure ExecEdLDindBC; inline;
      procedure ExecEdLDindDE; inline;
      procedure ExecEdLDindHL; inline;
      procedure ExecEdLDindSP; inline;
      procedure ExecEdLDRA; inline;
      procedure ExecEdLDSPind; inline;
      procedure ExecEdNEG; inline;
      procedure ExecEdOUTCindA; inline;
      procedure ExecEdOUTCindB; inline;
      procedure ExecEdOUTCindC; inline;
      procedure ExecEdOUTCindD; inline;
      procedure ExecEdOUTCindE; inline;
      procedure ExecEdOUTCindH; inline;
      procedure ExecEdOUTCindL; inline;
      procedure ExecEdOUTCindZ; inline;
      procedure ExecEdOUTD; inline;
      procedure ExecEdOTDR; inline;
      procedure ExecEdOUTI; inline;
      procedure ExecEdOTIR; inline;
      procedure ExecEdRETI; inline;
      procedure ExecEdRETN; inline;
      procedure ExecEdRLD; inline;
      procedure ExecEdRRD; inline;
      procedure ExecEdSBCHLBC; inline;
      procedure ExecEdSBCHLDE; inline;
      procedure ExecEdSBCHLHL; inline;
      procedure ExecEdSBCHLSP; inline;
      procedure ExecEI; inline;
      procedure ExecEXAFAF_; inline;
      procedure ExecEXDEHL; inline;
      procedure ExecEXSPindHL; inline;
      procedure ExecEXX; inline;
      procedure ExecFd; inline;
      procedure ExecHALT; inline;
      procedure ExecINAport; inline;
      procedure ExecINC8(_m: PByte; _states: integer);
      procedure ExecINCA; inline;
      procedure ExecINCB; inline;
      procedure ExecINCBC; inline;
      procedure ExecINCC; inline;
      procedure ExecINCD; inline;
      procedure ExecINCDE; inline;
      procedure ExecINCE; inline;
      procedure ExecINCH; inline;
      procedure ExecINCHL; inline;
      procedure ExecINCL; inline;
      procedure ExecINCM; inline;
      procedure ExecINCSP; inline;
      procedure ExecJPC; inline;
      procedure ExecJPHLind; inline;
      procedure ExecJPM; inline;
      procedure ExecJPNC; inline;
      procedure ExecJPNZ; inline;
      procedure ExecJPP;
      procedure ExecJPPE; inline;
      procedure ExecJPPO; inline;
      procedure ExecJPZ; inline;
      procedure ExecJPabs; inline;
      procedure ExecJPcond(_mask, _required: byte); inline;
      procedure ExecJR; inline;
      procedure ExecJRC; inline;
      procedure ExecJRNC; inline;
      procedure ExecJRNZ; inline;
      procedure ExecJRZ; inline;
      procedure ExecJRcond(_mask, _required: byte); inline;
      procedure ExecLDAA; inline;
      procedure ExecLDAB; inline;
      procedure ExecLDABCind; inline;
      procedure ExecLDAC; inline;
      procedure ExecLDAD; inline;
      procedure ExecLDADEind; inline;
      procedure ExecLDAE; inline;
      procedure ExecLDAH; inline;
      procedure ExecLDAL; inline;
      procedure ExecLDAM; inline;
      procedure ExecLDAaddr; inline;
      procedure ExecLDAimm; inline;
      procedure ExecLDBA; inline;
      procedure ExecLDBB; inline;
      procedure ExecLDBC; inline;
      procedure ExecLDBCimm; inline;
      procedure ExecLDBCindA; inline;
      procedure ExecLDBD; inline;
      procedure ExecLDBE; inline;
      procedure ExecLDBH; inline;
      procedure ExecLDBL; inline;
      procedure ExecLDBM; inline;
      procedure ExecLDBimm; inline;
      procedure ExecLDCA; inline;
      procedure ExecLDCB; inline;
      procedure ExecLDCC; inline;
      procedure ExecLDCD; inline;
      procedure ExecLDCE; inline;
      procedure ExecLDCH; inline;
      procedure ExecLDCL; inline;
      procedure ExecLDCM; inline;
      procedure ExecLDCimm; inline;
      procedure ExecLDDA; inline;
      procedure ExecLDDB; inline;
      procedure ExecLDDC; inline;
      procedure ExecLDDD; inline;
      procedure ExecLDDE; inline;
      procedure ExecLDDEimm; inline;
      procedure ExecLDDEindA; inline;
      procedure ExecLDDH; inline;
      procedure ExecLDDL; inline;
      procedure ExecLDDM; inline;
      procedure ExecLDDimm; inline;
      procedure ExecLDEA; inline;
      procedure ExecLDEB; inline;
      procedure ExecLDEC; inline;
      procedure ExecLDED; inline;
      procedure ExecLDEE; inline;
      procedure ExecLDEH; inline;
      procedure ExecLDEL; inline;
      procedure ExecLDEM; inline;
      procedure ExecLDEimm; inline;
      procedure ExecLDHA; inline;
      procedure ExecLDHB; inline;
      procedure ExecLDHC; inline;
      procedure ExecLDHD; inline;
      procedure ExecLDHE; inline;
      procedure ExecLDHH; inline;
      procedure ExecLDHL; inline;
      procedure ExecLDHLaddr; inline;
      procedure ExecLDHLimm; inline;
      procedure ExecLDHLindimm; inline;
      procedure ExecLDHM; inline;
      procedure ExecLDHimm; inline;
      procedure ExecLDLA; inline;
      procedure ExecLDLB; inline;
      procedure ExecLDLC; inline;
      procedure ExecLDLD; inline;
      procedure ExecLDLE; inline;
      procedure ExecLDLH; inline;
      procedure ExecLDLL; inline;
      procedure ExecLDLM; inline;
      procedure ExecLDLimm; inline;
      procedure ExecLDMA; inline;
      procedure ExecLDMB; inline;
      procedure ExecLDMC; inline;
      procedure ExecLDMD; inline;
      procedure ExecLDME; inline;
      procedure ExecLDMH; inline;
      procedure ExecLDML; inline;
      procedure ExecLDSPHL; inline;
      procedure ExecLDSPimm; inline;
      procedure ExecLDaddrA; inline;
      procedure ExecLDaddrHL; inline;
      procedure ExecNOP; inline;
      procedure ExecORA; inline;
      procedure ExecORB; inline;
      procedure ExecORC; inline;
      procedure ExecORD; inline;
      procedure ExecORE; inline;
      procedure ExecORH; inline;
      procedure ExecORimm; inline;
      procedure ExecORL; inline;
      procedure ExecORM; inline;
      procedure ExecOUTportA; inline;
      procedure ExecPOPAF; inline;
      procedure ExecPOPBC; inline;
      procedure ExecPOPDE; inline;
      procedure ExecPOPHL; inline;
      procedure ExecPUSHAF; inline;
      procedure ExecPUSHBC; inline;
      procedure ExecPUSHDE; inline;
      procedure ExecPUSHHL; inline;
      procedure ExecRET; inline;
      procedure ExecRETC; inline;
      procedure ExecRETM; inline;
      procedure ExecRETNC; inline;
      procedure ExecRETNZ; inline;
      procedure ExecRETP; inline;
      procedure ExecRETPE; inline;
      procedure ExecRETPO; inline;
      procedure ExecRETZ; inline;
      procedure ExecRETcond(_mask, _required: byte); inline;
      procedure ExecRLA; inline;
      procedure ExecRLCA; inline;
      procedure ExecRRA; inline;
      procedure ExecRRCA; inline;
      procedure ExecRST(_addr: byte); inline;
      procedure ExecRST00; inline;
      procedure ExecRST08; inline;
      procedure ExecRST10; inline;
      procedure ExecRST18; inline;
      procedure ExecRST20; inline;
      procedure ExecRST28; inline;
      procedure ExecRST30; inline;
      procedure ExecRST38; inline;
      procedure ExecSBCA; inline;
      procedure ExecSBCB; inline;
      procedure ExecSBCC; inline;
      procedure ExecSBCD; inline;
      procedure ExecSBCE; inline;
      procedure ExecSBCH; inline;
      procedure ExecSBCimm; inline;
      procedure ExecSBCL; inline;
      procedure ExecSBCM; inline;
      procedure ExecSCF; inline;
      procedure ExecShiftRotate(p: PByte; style: byte);
      procedure ExecSUBA; inline;
      procedure ExecSUBB; inline;
      procedure ExecSUBC; inline;
      procedure ExecSUBD; inline;
      procedure ExecSUBE; inline;
      procedure ExecSUBH; inline;
      procedure ExecSUBHLr16(_w: Word; _dosbc: boolean = False); inline;
      procedure ExecSUBimm; inline;
      procedure ExecSUBL; inline;
      procedure ExecSUBM; inline;
      procedure ExecSub(_b: byte; _states: integer; _save: boolean = True; _dosbc: boolean = False); { inline; }
      procedure ExecXORA; inline;
      procedure ExecXORB; inline;
      procedure ExecXORC; inline;
      procedure ExecXORD; inline;
      procedure ExecXORE; inline;
      procedure ExecXORH; inline;
      procedure ExecXORimm; inline;
      procedure ExecXORL; inline;
      procedure ExecXORM; inline;
      procedure Init;
      procedure Interrupt(_vec: byte);    // Perform interrupt, use vector
      procedure ProcessPortOut(_port, _byte: byte);
      procedure PushWord(_word: Word); inline;
      procedure SetCPUspeed(_speed: int64);
      procedure SetNegZero; inline;
      procedure SetOnTransmitA(_proc: TSIOtransmitFunc);
      procedure SetPCrelative(_b: byte); inline;
      procedure SetProcessorState(_ps: TProcessorState);
      procedure SetLogicalflags(_seth: boolean); inline;
    protected
      procedure Execute; override;
    public
      // Pointers 16 bit dest
      pregAF:  PWord;
      pregBC:  PWord;
      pregDE:  PWord;
      pregHL:  PWord;
      pregAF_: PWord;
      pregBC_: PWord;
      pregDE_: PWord;
      pregHL_: PWord;
      pregIR:  PWord;
      pregIX:  PWord;
      pregIY:  PWord;
      pregSP:  PWord;
      pregPC:  PWord;
      pregIXY: PWord; // Will point to IX or IY depending on prefix
      // Pointers 8 bit dest
      pregA:   PByte;
      pregB:   PByte;
      pregC:   PByte;
      pregD:   PByte;
      pregE:   PByte;
      pregH:   PByte;
      pregL:   PByte;
      pregF:   PByte;
      pregI:   PByte;
      pregR:   PByte;
      pregIXH: PByte;
      pregIXL: PByte;
      pregIYH: PByte;
      pregIYL: PByte;
      pregSPH: PByte;
      pregSPL: PByte;
      pregPCH: PByte;
      pregPCL: PByte;
      pregIntE: PByte;
      pregIntM: PByte;
      Idle:     boolean;
      SavedPC: Word;
      SIO:      TSIO;
      constructor Create;
      destructor Destroy; override;
      procedure BreakpointsClearAll;
      procedure ChannelReceiveA(_byte: byte);
      function  ExecuteOneInst: boolean;     // Execute one instruction
//    function  ExecuteOver: boolean;     // Execute over the next instruction
      procedure ExecuteRun;
      procedure ExecuteStepInto;          // Execute one step
      procedure ExecuteStepOver;          // Step over next instruction
      procedure ExecuteStop;              // Stop processor if it's running
      procedure Reset(_initram: boolean = True); // Initialise, defaults to COLD
      procedure ReadFromStream(_strm: TStream; _start, _length: integer);
      procedure ReadFromXml(doc: TXMLDocument);
      procedure WaitForStop;
      procedure WriteToXml(doc: TXMLDocument);
      procedure WriteRAM(_addr: word; _data: byte);
      property AllowUndocumented: boolean read FAllowUndocumented write FAllowUndocumented;
      property CFlash: TCompactFlashInterface read FCFlash;
      property CPUspeed: int64 read cpu_speed write SetCPUspeed;
      property ErrorFlag: TErrorFlags read error_flag;
      property ErrorString: string read FErrorString;
      property OnStateChange: TStateChangeProc read FOnStateChange write FOnStateChange;
      property OnTransmitA: TSIOtransmitFunc read FOnTransmitA write SetOnTransmitA;
      property PC: Word read regset.registers[regPC] write regset.registers[regPC];
      property PerfMHz: double read GetPerfMHz;
      property PerfMIPS: double read GetPerfMIPS;
      property ProcessorState: TProcessorState read FProcessorState write SetProcessorState;
      property RAM: TRAMarray read ramarray write ramarray;
      property RegisterSet: TRegisterSet read GetRegisterSet;
      property TStates: int64 read t_states write t_states;
  end;





{$IFDEF EXEC_TRACE}
var
  exec_trace_log: array[0..EXEC_TRACE_SIZE-1] of TRegisterSet;
{$ENDIF}


implementation

uses
  uglobals, uxml;


constructor TProcessor.Create;
begin
  inherited Create(True);
  Idle := True;
  cpu_speed := 4000000; // Default to 4MHz device
  FAllowUndocumented := True;  // Allow undocumented instructions
  FreeOnTerminate := True;
  FProcessorState := psNone;
  BreakpointsClearAll;
  // Set up pointers to 16 bit registers
  pregAF  := @regset.registers[regAF];
  pregBC  := @regset.registers[regBC];
  pregDE  := @regset.registers[regDE];
  pregHL  := @regset.registers[regHL];
  pregAF_ := @regset.registers[regAF_];
  pregBC_ := @regset.registers[regBC_];
  pregDE_ := @regset.registers[regDE_];
  pregHL_ := @regset.registers[regHL_];
  pregIR  := @regset.registers[regIR];
  pregIX  := @regset.registers[regIX];
  pregIY  := @regset.registers[regIY];
  pregSP  := @regset.registers[regSP];
  pregPC  := @regset.registers[regPC];
  // Set up pointers to 8 bit registers
  pregA   := PByte(@regset.registers[regAF]) + 1;
  pregB   := PByte(@regset.registers[regBC]) + 1;
  pregC   := PByte(@regset.registers[regBC]);
  pregD   := PByte(@regset.registers[regDE]) + 1;
  pregE   := PByte(@regset.registers[regDE]);
  pregH   := PByte(@regset.registers[regHL]) + 1;
  pregL   := PByte(@regset.registers[regHL]);
  pregF   := PByte(@regset.registers[regAF]);
  pregI   := PByte(@regset.registers[regIR]) + 1;
  pregR   := PByte(@regset.registers[regIR]);
  pregIXH := PByte(@regset.registers[regIX]) + 1;
  pregIXL := PByte(@regset.registers[regIX]);
  pregIYH := PByte(@regset.registers[regIY]) + 1;
  pregIYL := PByte(@regset.registers[regIY]);
  pregSPH := PByte(@regset.registers[regSP]) + 1;
  pregSPL := PByte(@regset.registers[regSP]);
  pregPCH := PByte(@regset.registers[regPC]) + 1;
  pregPCL := PByte(@regset.registers[regPC]);
  pregIntE := @regset.int_enabled;
  pregIntM := @regset.int_mode;
  // Set up SIO
  SIO := TSIO.Create;
  SIO.OnInterrupt := @Interrupt;
  SIO.OnCanInterrupt := @CanInterrupt;
  // Set up CF card
  FCFlash := TCompactFlashInterface.Create;
  // Finally, initialise all instruction links, RAM, etc.
  Init;
end;

destructor TProcessor.Destroy;
begin
  FreeAndNil(FCFlash);
  FreeAndNil(SIO);
  inherited Destroy;
end;

function TProcessor.Flag(_flagmask: byte): boolean; inline;
begin
  if (pregF^ and _flagmask) <> 0 then
    Result := True
  else
    Result := False;
end;

function TProcessor.FlagNegative: boolean; inline;
begin
  Result := Flag(FLAG_NEGATIVE);
end;

function TProcessor.FlagZero: boolean; inline;
begin
  Result := Flag(FLAG_ZERO);
end;

function TProcessor.FlagHalfCarry: boolean; inline;
begin
  Result := Flag(FLAG_HALFCARRY);
end;

function TProcessor.FlagPV: boolean; inline;
begin
  Result := Flag(FLAG_PV);
end;

function TProcessor.FlagSubtract: boolean; inline;
begin
  Result := Flag(FLAG_SUBTRACT);
end;

function TProcessor.FlagCarry: boolean; inline;
begin
  Result := Flag(FLAG_CARRY);
end;

procedure TProcessor.SetFlag(_flagmask: byte; _value: boolean); inline;
begin
  if _value then
    pregF^ := pregF^ or _flagmask
  else
    pregF^ := pregF^ and (_flagmask xor $FF);
end;

procedure TProcessor.SetFlagNegative(_value: boolean); inline;
begin
  SetFlag(FLAG_NEGATIVE,_value);
end;

procedure TProcessor.SetFlagZero(_value: boolean); inline;
begin
  SetFlag(FLAG_ZERO,_value);
end;

procedure TProcessor.SetFlagHalfCarry(_value: boolean); inline;
begin
  SetFlag(FLAG_HALFCARRY,_value);
end;

procedure TProcessor.SetFlagPV(_value: boolean); inline;
begin
  SetFlag(FLAG_PV,_value);
end;

procedure TProcessor.SetFlagSubtract(_value: boolean); inline;
begin
  SetFlag(FLAG_SUBTRACT,_value);
end;

procedure TProcessor.SetFlagCarry(_value: boolean); inline;
begin
  SetFlag(FLAG_CARRY,_value);
end;

procedure TProcessor.SetFlagNZ53;
begin // Sets the Negative, Zero, X5 and X3 flags based on contents of A
  pregF^ := pregF^ and NOT_FLAG_NEGATIVE and NOT_FLAG_ZERO and NOT_FLAG_X5 and NOT_FLAG_X3;
  pregF^ := pregF^ or (pregA^ and (FLAG_NEGATIVE and FLAG_X5 and FLAG_X3));
  if pregA^ = 0 then
    SetFlagZero(True);
end;

{$RANGECHECKS OFF}  // Range checking is off so we can roll over registers

function TProcessor.Add16u8s(_word: Word; _byte: byte): Word;
begin
  if (_byte and $80) <> 0 then
    Result := _word - $100 + _byte
  else
    Result := _word + _byte;
end;

procedure TProcessor.BreakpointsClearAll;
var w: word;
begin
  for w in word do
    bparray[w] := [];
end;

procedure TProcessor.ChannelReceiveA(_byte: byte);
begin
  SIO.ChannelA.IncomingChar(_byte);
end;

procedure TProcessor.ClearTemporaryBreakpoints;
var w: Word;
begin
  for w in Word do
    bparray[w] := bparray[w] - [bpTemporary];
end;

procedure TProcessor.ExecAdd(_b: byte; _states: integer; _doadc: boolean = False); inline;
var _a,_c, _cf: byte;
    flags: byte;
begin
  _a    := pregA^;
  flags := pregF^;
  if _doadc and ((flags and FLAG_CARRY) <> 0) then
    _cf := 1
  else
    _cf := 0;
  _c := _a + _b + _cf;
  flags := (flags and NOT_FLAG_NEGATIVE) or (_c and FLAG_NEGATIVE);
  if _c = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;
  if (_b and $0F) + (_a and $0F) + _cf >= $10 then
    flags := flags or FLAG_HALFCARRY
  else
    flags := flags and NOT_FLAG_HALFCARRY;
  flags := flags and NOT_FLAG_PV; // Clear overflow for now
  if ((_a xor _b) and $80) = 0 then // check for alike signs
      if ((_a xor _c) and $80) <> $00 then
        flags := flags or FLAG_PV;
  flags := flags and NOT_FLAG_SUBTRACT;
  if (_b + _a + _cf) >= $100 then
    flags := flags or FLAG_CARRY
  else
    flags := flags and NOT_FLAG_CARRY;
  pregF^ := flags;
  pregA^ := _c;
  Inc(t_states,_states);
end;

procedure TProcessor.ExecADDA; inline;
begin
  ExecAdd(pregA^,4);
end;

procedure TProcessor.ExecADDB; inline;
begin
  ExecAdd(pregB^,4);
end;

procedure TProcessor.ExecADDC; inline;
begin
  ExecAdd(pregC^,4);
end;

procedure TProcessor.ExecADDD; inline;
begin
  ExecAdd(pregD^,4);
end;

procedure TProcessor.ExecADDE; inline;
begin
  ExecAdd(pregE^,4);
end;

procedure TProcessor.ExecADDH; inline;
begin
  ExecAdd(pregH^,4);
end;

procedure TProcessor.ExecADDHLr16(_w: Word; _doadc: boolean = False); inline;
var _states: integer;
begin
  _states := 11;
  if _doadc then
    _states := 15;
  ExecADDr16r16(pregHL,_w,_states,_doadc);
end;

procedure TProcessor.ExecADDHLBC; inline;
begin
  ExecAddHLr16(pregBC^);
end;

procedure TProcessor.ExecADDHLDE; inline;
begin
  ExecAddHLr16(pregDE^);
end;

procedure TProcessor.ExecADDHLHL; inline;
begin
  ExecAddHLr16(pregHL^);
end;

procedure TProcessor.ExecADDHLSP; inline;
begin
  ExecAddHLr16(pregSP^);
end;

procedure TProcessor.ExecADDimm; inline;
begin
  ExecAdd(Fetch8,7);
end;

procedure TProcessor.ExecADDL; inline;
begin
  ExecAdd(pregL^,4);
end;

procedure TProcessor.ExecADDM; inline;
begin
  ExecAdd(ramarray[pregHL^],7);
end;

procedure TProcessor.ExecADDr16r16(_pdst: PWord; _w: Word; _states: integer; _doadc: boolean = False); {inline;}
var _cf: Word;
    flags: byte;
    _src: Word;
    _dst: Word;
begin
  _src := _pdst^;
  flags := pregF^;
  if _doadc and ((flags and FLAG_CARRY) <> 0) then
    _cf := 1
  else
    _cf := 0;
  if _doadc then
    flags := flags and NOT_FLAG_SUBTRACT and NOT_FLAG_HALFCARRY and NOT_FLAG_CARRY and NOT_FLAG_ZERO and NOT_FLAG_PV and NOT_FLAG_NEGATIVE
  else
    flags := flags and NOT_FLAG_SUBTRACT and NOT_FLAG_HALFCARRY and NOT_FLAG_CARRY;
  if ((integer(_src and $0FFF) + integer(_w and $0FFF) + _cf)) and $FFFFF000 >= $1000 then
    flags := flags or FLAG_HALFCARRY;
  if integer(_src) + integer(_w) + _cf >= $10000 then
    flags := flags or FLAG_CARRY;
  _dst := _src + _w + _cf;
  if _doadc then
    begin // Don't do this stuff for ADD r16,r16
      if ((_src xor _w) and $8000) = 0 then // check for alike signs
          if ((_src xor _dst) and $8000) <> $00 then
            flags := flags or FLAG_PV;
      if (_dst and $8000) <> 0 then
        flags := flags or FLAG_NEGATIVE;
      if (_dst = 0) then
        flags := flags or FLAG_ZERO;
    end;
  _pdst^ := _dst;
  pregF^ := Flags;
  Inc(t_states,_states);
end;

procedure TProcessor.ExecADCA; inline;
begin
  ExecADD(pregA^,4,True);
end;

procedure TProcessor.ExecADCB; inline;
begin
  ExecADD(pregB^,4,True);
end;

procedure TProcessor.ExecADCC; inline;
begin
  ExecADD(pregC^,4,True);
end;

procedure TProcessor.ExecADCD; inline;
begin
  ExecADD(pregD^,4,True);
end;

procedure TProcessor.ExecADCE; inline;
begin
  ExecADD(pregE^,4,True);
end;

procedure TProcessor.ExecADCH; inline;
begin
  ExecADD(pregH^,4,True);
end;

procedure TProcessor.ExecADCimm; inline;
begin
  ExecADD(Fetch8,7,True);
end;

procedure TProcessor.ExecADCL; inline;
begin
  ExecADD(pregL^,4,True);
end;

procedure TProcessor.ExecADCM; inline;
begin
  ExecADD(ramarray[pregHL^],7,True);
end;

procedure TProcessor.ExecANDimm; inline;
begin
  pregA^ := pregA^ and Fetch8;
  SetLogicalFlags(True);
  Inc(t_states,7);
end;

procedure TProcessor.ExecANDA; inline;
begin
  pregA^ := pregA^ and pregA^;
  SetLogicalFlags(True);
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDB; inline;
begin
  pregA^ := pregA^ and pregB^;
  SetLogicalFlags(True);
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDC; inline;
begin
  pregA^ := pregA^ and pregC^;
  SetLogicalFlags(True);
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDD; inline;
begin
  pregA^ := pregA^ and pregD^;
  SetLogicalFlags(True);
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDE; inline;
begin
  pregA^ := pregA^ and pregE^;
  SetLogicalFlags(True);
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDH; inline;
begin
  pregA^ := pregA^ and pregH^;
  SetLogicalFlags(True);
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDL; inline;
begin
  pregA^ := pregA^ and pregL^;
  SetLogicalFlags(True);
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDM; inline;
begin
  pregA^ := pregA^ and ramarray[pregHL^];
  SetLogicalFlags(True);
  Inc(t_states,7);
end;

procedure TProcessor.ExecCALLabs; inline;
var addr: Word;
begin
  addr := Fetch16;
  PushWord(pregPC^);
  pregPC^ := addr;
  Inc(t_states,17);
end;

procedure TProcessor.ExecCALLcond(_mask, _required: byte); inline;
var addr: Word;
begin
  addr := Fetch16;
  if pregF^ and _mask = _required then
    begin // Succeeded
      PushWord(pregPC^);
      pregPC^ := addr;
      Inc(t_states,17);
    end
  else
    Inc(t_states,10); // Failed
end;

procedure TProcessor.ExecCALLC; inline;
begin
  ExecCALLCond(FLAG_CARRY,FLAG_CARRY);
end;

procedure TProcessor.ExecCALLM; inline;
begin
  ExecCALLCond(FLAG_NEGATIVE,FLAG_NEGATIVE);
end;

procedure TProcessor.ExecCALLNC; inline;
begin
  ExecCALLCond(FLAG_CARRY,0);
end;

procedure TProcessor.ExecCALLNZ; inline;
begin
  ExecCALLCond(FLAG_ZERO,0);
end;

procedure TProcessor.ExecCALLP; inline;
begin
  ExecCALLCond(FLAG_NEGATIVE,0);
end;

procedure TProcessor.ExecCALLPE; inline;
begin
  ExecCALLCond(FLAG_PV,FLAG_PV);
end;

procedure TProcessor.ExecCALLPO; inline;
begin
  ExecCALLCond(FLAG_PV,0);
end;

procedure TProcessor.ExecCALLZ; inline;
begin
  ExecCALLCond(FLAG_ZERO,FLAG_ZERO);
end;

procedure TProcessor.ExecCCF; inline;
begin
  pregF^ := pregF^ and NOT_FLAG_SUBTRACT and NOT_FLAG_HALFCARRY;
  pregF^ := pregF^ xor FLAG_CARRY;
  Inc(t_states,4);
end;

procedure TProcessor.ExecCPA; inline;
begin
  ExecSub(pregA^,4,False);
end;

procedure TProcessor.ExecCPB; inline;
begin
  ExecSub(pregB^,4,False);
end;

procedure TProcessor.ExecCPC; inline;
begin
  ExecSub(pregC^,4,False);
end;

procedure TProcessor.ExecCPD; inline;
begin
  ExecSub(pregD^,4,False);
end;

procedure TProcessor.ExecCPE; inline;
begin
  ExecSub(pregE^,4,False);
end;

procedure TProcessor.ExecCPimm; inline;
begin
  ExecSub(Fetch8,7,False); // Subtract immediate from a, throw away results
end;

procedure TProcessor.ExecCPH; inline;
begin
  ExecSub(pregH^,4,False);
end;

procedure TProcessor.ExecCPL; inline;
begin
  ExecSub(pregL^,4,False);
end;

procedure TProcessor.ExecCPL2; inline;
begin  // Complement accumulator
  pregA^ := pregA^ xor $FF;
  pregF^ := pregF^ or FLAG_HALFCARRY or FLAG_SUBTRACT;
  Inc(t_states,4);
end;

procedure TProcessor.ExecCPM; inline;
begin
  ExecSub(ramarray[pregHL^],7,False);
end;

{
procedure TProcessor.ExecDAA; inline;
type
  TDAAflags = (dfMSB0_2,dfMSB0_3,dfMSB0_8,dfMSB0_9,dfMSB6_F,dfMSB7_F,dfMSB9_F,
               dfMSBA_F, dfLSB0_3, dfLSB0_9,dfLSB6_F,dfLSBA_F);
  TDAAset = set of TDAAflags;
var
  myset: TDAAset;
  nyb1:  byte;
  nyb2:  byte;
  iflag: byte;

  procedure MakeRes(_add: byte; _c: byte);
  begin
    pregA^ := pregA^ + _add;
    pregF^ := (pregF^ and NOT_FLAG_CARRY) or _c;
    pregF^ := pregF^ and NOT_FLAG_HALFCARRY;
    if (_add and $0F) <> 0 then
      pregF^ := pregF^ or FLAG_HALFCARRY;
  end;

begin
  iflag := ((pregF^ and FLAG_HALFCARRY) shr 2) or
            (pregF^ and (FLAG_SUBTRACT or FLAG_CARRY));
  myset := [];
  nyb1 := pregA^ and $F0;
  nyb2 := pregA^ and $0F;
  if (nyb1 < $30) then myset := myset + [dfMSB0_2];
  if (nyb1 < $40) then myset := myset + [dfMSB0_3];
  if (nyb1 < $90) then myset := myset + [dfMSB0_8];
  if (nyb1 < $A0) then myset := myset + [dfMSB0_9];
  if (nyb1 > $50) then myset := myset + [dfMSB6_F];
  if (nyb1 > $60) then myset := myset + [dfMSB7_F];
  if (nyb1 > $80) then myset := myset + [dfMSB9_F];
  if (nyb1 > $90) then myset := myset + [dfMSBA_F];
  if (nyb2 < $04) then myset := myset + [dfLSB0_3];
  if (nyb2 < $0A) then myset := myset + [dfLSB0_9];
  if (nyb2 > $05) then myset := myset + [dfLSB6_F];
  if (nyb2 > $09) then myset := myset + [dfLSBA_F];
  if      (iflag = $00) and (dfMSB0_8 in myset) and (dfLSBA_F in myset) then MakeRes($06,0)
  else if (iflag = $00) and (dfMSB0_9 in myset) and (dfLSB0_9 in myset) then MakeRes($00,0)
  else if (iflag = $00) and (dfMSB9_F in myset) and (dfLSBA_F in myset) then MakeRes($66,1)
  else if (iflag = $00) and (dfMSBA_F in myset) and (dfLSB0_9 in myset) then MakeRes($60,1)
  else if (iflag = $01) and (dfMSB0_2 in myset) and (dfLSB0_9 in myset) then MakeRes($60,1)
  else if (iflag = $01) and (dfMSB0_2 in myset) and (dfLSBA_F in myset) then MakeRes($66,1)
  else if (iflag = $02) and (dfMSB0_9 in myset) and (dfLSB0_9 in myset) then MakeRes($00,0)
  else if (iflag = $03) and (dfMSB0_9 in myset) and (dfLSB0_9 in myset) then MakeRes($00,0)
  else if (iflag = $04) and (dfMSB0_9 in myset) and (dfLSB0_3 in myset) then MakeRes($06,0)
  else if (iflag = $04) and (dfMSBA_F in myset) and (dfLSB0_3 in myset) then MakeRes($66,1)
  else if (iflag = $05) and (dfMSB0_3 in myset) and (dfLSB0_3 in myset) then MakeRes($66,1)
  else if (iflag = $06) and (dfMSB0_8 in myset) and (dfLSB6_F in myset) then MakeRes($FA,0)
  else if (iflag = $07) and (dfMSB0_8 in myset) and (dfLSB6_F in myset) then MakeRes($FA,0);
  pregF^ := pregF^ and NOT_FLAG_ZERO and NOT_FLAG_NEGATIVE;
  if pregA^ = 0 then
    pregF^ := pregF^ or FLAG_ZERO;
  pregF^ := pregF^ and NOT_FLAG_PV;
  pregF^ := pregF^ or parity_table[pregA^];
  if (pregA^ and $80) <> 0 then
    pregF^ := pregF^ or FLAG_NEGATIVE;
  Inc(t_states,4);
end;
}

procedure TProcessor.ExecDAA; inline;
var t: integer;
    multiplier: integer;
begin
  // Algorithm from StackOverflow article by Rui F Ribeiro:
  // https://stackoverflow.com/questions/8119577/z80-daa-instruction

  t := 0;
  if FlagHalfCarry or ((pregA^ and $0F) > $09) then
    Inc(t);
  if FlagCarry or (pregA^ > $99) then
    begin
      t := t + 2;
      SetFlagCarry(True);
    end;

  // Build the final H flag
  if FlagSubtract and not FlagHalfCarry then
    SetFlagHalfCarry(False)
  else
    begin
      if FlagNegative and FlagHalfCarry then
        SetFlagHalfCarry((pregA^ and $0F) < 6)
      else
        SetFlagHalfCarry((pregA^ and $0F) >= $0A);
    end;

  // Adjust accumulator
  multiplier := 1;
  if FlagSubtract then multiplier := -1;
  case t of
    1: pregA^ := pregA^ + multiplier * $06;
    2: pregA^ := pregA^ + multiplier * $60;
    3: pregA^ := pregA^ + multiplier * $66;
  end;

  // Final tidy up of S, Z and P/V flags
  SetFlagNegative(pregA^ >= $80);
  SetFlagZero(pregA^ = $00);
  pregF^ := pregF^ or parity_table[pregA^];

  Inc(t_states,4);
end;

procedure TProcessor.ExecDEC8(_m: PByte; _states: integer);
begin
  pregF^ := pregF^ or FLAG_SUBTRACT; // Set N flag
  if (_m^ and $0F) = $00 then
    pregF^ := pregF^ or FLAG_HALFCARRY // Borrow from bit 4
  else
    pregF^ := pregF^ and NOT_FLAG_HALFCARRY;
  if _m^ = $80 then
    pregF^ := pregF^ or FLAG_PV
  else
    pregF^ := pregF^ and NOT_FLAG_PV;
  Dec(_m^);
  pregF^ := (pregF^ and $7F) or (_m^ and $80); // Set negative flag if reqd
  if _m^ = $00 then
    pregF^ := pregF^ or FLAG_ZERO
  else
    pregF^ := pregF^ and NOT_FLAG_ZERO;
  Inc(t_states,_states);
end;

procedure TProcessor.ExecDECA; inline;
begin
  ExecDec8(pregA,4);
end;

procedure TProcessor.ExecDECB; inline;
begin
  ExecDec8(pregB,4);
end;

procedure TProcessor.ExecDECBC; inline;
begin
  Dec(pregBC^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecDECC; inline;
begin
  ExecDec8(pregC,4);
end;

procedure TProcessor.ExecDECD; inline;
begin
  ExecDec8(pregD,4);
end;

procedure TProcessor.ExecDECDE; inline;
begin
  Dec(pregDE^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecDECE; inline;
begin
  ExecDec8(pregE,4);
end;

procedure TProcessor.ExecDECH; inline;
begin
  ExecDec8(pregH,4);
end;

procedure TProcessor.ExecDECHL; inline;
begin
  Dec(pregHL^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecDECL; inline;
begin
  ExecDec8(pregL,4);
end;

procedure TProcessor.ExecDECM; inline;
begin
  ExecDec8(@ramarray[pregHL^],11);
end;

procedure TProcessor.ExecDECSP; inline;
begin
  Dec(pregSP^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecDI; inline;
begin
  pregIntE^ := 0;
  Inc(t_states,4);
end;

procedure TProcessor.ExecDJNZ; inline;
var b: byte;
begin
  b := Fetch8;
  Dec(pregB^);
  if pregB^ = 0 then
    Inc(t_states,8)
  else
    begin
      SetPCrelative(b);
      Inc(t_states,13);
    end;
end;

procedure TProcessor.ExecEI; inline;
begin
  pregIntE^ := 3; // Allow this instruction and one more before enabling
  Inc(t_states,4);
end;

procedure TProcessor.ExecEXAFAF_; inline;
var w: Word;
begin
  w := pregAF^;
  pregAF^ := pregAF_^;
  pregAF_^ := w;
  Inc(t_states,4);
end;

procedure TProcessor.ExecEXDEHL; inline;
var w: Word;
begin
  w := pregDE^;
  pregDE^ := pregHL^;
  pregHL^ := w;
  Inc(t_states,4);
end;

procedure TProcessor.ExecEXSPindHL; inline;
var w: word;
    p: PWord;
begin
  p := PWord(@ramarray[pregSP^]);
  w := p^;
  p^ := pregHL^;
  pregHL^ := w;
  Inc(t_states,19);
end;

procedure TProcessor.ExecEXX; inline;
var w: word;
begin
  w := pregBC^;  pregBC^ := pregBC_^;  pregBC_^ := w;
  w := pregDE^;  pregBC^ := pregDE_^;  pregDE_^ := w;
  w := pregHL^;  pregBC^ := pregHL_^;  pregHL_^ := w;
  Inc(t_states,4);
end;

procedure TProcessor.ExecHALT; inline;
begin
  error_flag := error_flag + [efHalt];
  Inc(t_states,4);
end;

procedure TProcessor.ExecINAport; inline;
var port: byte;
begin
  // @@@@@ DO PORT HANDLING FUNCTIONS
  // e.g.  ProcessPortOut(port,af shr 8);
  port := Fetch8;
  pregA^ := ProcessPortIn(port);
  Inc(t_states,11);
end;

procedure TProcessor.ExecINC8(_m: PByte; _states: integer);
begin
  pregF^ := pregF^ and NOT_FLAG_SUBTRACT; // Clear N flag
  if (_m^ and $0F) = $0F then
    pregF^ := pregF^ or FLAG_HALFCARRY // Carry from bit 3
  else
    pregF^ := pregF^ and NOT_FLAG_HALFCARRY;
  if _m^ = $7F then
    pregF^ := pregF^ or FLAG_PV
  else
    pregF^ := pregF^ and NOT_FLAG_PV;
  Inc(_m^);
  pregF^ := (pregF^ and $7F) or (_m^ and $80); // Set negative flag if reqd
  if _m^ = $00 then
    pregF^ := pregF^ or FLAG_ZERO
  else
    pregF^ := pregF^ and NOT_FLAG_ZERO;
  Inc(t_states,_states);
end;

procedure TProcessor.ExecINCA; inline;
begin
  ExecINC8(pregA,4);
end;

procedure TProcessor.ExecINCB; inline;
begin
  ExecINC8(pregB,4);
end;

procedure TProcessor.ExecINCC; inline;
begin
  ExecINC8(pregC,4);
end;

procedure TProcessor.ExecINCD; inline;
begin
  ExecINC8(pregD,4);
end;

procedure TProcessor.ExecINCE; inline;
begin
  ExecINC8(pregE,4);
end;

procedure TProcessor.ExecINCH; inline;
begin
  ExecINC8(pregH,4);
end;

procedure TProcessor.ExecINCL; inline;
begin
  ExecINC8(pregL,4);
end;

procedure TProcessor.ExecINCM; inline;
begin
  ExecINC8(@ramarray[pregHL^],11);
end;

procedure TProcessor.ExecINCBC; inline;
begin
  Inc(pregBC^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecINCDE; inline;
begin
  Inc(pregDE^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecINCHL; inline;
begin
  Inc(pregHL^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecINCSP; inline;
begin
  Inc(pregSP^);
  Inc(t_states,6);
end;

procedure TProcessor.ExecJPabs; inline;
var addr: Word;
begin
  addr := Fetch16;
  pregPC^ := addr;
  Inc(t_states,10);
end;

procedure TProcessor.ExecJPC; inline;
begin
  ExecJPcond(FLAG_CARRY,FLAG_CARRY);
end;

procedure TProcessor.ExecJPcond(_mask, _required: byte); inline;
var addr: Word;
begin
  addr := Fetch16;
  if pregF^ and _mask = _required then
    pregPC^ := addr;
  Inc(t_states,10);
end;

procedure TProcessor.ExecJPHLind; inline;
begin
  pregPC^ := pregHL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecJPM; inline;
begin
  ExecJPcond(FLAG_NEGATIVE,FLAG_NEGATIVE);
end;

procedure TProcessor.ExecJPNC; inline;
begin
  ExecJPcond(FLAG_CARRY,0);
end;

procedure TProcessor.ExecJPNZ; inline;
begin
  ExecJPcond(FLAG_ZERO,0);
end;

procedure TProcessor.ExecJPP;
begin
  ExecJPcond(FLAG_NEGATIVE,0);
end;

procedure TProcessor.ExecJPPE; inline;
begin
  ExecJPcond(FLAG_PV,FLAG_PV);
end;

procedure TProcessor.ExecJPPO; inline;
begin
  ExecJPcond(FLAG_PV,0);
end;

procedure TProcessor.ExecJPZ; inline;
begin
  ExecJPcond(FLAG_ZERO,FLAG_ZERO);
end;

procedure TProcessor.ExecJR; inline;
var b: byte;
begin
  b := Fetch8;
  SetPCrelative(b);
  Inc(t_states,12);
end;

procedure TProcessor.ExecJRC; inline;
begin
  ExecJRcond(FLAG_CARRY,FLAG_CARRY);
end;

procedure TProcessor.ExecJRcond(_mask, _required: byte); inline;
var b: byte;
begin
  b := Fetch8;
  if pregF^ and _mask = _required then
    begin // Succeeded
      SetPCrelative(b);
      Inc(t_states,12);
    end
  else
    Inc(t_states,7); // Failed
end;

procedure TProcessor.ExecJRNC; inline;
begin
  ExecJRcond(FLAG_CARRY,0);
end;

procedure TProcessor.ExecJRNZ; inline;
begin
  ExecJRcond(FLAG_ZERO,0);
end;

procedure TProcessor.ExecJRZ; inline;
begin
  ExecJRcond(FLAG_ZERO,FLAG_ZERO);
end;

procedure TProcessor.ExecLDaddrA; inline;
var addr: Word;
begin
  addr := Fetch16;
  ramarray[addr] := pregA^;
  Inc(t_states,13);
end;

procedure TProcessor.ExecLDaddrHL; inline;
var addr: Word;
    paddr: PWord;
begin
  addr := Fetch16;
  paddr := PWord(@ramarray[addr]);
  paddr^ := pregHL^;
  Inc(t_states,16);
end;

procedure TProcessor.ExecLDAaddr; inline;
var addr: Word;
begin
  addr := Fetch16;
  pregA^ := ramarray[addr];
  Inc(t_states,13);
end;

procedure TProcessor.ExecLDAimm; inline;
begin
  pregA^ := Fetch8;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDAA; inline;
begin
  pregA^ := pregA^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDAB; inline;
begin
  pregA^ := pregB^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDABCind; inline;
begin
  pregA^ := ramarray[pregBC^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDAC; inline;
begin
  pregA^ := pregC^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDAD; inline;
begin
  pregA^ := pregD^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDADEind; inline;
begin
  pregA^ := ramarray[pregDE^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDAE; inline;
begin
  pregA^ := pregE^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDAH; inline;
begin
  pregA^ := pregH^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDAL; inline;
begin
  pregA^ := pregL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDAM; inline;
begin
  pregA^ := ramarray[pregHL^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDBA; inline;
begin
  pregB^ := pregA^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDBB; inline;
begin
  pregB^ := pregB^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDBC; inline;
begin
  pregB^ := pregC^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDBD; inline;
begin
  pregB^ := pregD^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDBE; inline;
begin
  pregB^ := pregE^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDBH; inline;
begin
  pregB^ := pregH^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDBL; inline;
begin
  pregB^ := pregL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDBM; inline;
begin
  pregB^ := ramarray[pregHL^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDCA; inline;
begin
  pregC^ := pregA^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDCB; inline;
begin
  pregC^ := pregB^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDCC; inline;
begin
  pregC^ := pregC^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDCD; inline;
begin
  pregC^ := pregD^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDCE; inline;
begin
  pregC^ := pregE^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDCH; inline;
begin
  pregC^ := pregH^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDCL; inline;
begin
  pregC^ := pregL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDCM; inline;
begin
  pregC^ := ramarray[pregHL^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDDA; inline;
begin
  pregD^ := pregA^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDDB; inline;
begin
  pregD^ := pregB^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDDC; inline;
begin
  pregD^ := pregC^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDDD; inline;
begin
  pregD^ := pregD^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDDE; inline;
begin
  pregD^ := pregE^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDDH; inline;
begin
  pregD^ := pregH^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDDL; inline;
begin
  pregD^ := pregL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDDM; inline;
begin
  pregD^ := ramarray[pregHL^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDEA; inline;
begin
  pregE^ := pregA^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDEB; inline;
begin
  pregE^ := pregB^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDEC; inline;
begin
  pregE^ := pregC^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDED; inline;
begin
  pregE^ := pregD^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDEE; inline;
begin
  pregE^ := pregE^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDEH; inline;
begin
  pregE^ := pregH^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDEL; inline;
begin
  pregE^ := pregL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDEM; inline;
begin
  pregE^ := ramarray[pregHL^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDHA; inline;
begin
  pregH^ := pregA^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDHB; inline;
begin
  pregH^ := pregB^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDHC; inline;
begin
  pregH^ := pregC^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDHD; inline;
begin
  pregH^ := pregD^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDHE; inline;
begin
  pregH^ := pregE^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDHH; inline;
begin
  pregH^ := pregH^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDHL; inline;
begin
  pregH^ := pregL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDHM; inline;
begin
  pregH^ := ramarray[pregHL^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDLA; inline;
begin
  pregL^ := pregA^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDLB; inline;
begin
  pregL^ := pregB^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDLC; inline;
begin
  pregL^ := pregC^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDLD; inline;
begin
  pregL^ := pregD^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDLE; inline;
begin
  pregL^ := pregE^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDLH; inline;
begin
  pregL^ := pregH^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDLL; inline;
begin
  pregL^ := pregL^;
  Inc(t_states,4);
end;

procedure TProcessor.ExecLDLM; inline;
begin
  pregL^ := ramarray[pregHL^];
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDBimm; inline;
begin
  pregB^ := Fetch8;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDBCimm; inline;
begin
  pregBC^ := Fetch16;
  Inc(t_states,10);
end;

procedure TProcessor.ExecLDBCindA; inline;
begin
  ramarray[pregBC^] := pregA^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDCimm; inline;
begin
  pregC^ := Fetch8;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDDimm; inline;
begin
  pregD^ := Fetch8;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDDEimm; inline;
begin
  pregDE^ := Fetch16;
  Inc(t_states,10);
end;

procedure TProcessor.ExecLDDEindA; inline;
begin
  ramarray[pregDE^] := pregA^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDEimm; inline;
begin
  pregE^ := Fetch8;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDHimm; inline;
begin
  pregH^ := Fetch8;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDHLaddr; inline;
var addr: Word;
    paddr: PWord;
begin
  addr := Fetch16;
  paddr := PWord(@ramarray[addr]);
  pregHL^ := paddr^;
  Inc(t_states,16);
end;

procedure TProcessor.ExecLDHLimm; inline;
begin
  pregHL^ := Fetch16;
  Inc(t_states,10);
end;

procedure TProcessor.ExecLDHLindimm; inline;
begin
  ramarray[pregHL^] := Fetch8;
  Inc(t_states,10);
end;

procedure TProcessor.ExecLDLimm; inline;
begin
  pregL^ := Fetch8;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDMA; inline;
begin
  ramarray[pregHL^] := pregA^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDMB; inline;
begin
  ramarray[pregHL^] := pregB^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDMC; inline;
begin
  ramarray[pregHL^] := pregC^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDMD; inline;
begin
  ramarray[pregHL^] := pregD^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDME; inline;
begin
  ramarray[pregHL^] := pregE^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDMH; inline;
begin
  ramarray[pregHL^] := pregH^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDML; inline;
begin
  ramarray[pregHL^] := pregL^;
  Inc(t_states,7);
end;

procedure TProcessor.ExecLDSPHL; inline;
begin
  pregSP^ := pregHL^;
  Inc(t_states,6);
end;

procedure TProcessor.ExecLDSPimm; inline;
begin
  pregSP^ := Fetch16;
  Inc(t_states,10);
end;

procedure TProcessor.ExecNOP; inline;
begin
  Inc(t_states,4);
end;

procedure TProcessor.ExecORA; inline;
begin
  pregA^ := pregA^ or pregA^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecORB; inline;
begin
  pregA^ := pregA^ or pregB^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecORC; inline;
begin
  pregA^ := pregA^ or pregC^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecORD; inline;
begin
  pregA^ := pregA^ or pregD^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecORE; inline;
begin
  pregA^ := pregA^ or pregE^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecORH; inline;
begin
  pregA^ := pregA^ or pregH^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecORimm; inline;
begin
  pregA^ := pregA^ or Fetch8;
  SetLogicalFlags(False);
  Inc(t_states,7);
end;

procedure TProcessor.ExecORL; inline;
begin
  pregA^ := pregA^ or pregL^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecORM; inline;
begin
  pregA^ := pregA^ or ramarray[pregHL^];
  SetLogicalFlags(False);
  Inc(t_states,7);
end;

procedure TProcessor.ExecOUTportA; inline;
var port: byte;
begin
  port := Fetch8;
  ProcessPortOut(port,pregA^);
  Inc(t_states,11);
end;

procedure TProcessor.ExecPOPAF; inline;
begin
  pregAF^ := PopWord;
  Inc(t_states,10);
end;

procedure TProcessor.ExecPOPBC; inline;
begin
  pregBC^ := PopWord;
  Inc(t_states,10);
end;

procedure TProcessor.ExecPOPDE; inline;
begin
  pregDE^ := PopWord;
  Inc(t_states,10);
end;

procedure TProcessor.ExecPOPHL; inline;
begin
  pregHL^ := PopWord;
  Inc(t_states,10);
end;

procedure TProcessor.ExecPUSHAF; inline;
begin
  PushWord(pregAF^);
  Inc(t_states,11);
end;

procedure TProcessor.ExecPUSHBC; inline;
begin
  PushWord(pregBC^);
  Inc(t_states,11);
end;

procedure TProcessor.ExecPUSHDE; inline;
begin
  PushWord(pregDE^);
  Inc(t_states,11);
end;

procedure TProcessor.ExecPUSHHL; inline;
begin
  PushWord(pregHL^);
  Inc(t_states,11);
end;

procedure TProcessor.ExecRETcond(_mask, _required: byte); inline;
begin
  if pregF^ and _mask = _required then
    begin // Succeeded
      pregPC^ := PopWord;
      Inc(t_states,11);
    end
  else
    Inc(t_states,5); // Failed
end;

procedure TProcessor.ExecRST(_addr: byte); inline;
begin
  PushWord(pregPC^);
  pregPC^ := _addr;
  Inc(t_states,11);
end;

procedure TProcessor.ExecRET; inline;
begin
  pregPC^ := PopWord;
  Inc(t_states,10);
end;

procedure TProcessor.ExecRETC; inline;
begin
  ExecRETcond(FLAG_CARRY,FLAG_CARRY);
end;

procedure TProcessor.ExecRETM; inline;
begin
  ExecRETcond(FLAG_NEGATIVE,FLAG_NEGATIVE);
end;

procedure TProcessor.ExecRETNC; inline;
begin
  ExecRETcond(FLAG_CARRY,0);
end;

procedure TProcessor.ExecRETP; inline;
begin
  ExecRETcond(FLAG_NEGATIVE,0);
end;

procedure TProcessor.ExecRETPE; inline;
begin
  ExecRETcond(FLAG_PV,FLAG_PV);
end;

procedure TProcessor.ExecRETPO; inline;
begin
  ExecRETcond(FLAG_PV,0);
end;

procedure TProcessor.ExecRETNZ; inline;
begin
  ExecRETcond(FLAG_ZERO,0);
end;

procedure TProcessor.ExecRETZ; inline;
begin
  ExecRETcond(FLAG_ZERO,FLAG_ZERO);
end;

procedure TProcessor.ExecRLA; inline;
var bit7: byte;
begin
  bit7 := pregA^ and $80;
  pregA^ := ((pregA^ shl 1) or (pregF^ and FLAG_CARRY)) and $00FF;
  pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or (bit7 shr 7); // Set C flag if reqd and reset H, N
  Inc(t_states,4);
end;

procedure TProcessor.ExecRLCA; inline;
var bit7: byte;
begin
  bit7 := pregA^ and $80;
  pregA^ := ((pregA^ shl 1) or (bit7 shr 7)) and $00FF;
  pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or (bit7 shr 7); // Set C flag if reqd and reset H, N
  Inc(t_states,4);
end;

procedure TProcessor.ExecRRCA; inline;
var bit0: byte;
begin
  bit0 := pregA^ and $01;
  pregA^ := (pregA^ shr 1) or (bit0 shl 7);
  pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or bit0; // Set C flag if reqd and reset H, N
  Inc(t_states,4);
end;

procedure TProcessor.ExecRRA; inline;
var bit0: byte;
begin
  bit0 := pregA^ and $01;
  pregA^ := (pregA^ shr 1) or ((pregF^ and FLAG_CARRY) shl 7);
  pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or bit0; // Set C flag if reqd and reset H, N
  Inc(t_states,4);
end;

procedure TProcessor.ExecRST00; inline;
begin
  ExecRST($00);
end;

procedure TProcessor.ExecRST08; inline;
begin
  ExecRST($08);
end;

procedure TProcessor.ExecRST10; inline;
begin
  ExecRST($10);
end;

procedure TProcessor.ExecRST18; inline;
begin
  ExecRST($18);
end;

procedure TProcessor.ExecRST20; inline;
begin
  ExecRST($20);
end;

procedure TProcessor.ExecRST28; inline;
begin
  ExecRST($28);
end;

procedure TProcessor.ExecRST30; inline;
begin
  ExecRST($30);
end;

procedure TProcessor.ExecRST38; inline;
begin
  ExecRST($38);
end;

procedure TProcessor.ExecSBCA; inline;
begin
  ExecSUB(pregA^,4,True,True);
end;

procedure TProcessor.ExecSBCB; inline;
begin
  ExecSUB(pregB^,4,True,True);
end;

procedure TProcessor.ExecSBCC; inline;
begin
  ExecSUB(pregC^,4,True,True);
end;

procedure TProcessor.ExecSBCD; inline;
begin
  ExecSUB(pregD^,4,True,True);
end;

procedure TProcessor.ExecSBCE; inline;
begin
  ExecSUB(pregE^,4,True,True);
end;

procedure TProcessor.ExecSBCH; inline;
begin
  ExecSUB(pregH^,4,True,True);
end;

procedure TProcessor.ExecSBCimm; inline;
begin
  ExecSUB(Fetch8,7,True,True); // Subtract immediate from a, keep results
end;

procedure TProcessor.ExecSBCL; inline;
begin
  ExecSUB(pregL^,4,True,True);
end;

procedure TProcessor.ExecSBCM; inline;
begin
  ExecSUB(ramarray[pregHL^],7,True,True);
end;

procedure TProcessor.ExecSCF; inline;
begin
  pregF^ := pregF^ or FLAG_CARRY;
  pregF^ := pregF^ and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT;
  Inc(t_states,4);
end;

procedure TProcessor.ExecShiftRotate(p: PByte; style: byte);
var bit0:  byte;
    bit7:  byte;
begin
  case style of
    0: // RLC
      begin
        bit7 := p^ and $80;
        p^ := ((p^ shl 1) or (bit7 shr 7)) and $00FF;
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or (bit7 shr 7); // Set C flag if reqd and reset H, N
      end;
    1: // RRC
      begin
        bit0 := p^ and $01;
        p^ := ((p^ shr 1) or (bit0 shl 7));
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or bit0; // Set C flag if reqd and reset H, N
      end;
    2: // RL
      begin
        bit7 := p^ and $80;
        p^ := ((p^ shl 1) or (pregF^ and $01)) and $00FF;
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or (bit7 shr 7); // Set C flag if reqd and reset H, N
      end;
    3: // RR
      begin
        bit0 := p^ and $01;
        p^ := ((p^ shr 1) or ((pregF^ and $01) shl 7));
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or bit0; // Set C flag if reqd and reset H, N
      end;
    4: // SLA
      begin
        bit7 := p^ and $80;
        p^ := (p^ shl 1) and $00FF;
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or (bit7 shr 7); // Set C flag if reqd and reset H, N
      end;
    5: // SRA
      begin
        bit0 := p^ and $01;
        bit7 := p^ and $80;
        p^ := ((p^ shr 1) and $7F) or bit7;
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or bit0; // Set C flag if reqd and reset H, N
      end;
    6: // SLL (undocumented but implemented as for SLA)
      begin
        bit7 := p^ and $80;
        p^ := (p^ shl 1) and $00FF;
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or (bit7 shr 7); // Set C flag if reqd and reset H, N
      end;
    7: // SRL
      begin
        bit0 := p^ and $01;
        p^ := (p^ shr 1);
        pregF^ := (pregF^ and (NOT_FLAG_CARRY and NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT)) or bit0; // Set C flag if reqd and reset H, N
      end;
  end;
end;

procedure TProcessor.ExecSub(_b: byte; _states: integer; _save: boolean = True; _dosbc: boolean = False); {inline;}
var _a,_c,_cf: byte;
begin
  _a    := pregA^;
  _cf   := 0;
  if _dosbc and ((pregF^ and FLAG_CARRY) <> 0) then
    _cf := 1;
  _c := _a - _b - _cf;
  pregF^ := (pregF^ and NOT_FLAG_NEGATIVE) or (_c and FLAG_NEGATIVE);
  if _c = 0 then
    pregF^ := pregF^ or FLAG_ZERO
  else
    pregF^ := pregF^ and NOT_FLAG_ZERO;
  if (integer(_a and $0F) - integer(_b and $0F) - integer(_cf)) < 0 then
    pregF^ := pregF^ or FLAG_HALFCARRY  // Borrow from bit 4
  else
    pregF^ := pregF^ and NOT_FLAG_HALFCARRY;
  pregF^ := pregF^ and NOT_FLAG_PV; // Clear overflow for now
  if ((_a xor _b) and $80) <> 0 then // check for unalike signs
      if ((_a xor _c) and $80) <> 0 then
        pregF^ := pregF^ or FLAG_PV;
  pregF^ := pregF^ or FLAG_SUBTRACT;
  if integer(_a) - integer(_b) - integer(_cf) < 0 then
    pregF^ := pregF^ or FLAG_CARRY
  else
    pregF^ := pregF^ and NOT_FLAG_CARRY;
  if _save then
    pregA^ := _c;
  Inc(t_states,_states);
end;

procedure TProcessor.ExecSUBA; inline;
begin
  ExecSub(pregA^,4);
end;

procedure TProcessor.ExecSUBB; inline;
begin
  ExecSub(pregB^,4);
end;

procedure TProcessor.ExecSUBC; inline;
begin
  ExecSub(pregC^,4);
end;

procedure TProcessor.ExecSUBD; inline;
begin
  ExecSub(pregD^,4);
end;

procedure TProcessor.ExecSUBE; inline;
begin
  ExecSub(pregE^,4);
end;

procedure TProcessor.ExecSUBH; inline;
begin
  ExecSub(pregH^,4);
end;

procedure TProcessor.ExecSUBHLr16(_w: Word; _dosbc: boolean = False); inline;
var _cf: Word;
    flags: byte;
    _src: Word;
    _dst: Word;
begin
  _src := pregHL^;
  flags := pregF^;
  if _dosbc and ((flags and FLAG_CARRY) <> 0) then
    _cf := 1
  else
    _cf := 0;
  flags := flags and NOT_FLAG_SUBTRACT and NOT_FLAG_HALFCARRY and NOT_FLAG_CARRY and NOT_FLAG_ZERO and NOT_FLAG_PV and NOT_FLAG_NEGATIVE;
  // Check for borrow from bit 12
  if Word((_src and $0FFF) - (_w and $0FFF) - _cf) >= $1000 then
    flags := flags or FLAG_HALFCARRY;
  if integer(_src) - integer(_w) - integer(_cf) < 0 then
    flags := flags or FLAG_CARRY;
  _dst := _src - _w - _cf;
  if ((_src xor _w) and $8000) = $8000 then // check for unalike signs
      if ((_src xor _dst) and $8000) <> $0000 then
        flags := flags or FLAG_PV;
  if (_dst and $8000) <> 0 then
    flags := flags or FLAG_NEGATIVE;
  if (_dst = 0) then
    flags := flags or FLAG_ZERO;
  pregHL^ := _dst;
  pregF^ := Flags;
  Inc(t_states,11);
  if _dosbc then
    Inc(t_states,4); // Total 15 t states for ADC
end;

procedure TProcessor.ExecSUBimm; inline;
begin
  ExecSub(Fetch8,7,True); // Subtract immediate from a, keep results
end;

procedure TProcessor.ExecSUBL; inline;
begin
  ExecSub(pregL^,4);
end;

procedure TProcessor.ExecSUBM; inline;
begin
  ExecSub(ramarray[pregHL^],7);
end;

procedure TProcessor.ExecXORA; inline;
begin
  pregA^ := pregA^ xor pregA^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORB; inline;
begin
  pregA^ := pregA^ xor pregB^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORC; inline;
begin
  pregA^ := pregA^ xor pregC^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORD; inline;
begin
  pregA^ := pregA^ xor pregD^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORE; inline;
begin
  pregA^ := pregA^ xor pregE^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORH; inline;
begin
  pregA^ := pregA^ xor pregH^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORimm; inline;
begin
  pregA^ := pregA^ xor Fetch8;
  SetLogicalFlags(False);
  Inc(t_states,7);
end;

procedure TProcessor.ExecXORL; inline;
begin
  pregA^ := pregA^ xor pregL^;
  SetLogicalFlags(False);
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORM; inline;
begin
  pregA^ := pregA^ xor ramarray[pregHL^];;
  SetLogicalFlags(False);
  Inc(t_states,7);
end;

function TProcessor.Fetch8: byte; inline;
begin
  Result := ramarray[pregPC^];
  Inc(pregPC^);
end;

function TProcessor.Fetch16: Word; inline;
var _pc: Word;
begin
  _pc := pregPC^;
  Result := ramarray[_pc] + (ramarray[_pc+1] shl 8);
  Inc(pregPC^);
  Inc(pregPC^);
end;

function TProcessor.FetchIQPDindex: Word; inline;
begin
  Result :=  Add16u8s(pregIXY^,Fetch8);
end;

function TProcessor.FetchOpcode: byte; inline;
begin
  Result := Fetch8;
{$IFDEF UPDATE_R_REG}
  pregR^ := (pregR^ + 1) and $7F;
{$ENDIF}
end;

function TProcessor.GetCFPortBase: byte;
begin
  Result := FCFlash.PortBase;
end;

function TProcessor.GetRegisterSet: TRegisterSet;
begin
  Result := regset;
end;

function TProcessor.GetPerfMIPS: double;
var elapsed_time:  double;
    elapsed_insts: int64;
begin
  if FProcessorState <> psRunning then
    Result := 0.0
  else
    begin
      elapsed_insts := insts - run_start_instructions;
      elapsed_time  := (Now() - run_start_time) * 86400.0;
      if elapsed_time = 0.0 then
        Result := 0.0
      else
        Result := elapsed_insts / elapsed_time / 1000000.0;
    end;
end;

function TProcessor.GetPerfMHz: double;
var elapsed_time:  double;
    elapsed_T:     int64;
begin
  if FProcessorState <> psRunning then
    Result := 0.0
  else
    begin
      elapsed_T    := TStates - run_start_cycles;
      elapsed_time := (Now() - run_start_time) * 86400.0;
      if elapsed_time = 0.0 then
        Result := 0.0
      else
        Result := elapsed_T / elapsed_time / 1000000.0;
    end;
end;

function TProcessor.PopWord: Word; inline;
begin
  Result := ramarray[pregSP^];
  Inc(pregSP^);
  Result := Result or (ramarray[pregSP^] shl 8);
  Inc(pregSP^);
end;

function TProcessor.ProcessPortIn(_port: byte): byte;
begin
  Result := 0;
  if (_port and $F8) = FCFlash.PortBase then
    Result := FCFlash.PortRead(_port - FCFlash.PortBase)
  else
    case _port of
      SIOA_D: Result := SIO.ChannelA.Data;        // Port 00
      SIOB_D: Result := SIO.ChannelB.Data;        // Port 01
      SIOA_C: Result := SIO.ChannelA.Control;     // Port 02
      SIOB_C: Result := SIO.ChannelB.Control;     // Port 03
      otherwise
        begin
          FErrorString := Format('Input port $%2.2X not catered for',[_port]);
          error_flag := error_flag + [efBadPortIn];
        end;
    end;
end;

procedure TProcessor.ProcessPortOut(_port, _byte: byte);
begin
  // Check if in CF port space first
  if (_port and $F8) = FCFlash.PortBase then
    FCFlash.PortWrite(_port-FCFlash.PortBase,_byte)
  else // Try for serial
    case _port of
      $38: ; // ROM port - ignore
      SIOA_D: SIO.ChannelA.Data := _byte;        // Port 00
      SIOB_D: SIO.ChannelB.Data := _byte;        // Port 01
      SIOA_C: SIO.ChannelA.Control := _byte;     // Port 02
      SIOB_C: SIO.ChannelB.Control := _byte;     // Port 03
      otherwise
        begin
          FErrorString := Format('Output port $%2.2X not catered for',[_port]);
          error_flag := error_flag + [efBadPortOut];
        end;
    end;
end;

procedure TProcessor.PushWord(_word: Word); inline;
begin
  Dec(pregSP^);
  ramarray[pregSP^] := _word shr 8;
  Dec(pregSP^);
  ramarray[pregSP^] := _word and $00FF;
end;

procedure TProcessor.ReadFromStream(_strm: TStream; _start, _length: integer);
begin
  _strm.Read(ramarray[_start],_length);
end;

procedure TProcessor.ReadFromXml(doc: TXMLdocument);
var node: TDOMnode;
    i,j:  integer;
    p:    PByte;
    s:    string;
begin
  // Read the registers
  node := doc.DocumentElement.FindNode('registers');
  GetXmlWordP(node,'reg__af',pregAF,0);
  GetXmlWordP(node,'reg__bc',pregBC,0);
  GetXmlWordP(node,'reg__de',pregDE,0);
  GetXmlWordP(node,'reg__hl',pregHL,0);
  GetXmlWordP(node,'reg_xaf',pregAF_,0);
  GetXmlWordP(node,'reg_xbc',pregBC_,0);
  GetXmlWordP(node,'reg_xde',pregDE_,0);
  GetXmlWordP(node,'reg_xhl',pregHL_,0);
  GetXmlWordP(node,'reg__ir',pregIR,0);
  GetXmlWordP(node,'reg__ix',pregIX,0);
  GetXmlWordP(node,'reg__iy',pregIY,0);
  GetXmlWordP(node,'reg__sp',pregSP,0);
  GetXmlWordP(node,'reg__pc',pregPC,0);
  GetXmlByteP(node,'int_enabled',pregIntE,0);
  GetXmlByteP(node,'int_mode',pregIntM,0);
  // Read the memory
  node := doc.DocumentElement.FindNode('memory');
  i := 0;
  p := @RAM[0];
  while i < 65536 do
    begin
      s := GetXmlString(node,'memory_' + IntToHex(i,4));
      if Length(s) <> BYTES_PER_LINE * 2 then
        raise Exception.Create('VM file memory corrupted');
      for j := 0 to BYTES_PER_LINE-1 do
        begin
          p^ := StrToInt('$' + Copy(s,1+j*2,2));
          Inc(p);
          Inc(i);
        end;
    end;
  // Load the SIO sections
  SIO.ChannelA.ReadFromXml(doc,'a');
  SIO.ChannelB.ReadFromXml(doc,'b');
end;

procedure TProcessor.SetCFPortBase(_b: byte);
begin
  FCFlash.PortBase := _b;
end;

procedure TProcessor.SetCPUspeed(_speed: int64);
begin
  cpu_speed := _speed;
  TStates := 0;
  insts := 0;
  run_start_time := Now();
  run_start_cycles := TStates;
  run_start_instructions := insts;
end;

procedure TProcessor.SetLogicalflags(_seth: boolean); inline;
var flags: byte;
begin
  flags := pregF^ and (NOT_FLAG_HALFCARRY and NOT_FLAG_SUBTRACT and NOT_FLAG_NEGATIVE and NOT_FLAG_CARRY); // Reset H/N/C
  flags := (flags and $7F) or (pregA^ and FLAG_NEGATIVE); // Bit 7 is neg flag
  if pregA^ = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;      // Bit 6 is zero flag
  flags := flags and NOT_FLAG_PV;        // Kill parity flag for now
  flags := flags or parity_table[pregA^];
  if _seth then
    flags := flags or FLAG_HALFCARRY;
  pregF^ := flags;
end;

procedure TProcessor.SetNegZero; inline;
begin
  pregF^ := pregF^ and NOT_FLAG_NEGATIVE and NOT_FLAG_ZERO;
  if (pregA^ and $80) <> 0 then pregF^ := pregF^ or FLAG_NEGATIVE;
  if pregA^ = 0 then pregF^ := pregF^ or FLAG_ZERO;
end;

procedure TProcessor.SetOnTransmitA(_proc: TSIOtransmitFunc);
begin
  SIO.ChannelA.OnTransmit := _proc;
end;

procedure TProcessor.SetPCrelative(_b: byte); inline;
begin
  pregPC^ := Add16u8s(pregPC^,_b);
end;

procedure TProcessor.SetProcessorState(_ps: TProcessorState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(_ps);
  FProcessorState := _ps;
end;

procedure TProcessor.WaitForStop;
begin
  ExecuteStop;
  while not Idle do
    Sleep(5);
end;

procedure TProcessor.WriteToXml(doc: TXMLdocument);
var node: TDOMnode;
    node_line: TDOMnode;
    node_text: TDOMnode;
    i,j:  integer;
    p:    PByte;
    s:    string;
begin
  // Write the registers
  node := doc.CreateElement('registers');
  PutXmlWord(node,'reg__af',pregAF^);
  PutXmlWord(node,'reg__bc',pregBC^);
  PutXmlWord(node,'reg__de',pregDE^);
  PutXmlWord(node,'reg__hl',pregHL^);
  PutXmlWord(node,'reg_xaf',pregAF_^);
  PutXmlWord(node,'reg_xbc',pregBC_^);
  PutXmlWord(node,'reg_xde',pregDE_^);
  PutXmlWord(node,'reg_xhl',pregHL_^);
  PutXmlWord(node,'reg__ir',pregIR^);
  PutXmlWord(node,'reg__ix',pregIX^);
  PutXmlWord(node,'reg__iy',pregIY^);
  PutXmlWord(node,'reg__sp',pregSP^);
  PutXmlWord(node,'reg__pc',pregPC^);
  PutXmlByte(node,'int_enabled',pregIntE^);
  PutXmlByte(node,'int_mode',pregIntM^);
  doc.ChildNodes[0].AppendChild(node);
  // Write the memory
  node := doc.CreateElement('memory');
  i := 0;
  p := @RAM[0];
  while i < 65536 do
    begin
      s := '';
      node_line := doc.CreateElement(Format('memory_%4.4X',[i]){%H-});
      for j := 0 to BYTES_PER_LINE-1 do
        begin
          s := s + IntToHex(p^,2);
          Inc(p);
          Inc(i);
        end;
      node_text := doc.CreateTextNode(s{%H-});
      node_line.AppendChild(node_text);
      node.AppendChild(node_line);
    end;
  doc.ChildNodes[0].AppendChild(node);
  // Load the SIO sections
  SIO.ChannelA.WriteToXml(doc,'a');
  SIO.ChannelB.WriteToXml(doc,'b');
end;

procedure TProcessor.WriteRAM(_addr: word; _data: byte);
begin
  ramarray[_addr] := _data;
end;


//=============================================================================
//
// CB instructions
//
//=============================================================================

procedure TProcessor.ExecCb; inline;
var proc: TExecProc;
begin
  // Get byte to execute
  opcode := FetchOpcode;
  {
  opcode := ramarray[pregPC^];
  Inc(pregPC^);
  }
  if (not FAllowUndocumented) and (undoc_cb[opcode]) then
    error_flag := error_flag + [efUndocumented]
  else
    begin
      proc := inst_cb[opcode];
      if proc <> nil then
        proc
      else
        error_flag := error_flag + [efIllegal];
    end;
end;

procedure TProcessor.ExecCbBITreg; inline;
var _src: byte;
    _mask: byte;
    _reg: byte;
    _bit: byte;
    flags: byte;
begin
  _bit:= (opcode shr 3) and $07;
  _reg := opcode and $07;
  _src := 0;
  case _reg of
    0: _src := pregB^;
    1: _src := pregC^;
    2: _src := pregD^;
    3: _src := pregE^;
    4: _src := pregH^;
    5: _src := pregL^;
    6: _src := ramarray[pregHL^];
    7: _src := pregA^;
  end;
  _mask := 1 shl _bit;
  // Set the flags
  flags := pregF^;
  flags := flags and NOT_FLAG_ZERO and NOT_FLAG_SUBTRACT; // Reset flags
  flags := flags or FLAG_HALFCARRY;
  if (_src and _mask) = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;
  pregF^ := flags;
  // Bump the t_states
  if _reg <> 6 then
    Inc(t_states,8)
  else
    Inc(t_states,12);
end;

procedure TProcessor.ExecCbBITresorset(_set: boolean); inline;
var p: Pbyte;
    _andmask: byte;
    _ormask:  byte;
    _reg: byte;
    _bit: byte;
begin
  _bit:= (opcode shr 3) and $07;
  _reg := opcode and $07;
  p := pregB; // Keep compiler quiet
  case _reg of
    0: p := pregB;
    1: p := pregC;
    2: p := pregD;
    3: p := pregE;
    4: p := pregH;
    5: p := pregL;
    6: begin
         p := @ramarray[pregHL^];
         Inc(t_states,4); // Extra 4 t_states for RAM access
       end;
    7: p := pregA;
  end;
  case _set of
    False: // Reset
      begin
        _andmask := (1 shl _bit) xor $FF;
        _ormask  := 0;
      end;
    True:  // Set
      begin
        _andmask := $FF;
        _ormask  := 1 shl _bit;
      end;
  end;
  // Set or Reset the bit
  p^ := (p^ and _andmask) or _ormask;
  // Bump the t_states
  Inc(t_states,8);
end;

procedure TProcessor.ExecCbBITres; inline;
begin
  ExecCbBITresorset(False);
end;

procedure TProcessor.ExecCbBITset; inline;
begin
  ExecCbBITresorset(True);
end;

procedure TProcessor.ExecCbShiftRotate; inline;
var p: PByte; // Pointer to the item to shift
    style: byte;
    regindex: byte;
begin
  regindex := opcode and $07; // BCDEHLMA
  style    := (opcode and $38) shr 3; // Type of shift or rotate
  p := pregB; // Keep compiler quiet
  case regindex of
    0: p := pregB;
    1: p := pregC;
    2: p := pregD;
    3: p := pregE;
    4: p := pregH;
    5: p := pregL;
    6: begin
         p := @ramarray[pregHL^];
         Inc(t_states,7);  // Extra 7 T states for mem fetch
       end;
    7: p := pregA;
  end;
  ExecShiftRotate(p,style);
  Inc(t_states,8);
end;

//=============================================================================
//
// DD instructions
//
//=============================================================================

procedure TProcessor.ExecDd; inline;
var proc: TExecProc;
begin
  // Get byte to execute
  opcode := FetchOpcode;
  {
  opcode := ramarray[pregPC^];
  Inc(pregPC^);
  }
  pregIXY := pregIX;
  proc := inst_ddfd[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
end;

procedure TProcessor.ExecDdFdCb; inline;
var proc: TExecProc;
begin
  // Get byte to execute
  opcode := FetchOpcode;
  proc := inst_ddfdcb[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
end;


procedure TProcessor.ExecDdFdADCAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ExecAdd(ramarray[_index],19,True);
end;

procedure TProcessor.ExecDdFdADDAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ExecAdd(ramarray[_index],19,False);
end;

procedure TProcessor.ExecDdFdADDIQBC; inline;
begin
  ExecADDr16r16(pregIXY,pregBC^,15,False);
end;

procedure TProcessor.ExecDdFdADDIQDE; inline;
begin
  ExecADDr16r16(pregIXY,pregDE^,15,False);
end;

procedure TProcessor.ExecDdFdADDIQIQ; inline;
begin
  ExecADDr16r16(pregIXY,pregIXY^,15,False);
end;

procedure TProcessor.ExecDdFdADDIQSP; inline;
begin
  ExecADDr16r16(pregIXY,pregSP^,15,False);
end;

procedure TProcessor.ExecDdFdANDAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  pregA^ := pregA^ and ramarray[_index];
  SetLogicalFlags(True);
  Inc(t_states,19);
end;

procedure TProcessor.ExecDdFdCbBITIQpdind; inline;
var _src: byte;
    _mask: byte;
    _bit: byte;
    flags: byte;
begin
  _bit:= (opcode shr 3) and $07;
  _src := ramarray[FetchIQPDindex];
  _mask := 1 shl (_bit - 1);
  // Set the flags
  flags := pregF^;
  flags := flags and NOT_FLAG_ZERO and NOT_FLAG_SUBTRACT; // Reset flags
  flags := flags or FLAG_HALFCARRY;
  if (_src and _mask) = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;
  pregF^ := flags;
  // Bump the t_states
  Inc(t_states,20);
end;

procedure TProcessor.ExecDdFdCbRESIQpdind; inline;
var _p:  Pbyte;
    _mask: byte;
    _bit: byte;
begin
  _bit:= (opcode shr 3) and $07;
  _p := @ramarray[FetchIQPDindex];
  _mask := 1 shl (_bit - 1);
  _p^ := _p^ and (_mask xor $FF);
  // Bump the t_states
  Inc(t_states,23);
end;

procedure TProcessor.ExecDdFdCbSETIQpdind; inline;
var _p:  Pbyte;
    _mask: byte;
    _bit: byte;
begin
  _bit:= (opcode shr 3) and $07;
  _p := @ramarray[FetchIQPDindex];
  _mask := 1 shl (_bit - 1);
  _p^ := _p^ or _mask;
  // Bump the t_states
  Inc(t_states,23);
end;

procedure TProcessor.ExecDdFdCbShiftRotate; inline;
begin
  ExecShiftRotate(@ramarray[FetchIQPDindex],(opcode and $38) shr 3);
  Inc(t_states,23);
end;

procedure TProcessor.ExecDdFdCPAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ExecSub(ramarray[_index],19,False,False);
end;

procedure TProcessor.ExecDdFdDECIQ; inline;
begin
  Dec(pregIXY^);
  Inc(t_states,10);
end;

procedure TProcessor.ExecDdFdDECIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ExecDEC8(@ramarray[_index],23);
end;

procedure TProcessor.ExecDdFdEXSPindIQ; inline;
var psp: PWord;
    tmp: Word;
begin
  psp := PWord(@ramarray[pregSP^]);
  tmp := psp^;
  psp^ := pregIXY^;
  pregIXY^ := tmp;
  Inc(t_states,23);
end;

procedure TProcessor.ExecDdFdINCIQ; inline;
begin
  Inc(pregIXY^);
  Inc(t_states,10);
end;

procedure TProcessor.ExecDdFdINCIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ExecINC8(@ramarray[_index],23);
end;

procedure TProcessor.ExecDdFdLDIQimm; inline;
begin
  pregIXY^ := Fetch16;
  Inc(t_states,14);
end;

procedure TProcessor.ExecDdFdJPIQind; inline;
begin
  pregPC^ := pregIXY^;
  Inc(t_states,8);
end;

procedure TProcessor.ExecDdFdLDaddrIQ; inline;
var addr: Word;
    paddr: PWord;
begin
  addr := Fetch16;
  paddr := PWord(@ramarray[addr]);
  paddr^ := pregIXY^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecDdFdLDIQaddr; inline;
var addr: Word;
    paddr: PWord;
begin
  addr := Fetch16;
  paddr := PWord(@ramarray[addr]);
  pregIXY^ := paddr^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecDdFdLDIQpdindimm; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ramarray[_index] := Fetch8;
  Inc(t_states,19);
end;

procedure TProcessor.ExecDdFdLDIQpdindr8; inline;
var _b: PByte;
    _index: Word;
begin
  _index := FetchIQPDindex;
  _b := @ramarray[_index];
  case opcode and $38 of
    $70: _b^ := pregB^;
    $71: _b^ := pregC^;
    $72: _b^ := pregD^;
    $73: _b^ := pregE^;
    $74: _b^ := pregH^;
    $75: _b^ := pregL^;
    $77: _b^ := pregA^;
  end;
  Inc(t_states,19);
end;

procedure TProcessor.ExecDdFdLDr8IQpdind; inline;
var _b: Byte;
    _index: Word;
begin
  _index := FetchIQPDindex;
  _b := ramarray[_index];
  case opcode and $38 of
    $46: pregB^ := _b;
    $4E: pregC^ := _b;
    $56: pregD^ := _b;
    $5E: pregE^ := _b;
    $66: pregH^ := _b;
    $6E: pregL^ := _b;
    $7E: pregA^ := _b;
  end;
  Inc(t_states,19);
end;

procedure TProcessor.ExecDdFdLDSPIQ; inline;
begin
  pregSP^ := pregIXY^;
  Inc(t_states,10);
end;

procedure TProcessor.ExecDdFdORAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  pregA^ := pregA^ or ramarray[_index];
  SetLogicalFlags(False);
  Inc(t_states,19);
end;

procedure TProcessor.ExecDdFdPOPIQ; inline;
begin
  pregIXY^ := PopWord;
  Inc(t_states,14);
end;

procedure TProcessor.ExecDdFdPUSHIQ; inline;
begin
  PushWord(pregIXY^);
  Inc(t_states,14);
end;

procedure TProcessor.ExecDdFdSBCAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ExecSub(ramarray[_index],19,True,True);
end;

procedure TProcessor.ExecDdFdSUBAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  ExecSub(ramarray[_index],19,True,False);
end;

procedure TProcessor.ExecDdFdXORAIQpdind; inline;
var _index: Word;
begin
  _index := FetchIQPDindex;
  pregA^ := pregA^ xor ramarray[_index];
  SetLogicalFlags(False);
  Inc(t_states,19);
end;

//=============================================================================
//
// ED instructions
//
//=============================================================================

procedure TProcessor.ExecEd; inline;
var proc: TExecProc;
begin
  // Get byte to execute
  opcode := FetchOpcode;
  {
  opcode := ramarray[pregPC^];
  Inc(pregPC^);
  }
  proc := inst_ed[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
end;

procedure TProcessor.ExecEdADCHLBC; inline;
begin
  ExecADDHLr16(pregBC^,True);
end;

procedure TProcessor.ExecEdADCHLDE; inline;
begin
  ExecADDHLr16(pregDE^,True);
end;

procedure TProcessor.ExecEdADCHLHL; inline;
begin
  ExecADDHLr16(pregHL^,True);
end;

procedure TProcessor.ExecEdADCHLSP; inline;
begin
  ExecADDHLr16(pregSP^,True);
end;

procedure TProcessor.ExecEdCPD; inline;
var saved_c: byte;
begin
  saved_c := pregF^ and FLAG_CARRY; // Save incoming carry as ExecSub will set it
  ExecSub(ramarray[pregHL^],16,False);
  Dec(pregHL^);
  Dec(pregBC^);
  pregF^ := (pregF^ and NOT_FLAG_CARRY and NOT_FLAG_PV) or saved_c; // Restore incoming C flag
  if pregBC^ = $FFFF then
    pregF^ := pregF^ or FLAG_PV;
end;

procedure TProcessor.ExecEdCPDR; inline;
begin
  ExecEdCPD;
  if (pregBC^ <> 0) and ((pregF^ and FLAG_ZERO) = 0) then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdCPI; inline;
var saved_c: byte;
begin
  saved_c := pregF^ and FLAG_CARRY; // Save incoming carry as ExecSub will set it
  ExecSub(ramarray[pregHL^],16,False);
  Inc(pregHL^);
  Dec(pregBC^);
  pregF^ := (pregF^ and NOT_FLAG_CARRY and NOT_FLAG_PV) or saved_c; // Restore incoming C flag
  if pregBC^ = $FFFF then
    pregF^ := pregF^ or FLAG_PV;
end;

procedure TProcessor.ExecEdCPIR; inline;
begin
  ExecEdCPI;
  if (pregBC^ <> 0) and ((pregF^ and FLAG_ZERO) = 0) then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdIM0; inline;
begin
  pregIntM^ := 0;
  Inc(t_states,8);
end;

procedure TProcessor.ExecEdIM1; inline;
begin
  pregIntM^ := 1;
  Inc(t_states,8);
end;

procedure TProcessor.ExecEdIM2; inline;
begin
  pregIntM^ := 2;
  Inc(t_states,8);
end;

procedure TProcessor.ExecEdINACind; inline;
begin
  pregB^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdINBCind; inline;
begin
  pregB^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdINCCind; inline;
begin
  pregC^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdIND; inline;
begin
  ramarray[pregHL^] := ProcessPortIn(pregC^);
  Dec(pregHL^);
  Dec(pregB^);
  pregF^ := pregF^ and NOT_FLAG_PV;
  if pregB^ = $FF then pregF^ := pregF^ or FLAG_PV;
  pregF^ := pregF^ or FLAG_SUBTRACT;
  Inc(t_states,16);
end;

procedure TProcessor.ExecEdINDCind; inline;
begin
  pregD^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdINDR; inline;
begin
  ExecEdIND;
  if pregBC^ <> 0 then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdINECind; inline;
begin
  pregE^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdINHCind; inline;
begin
  pregH^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdINI; inline;
begin
  ramarray[pregHL^] := ProcessPortIn(pregC^);
  Inc(pregHL^);
  Dec(pregB^);
  pregF^ := pregF^ and NOT_FLAG_PV;
  if pregB^ = $FF then pregF^ := pregF^ or FLAG_PV;
  pregF^ := pregF^ or FLAG_SUBTRACT;
  Inc(t_states,16);
end;

procedure TProcessor.ExecEdINIR; inline;
begin
  ExecEdINI;
  if pregBC^ <> 0 then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdINLCind; inline;
begin
  pregL^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdINFCind; inline;
begin
  pregF^ := ProcessPortIn(pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdLDAI; inline;
begin
  // @@@@@ PV flag is not handled correctly
  pregF^ := pregF^ and NOT_FLAG_HALFCARRY and NOT_FLAG_PV and NOT_FLAG_SUBTRACT;
  pregA^ := pregI^;
  SetNegZero;
  Inc(t_states,9);
end;

procedure TProcessor.ExecEdLDAR; inline;
begin
  // @@@@@ PV flag is not handled correctly
  pregF^ := pregF^ and NOT_FLAG_HALFCARRY and NOT_FLAG_PV and NOT_FLAG_SUBTRACT;
  pregA^ := pregR^;
  SetNegZero;
  Inc(t_states,9);
end;

procedure TProcessor.ExecEdLDBCind; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pregBC^ := pw^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdLDDEind; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pregDE^ := pw^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdLDDR; inline;
begin
  ExecEdLDD;
  if pregBC^ <> 0 then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdLDHLind; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pregHL^ := pw^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdLDD; inline;
begin
  ramarray[pregDE^] := ramarray[pregHL^];
  Dec(pregDE^);
  Dec(pregHL^);
  Dec(pregBC^);
  pregF^ := pregF^ and NOT_FLAG_HALFCARRY and NOT_FLAG_PV and NOT_FLAG_SUBTRACT;
  if pregBC^ = $FFFF then pregF^ := pregF^ or FLAG_PV;
  Inc(t_states,16);
end;

procedure TProcessor.ExecEdLDI; inline;
begin
  ramarray[pregDE^] := ramarray[pregHL^];
  Inc(pregDE^);
  Inc(pregHL^);
  Dec(pregBC^);
  pregF^ := pregF^ and NOT_FLAG_HALFCARRY and NOT_FLAG_PV and NOT_FLAG_SUBTRACT;
  if pregBC^ = $FFFF then pregF^ := pregF^ or FLAG_PV;
  Inc(t_states,16);
end;

procedure TProcessor.ExecEdLDIA; inline;
begin
  pregI^ := pregA^;
  Inc(t_states,9);
end;

procedure TProcessor.ExecEdLDIR; inline;
begin
  ExecEdLDI;
  if pregBC^ <> 0 then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdLDindBC; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pw^ := pregBC^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdLDindDE; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pw^ := pregDE^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdLDindHL; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pw^ := pregHL^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdLDindSP; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pw^ := pregSP^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdLDRA; inline;
begin
  pregR^ := pregA^;
  Inc(t_states,9);
end;

procedure TProcessor.ExecEdLDSPind; inline;
var pw: PWord;
begin
  pw := PWord(@ramarray[Fetch16]);
  pregSP^ := pw^;
  Inc(t_states,20);
end;

procedure TProcessor.ExecEdNEG; inline;
var w: byte;
    flags: byte;
begin
  w := pregA^;
  flags := pregF^;
  flags := flags and NOT_FLAG_PV and NOT_FLAG_NEGATIVE and NOT_FLAG_ZERO and NOT_FLAG_HALFCARRY and NOT_FLAG_CARRY;
  flags := flags or FLAG_SUBTRACT;
  if (w and $0F) <> 0 then flags := flags or FLAG_HALFCARRY;
  if w = $80 then flags := flags or FLAG_PV;
  if w <> 0 then flags := flags or FLAG_CARRY;
  w := Byte(0 - w);
  if (w and $80) <> 0 then flags := flags or FLAG_NEGATIVE;
  if w = 0 then flags := flags or FLAG_ZERO;
  pregA^ := w;
  pregF^ := flags;
  Inc(t_states,8);
end;

procedure TProcessor.ExecEdOUTCindA; inline;
begin
  ProcessPortOut(pregC^,pregA^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTCindB; inline;
begin
  ProcessPortOut(pregC^,pregB^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTCindC; inline;
begin
  ProcessPortOut(pregC^,pregC^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTCindD; inline;
begin
  ProcessPortOut(pregC^,pregD^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTCindE; inline;
begin
  ProcessPortOut(pregC^,pregE^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTCindH; inline;
begin
  ProcessPortOut(pregC^,pregH^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTCindL; inline;
begin
  ProcessPortOut(pregC^,pregL^);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTCindZ; inline;
begin
  ProcessPortOut(pregC^,0);
  Inc(t_states,12);
end;

procedure TProcessor.ExecEdOUTD; inline;
begin
  ProcessPortOut(pregC^,ramarray[pregHL^]);
  Dec(pregHL^);
  Dec(pregB^);
  pregF^ := pregF^ and NOT_FLAG_PV;
  if pregB^ = $FF then pregF^ := pregF^ or FLAG_PV;
  pregF^ := pregF^ or FLAG_SUBTRACT;
  Inc(t_states,16);
end;

procedure TProcessor.ExecEdOTDR; inline;
begin
  ExecEdOUTD;
  if pregBC^ <> 0 then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdOUTI; inline;
begin
  ProcessPortOut(pregC^,ramarray[pregHL^]);
  Inc(pregHL^);
  Dec(pregB^);
  pregF^ := pregF^ and NOT_FLAG_PV;
  if pregB^ = $FF then pregF^ := pregF^ or FLAG_PV;
  pregF^ := pregF^ or FLAG_SUBTRACT;
  Inc(t_states,16);
end;

procedure TProcessor.ExecEdOTIR; inline;
begin
  ExecEdIND;
  if pregBC^ <> 0 then
    begin
      pregPC^ := SavedPC; // Go back to start of instruction
      Inc(t_states,5);     // Extra T states for looping back
    end;
end;

procedure TProcessor.ExecEdRETI; inline;
begin  // @@@@@ Not coded to do anything over and above what RET would do
  pregPC^ := PopWord;
  Inc(t_states,14);
end;

procedure TProcessor.ExecEdRETN; inline;
begin  // @@@@@ Not coded to do anything over and above what RET would do
  pregPC^ := PopWord;
  Inc(t_states,14);
end;

procedure TProcessor.ExecEdRLD; inline;
var m: PByte;
    newa: byte;
begin
  pregF^ := pregF^ and NOT_FLAG_NEGATIVE and NOT_FLAG_ZERO and NOT_FLAG_HALFCARRY and NOT_FLAG_PV and NOT_FLAG_SUBTRACT;
  m := @ramarray[pregHL^];
  newa := (pregA^ and $F0) or ((m^ and $F0) shr 4);
  m^ := (m^ and $F0) shr 4;
  m^ := ((m^ and $0F) shl 4) or (pregA^ and $0F);
  pregA^ := newa;
  SetNegZero;
  pregF^ := pregF^ or parity_table[newa];
  Inc(t_states,18);
end;

procedure TProcessor.ExecEdRRD; inline;
var m: PByte;
    newa: byte;
begin
  pregF^ := pregF^ and NOT_FLAG_NEGATIVE and NOT_FLAG_ZERO and NOT_FLAG_HALFCARRY and NOT_FLAG_PV and NOT_FLAG_SUBTRACT;
  m := @ramarray[pregHL^];
  newa := (pregA^ and $F0) or (m^ and $0F);
  m^ := (m^ and $F0) shr 4;
  m^ := (m^ and $0F) or ((pregA^ and $0F) shl 4);
  pregA^ := newa;
  SetNegZero;
  pregF^ := pregF^ or parity_table[newa];
  Inc(t_states,18);
end;

procedure TProcessor.ExecEdSBCHLBC; inline;
begin
  ExecSUBHLr16(pregBC^,True);
end;

procedure TProcessor.ExecEdSBCHLDE; inline;
begin
  ExecSUBHLr16(pregDE^,True);
end;

procedure TProcessor.ExecEdSBCHLHL; inline;
begin
  ExecSUBHLr16(pregHL^,True);
end;

procedure TProcessor.ExecEdSBCHLSP; inline;
begin
  ExecSUBHLr16(pregSP^,True);
end;


//=============================================================================
//
// FD instructions
//
//=============================================================================

procedure TProcessor.ExecFd; inline;
var proc: TExecProc;
begin
  // Get byte to execute
  opcode := FetchOpcode;
  pregIXY := pregIY;
  proc := inst_ddfd[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
  proc := inst_ddfd[opcode];
end;


//-----------------------------------------------------------------------------
//
// Processor main execution loop
//
//-----------------------------------------------------------------------------

procedure TProcessor.Execute;
var elapsed_time:   double; // Number of seconds elapsed since run started
    simulated_time: double; // Number of simulated seconds elapsed since run started
    i:              integer;
    check_every:    integer;
begin
  while (not Terminated) do
    begin
      case FProcessorState of
        psStepInto:
          begin
            Idle := False;
            ExecuteOneInst;
            if ErrorFlag <> [] then
              ProcessorState := psFault;
            ProcessorState := psPaused;
            Idle := True;
          end;
        psRunning:
          begin
            Idle := False;
            i := 0;
            if CPUspeed > 10000000 then
              check_every := 100000
            else if CPUspeed > 1000000 then
              check_every := 10000
            else if CPUspeed > 100000 then
              check_every := 1000
            else
              check_every := 250;
            while (i < CHECK_EVERY) and (FProcessorState = psRunning) and (error_flag = []) and (bparray[pregPC^] = []) do
              begin
                ExecuteOneInst;
                Inc(i);
                {
                // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                if pregPC^ = $5000 then
                  ProcessorState := psPaused;
                // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                }
              end;
            if error_flag <> [] then
              ProcessorState := psFault
            else
              begin
                // Check timing and see if we need to insert a sleep()
                elapsed_time := 86400.0 * (Now() - run_start_time);
                simulated_time := (TStates - run_start_cycles) / CpuSpeed;
                if simulated_time > elapsed_time then
                  sleep(Trunc((simulated_time-elapsed_time)*1000.0+0.5));
              end;
            if bpPermanent in bparray[pregPC^] then
              ProcessorState := psBreak
            else if bpTemporary in bparray[pregPC^] then
              begin
                ProcessorState := psPaused;
                ClearTemporaryBreakpoints;
              end;
            Idle := True;
          end;
        otherwise
          begin
            Idle := True;
            sleep(10);
          end;
      end;
    end;
end;

function TProcessor.ExecuteOneInst: boolean;
var proc:   TExecProc;
    addr:   Word;
    vector: Word;
{$IFDEF EXEC_TRACE}
    i:      integer;
{$ENDIF}
begin
   error_flag := [];
   SavedPC := pregPC^;
{$IFDEF EXEC_TRACE}
  for i := EXEC_TRACE_SIZE-2 downto 0 do
    exec_trace_log[i+1] := exec_trace_log[i];
  exec_trace_log[0] := regset;
{$ENDIF}
  // Check if interrupt enable has been executed
  if pregIntE^ > 1 then
    Dec(pregIntE^);
  // Check if interrupt waiting
  if int_flag and (pregIntE^ <> 0) then
    begin
      int_flag := False; // Reset the flag!
      pregIntE^ := 0; // Disable any further interrupts
      vector := (pregI^ shl 8) or (int_vec and $FE);
      addr := ramarray[vector] or (ramarray[vector+1] shl 8);
      PushWord(pregPC^);
      pregPC^ := addr;
      Inc(t_states,17); // @@@@@ Find out t states for processing interrupt
    end
  else
    begin
      // Get first byte to execute
      opcode := FetchOpcode;
      {
      opcode := ramarray[pregPC^];
      Inc(pregPC^);
      }
       proc := inst_std[opcode];
      if proc <> nil then
        proc
      else
        error_flag := error_flag + [efIllegal];
    end;
  // Check SIOs as they are clocked from here
  Inc(big_counter);
  if (big_counter and $3F) = 0 then // About every 32 instructions
    if (not int_flag) then
      SIO.ClockRX;
  Result := (error_flag = []);
  Inc(insts);
end;

procedure TProcessor.ExecuteRun;
begin
  if ProcessorState <> psRunning then
    begin
      error_flag := [];
      run_start_time         := Now();
      run_start_cycles       := TStates;
      run_start_instructions := insts;
      ProcessorState := psRunning;
    end;
end;

procedure TProcessor.ExecuteStepInto;
begin
  if ProcessorState <> psRunning then
    begin
      error_flag := [];
      ProcessorState := psStepInto;
    end;
end;

procedure TProcessor.ExecuteStepOver;
begin
  if ProcessorState <> psRunning then
    begin
      error_flag := [];
      // @@@@@ Put the breakpoint stuff here
      // @@@@@ We would only put one btTemporary in place straight after the
      // @@@@@ instruction. However, we don't know the length of the instruction
      // @@@@@ right now, just that it's in the range 1..4
      bparray[pregPC^+1] := bparray[pregPC^+1] + [bpTemporary];
      bparray[pregPC^+2] := bparray[pregPC^+2] + [bpTemporary];
      bparray[pregPC^+3] := bparray[pregPC^+3] + [bpTemporary];
      bparray[pregPC^+4] := bparray[pregPC^+4] + [bpTemporary];
      ProcessorState := psRunning;
    end;
end;

procedure TProcessor.ExecuteStop;
begin
  if ProcessorState = psRunning then
    ProcessorState := psPaused;
end;

procedure TProcessor.Init;
var b, n: byte;
begin
  // Set up parity table
  for b in byte do
    begin
      n := (b shr 4) xor (b and $0F);
      n := (n shr 2) xor (n and $03);
      n := (n shr 1) xor (n and $01);
      parity_table[b] := (n shl 2) xor FLAG_PV;
    end;

  for b in byte do inst_std[b]    := nil;
  for b in byte do inst_cb[b]     := nil;
  for b in byte do inst_ddfd[b]   := nil;
  for b in byte do inst_ddfdcb[b] := nil;
  for b in byte do inst_ed[b]     := nil;
  // Set up standard instructions
  inst_std[$00] := @ExecNOP;
  inst_std[$01] := @ExecLDBCimm;
  inst_std[$02] := @ExecLDBCindA;
  inst_std[$03] := @ExecINCBC;
  inst_std[$04] := @ExecINCB;
  inst_std[$05] := @ExecDECB;
  inst_std[$06] := @ExecLDBimm;
  inst_std[$07] := @ExecRLCA;
  inst_std[$08] := @ExecEXAFAF_;
  inst_std[$09] := @ExecADDHLBC;
  inst_std[$0A] := @ExecLDABCind;
  inst_std[$0B] := @ExecDECBC;
  inst_std[$0C] := @ExecINCC;
  inst_std[$0D] := @ExecDECC;
  inst_std[$0E] := @ExecLDCimm;
  inst_std[$0F] := @ExecRRCA;
  inst_std[$10] := @ExecDJNZ;
  inst_std[$11] := @ExecLDDEimm;
  inst_std[$12] := @ExecLDDEindA;
  inst_std[$13] := @ExecINCDE;
  inst_std[$14] := @ExecINCD;
  inst_std[$15] := @ExecDECD;
  inst_std[$16] := @ExecLDDimm;
  inst_std[$17] := @ExecRLA;
  inst_std[$18] := @ExecJR;
  inst_std[$19] := @ExecADDHLDE;
  inst_std[$1A] := @ExecLDADEind;
  inst_std[$1B] := @ExecDECDE;
  inst_std[$1C] := @ExecINCE;
  inst_std[$1D] := @ExecDECE;
  inst_std[$1E] := @ExecLDEimm;
  inst_std[$1F] := @ExecRRA;
  inst_std[$20] := @ExecJRNZ;
  inst_std[$21] := @ExecLDHLimm;
  inst_std[$22] := @ExecLDaddrHL;
  inst_std[$23] := @ExecINCHL;
  inst_std[$24] := @ExecINCH;
  inst_std[$25] := @ExecDECH;
  inst_std[$26] := @ExecLDHimm;
  inst_std[$27] := @ExecDAA;
  inst_std[$28] := @ExecJRZ;
  inst_std[$29] := @ExecADDHLHL;
  inst_std[$2A] := @ExecLDHLaddr;
  inst_std[$2B] := @ExecDECHL;
  inst_std[$2C] := @ExecINCL;
  inst_std[$2D] := @ExecDECL;
  inst_std[$2E] := @ExecLDLimm;
  inst_std[$2F] := @ExecCPL2;
  inst_std[$30] := @ExecJRNC;
  inst_std[$31] := @ExecLDSPimm;
  inst_std[$32] := @ExecLDaddrA;
  inst_std[$33] := @ExecINCSP;
  inst_std[$34] := @ExecINCM;
  inst_std[$35] := @ExecDECM;
  inst_std[$36] := @ExecLDHLindimm;
  inst_std[$37] := @ExecSCF;
  inst_std[$38] := @ExecJRC;
  inst_std[$39] := @ExecADDHLSP;
  inst_std[$3A] := @ExecLDAaddr;
  inst_std[$3B] := @ExecDECSP;
  inst_std[$3C] := @ExecINCA;
  inst_std[$3D] := @ExecDECA;
  inst_std[$3E] := @ExecLDAimm;
  inst_std[$3F] := @ExecCCF;
  inst_std[$40] := @ExecLDBB;
  inst_std[$41] := @ExecLDBC;
  inst_std[$42] := @ExecLDBD;
  inst_std[$43] := @ExecLDBE;
  inst_std[$44] := @ExecLDBH;
  inst_std[$45] := @ExecLDBL;
  inst_std[$46] := @ExecLDBM;
  inst_std[$47] := @ExecLDBA;
  inst_std[$48] := @ExecLDCB;
  inst_std[$49] := @ExecLDCC;
  inst_std[$4A] := @ExecLDCD;
  inst_std[$4B] := @ExecLDCE;
  inst_std[$4C] := @ExecLDCH;
  inst_std[$4D] := @ExecLDCL;
  inst_std[$4E] := @ExecLDCM;
  inst_std[$4F] := @ExecLDCA;
  inst_std[$50] := @ExecLDDB;
  inst_std[$51] := @ExecLDDC;
  inst_std[$52] := @ExecLDDD;
  inst_std[$53] := @ExecLDDE;
  inst_std[$54] := @ExecLDDH;
  inst_std[$55] := @ExecLDDL;
  inst_std[$56] := @ExecLDDM;
  inst_std[$57] := @ExecLDDA;
  inst_std[$58] := @ExecLDEB;
  inst_std[$59] := @ExecLDEC;
  inst_std[$5A] := @ExecLDED;
  inst_std[$5B] := @ExecLDEE;
  inst_std[$5C] := @ExecLDEH;
  inst_std[$5D] := @ExecLDEL;
  inst_std[$5E] := @ExecLDEM;
  inst_std[$5F] := @ExecLDEA;
  inst_std[$60] := @ExecLDHB;
  inst_std[$61] := @ExecLDHC;
  inst_std[$62] := @ExecLDHD;
  inst_std[$63] := @ExecLDHE;
  inst_std[$64] := @ExecLDHH;
  inst_std[$65] := @ExecLDHL;
  inst_std[$66] := @ExecLDHM;
  inst_std[$67] := @ExecLDHA;
  inst_std[$68] := @ExecLDLB;
  inst_std[$69] := @ExecLDLC;
  inst_std[$6A] := @ExecLDLD;
  inst_std[$6B] := @ExecLDLE;
  inst_std[$6C] := @ExecLDLH;
  inst_std[$6D] := @ExecLDLL;
  inst_std[$6E] := @ExecLDLM;
  inst_std[$6F] := @ExecLDLA;
  inst_std[$70] := @ExecLDMB;
  inst_std[$71] := @ExecLDMC;
  inst_std[$72] := @ExecLDMD;
  inst_std[$73] := @ExecLDME;
  inst_std[$74] := @ExecLDMH;
  inst_std[$75] := @ExecLDML;
  inst_std[$76] := @ExecHALT;
  inst_std[$77] := @ExecLDMA;
  inst_std[$78] := @ExecLDAB;
  inst_std[$79] := @ExecLDAC;
  inst_std[$7A] := @ExecLDAD;
  inst_std[$7B] := @ExecLDAE;
  inst_std[$7C] := @ExecLDAH;
  inst_std[$7D] := @ExecLDAL;
  inst_std[$7E] := @ExecLDAM;
  inst_std[$7F] := @ExecLDAA;
  inst_std[$80] := @ExecADDB;
  inst_std[$81] := @ExecADDC;
  inst_std[$82] := @ExecADDD;
  inst_std[$83] := @ExecADDE;
  inst_std[$84] := @ExecADDH;
  inst_std[$85] := @ExecADDL;
  inst_std[$86] := @ExecADDM;
  inst_std[$87] := @ExecADDA;
  inst_std[$88] := @ExecADCB;
  inst_std[$89] := @ExecADCC;
  inst_std[$8A] := @ExecADCD;
  inst_std[$8B] := @ExecADCE;
  inst_std[$8C] := @ExecADCH;
  inst_std[$8D] := @ExecADCL;
  inst_std[$8E] := @ExecADCM;
  inst_std[$8F] := @ExecADCA;
  inst_std[$90] := @ExecSUBB;
  inst_std[$91] := @ExecSUBC;
  inst_std[$92] := @ExecSUBD;
  inst_std[$93] := @ExecSUBE;
  inst_std[$94] := @ExecSUBH;
  inst_std[$95] := @ExecSUBL;
  inst_std[$96] := @ExecSUBM;
  inst_std[$97] := @ExecSUBA;
  inst_std[$98] := @ExecSBCB;
  inst_std[$99] := @ExecSBCC;
  inst_std[$9A] := @ExecSBCD;
  inst_std[$9B] := @ExecSBCE;
  inst_std[$9C] := @ExecSBCH;
  inst_std[$9D] := @ExecSBCL;
  inst_std[$9E] := @ExecSBCM;
  inst_std[$9F] := @ExecSBCA;
  inst_std[$A0] := @ExecANDB;
  inst_std[$A1] := @ExecANDC;
  inst_std[$A2] := @ExecANDD;
  inst_std[$A3] := @ExecANDE;
  inst_std[$A4] := @ExecANDH;
  inst_std[$A5] := @ExecANDL;
  inst_std[$A6] := @ExecANDM;
  inst_std[$A7] := @ExecANDA;
  inst_std[$A8] := @ExecXORB;
  inst_std[$A9] := @ExecXORC;
  inst_std[$AA] := @ExecXORD;
  inst_std[$AB] := @ExecXORE;
  inst_std[$AC] := @ExecXORH;
  inst_std[$AD] := @ExecXORL;
  inst_std[$AE] := @ExecXORM;
  inst_std[$AF] := @ExecXORA;
  inst_std[$B0] := @ExecORB;
  inst_std[$B1] := @ExecORC;
  inst_std[$B2] := @ExecORD;
  inst_std[$B3] := @ExecORE;
  inst_std[$B4] := @ExecORH;
  inst_std[$B5] := @ExecORL;
  inst_std[$B6] := @ExecORM;
  inst_std[$B7] := @ExecORA;
  inst_std[$B8] := @ExecCPB;
  inst_std[$B9] := @ExecCPC;
  inst_std[$BA] := @ExecCPD;
  inst_std[$BB] := @ExecCPE;
  inst_std[$BC] := @ExecCPH;
  inst_std[$BD] := @ExecCPL;
  inst_std[$BE] := @ExecCPM;
  inst_std[$BF] := @ExecCPA;
  inst_std[$C0] := @ExecRETNZ;
  inst_std[$C1] := @ExecPOPBC;
  inst_std[$C2] := @ExecJPNZ;
  inst_std[$C3] := @ExecJPabs;
  inst_std[$C4] := @ExecCALLNZ;
  inst_std[$C5] := @ExecPUSHBC;
  inst_std[$C6] := @ExecADDimm;
  inst_std[$C7] := @ExecRST00;
  inst_std[$C8] := @ExecRETZ;
  inst_std[$C9] := @ExecRET;
  inst_std[$CA] := @ExecJPZ;
  inst_std[$CB] := @ExecCb;
  inst_std[$CC] := @ExecCALLZ;
  inst_std[$CD] := @ExecCALLabs;
  inst_std[$CE] := @ExecADCimm;
  inst_std[$CF] := @ExecRST08;
  inst_std[$D0] := @ExecRETNC;
  inst_std[$D1] := @ExecPOPDE;
  inst_std[$D2] := @ExecJPNC;
  inst_std[$D3] := @ExecOUTportA;
  inst_std[$D4] := @ExecCALLNC;
  inst_std[$D5] := @ExecPUSHDE;
  inst_std[$D6] := @ExecSUBimm;
  inst_std[$D7] := @ExecRST10;
  inst_std[$D8] := @ExecRETC;
  inst_std[$D9] := @ExecEXX;
  inst_std[$DA] := @ExecJPC;
  inst_std[$DB] := @ExecINAport;
  inst_std[$DC] := @ExecCALLC;
  inst_std[$DD] := @ExecDd;
  inst_std[$DE] := @ExecSBCimm;
  inst_std[$DF] := @ExecRST18;
  inst_std[$E0] := @ExecRETPO;
  inst_std[$E1] := @ExecPOPHL;
  inst_std[$E2] := @ExecJPPO;
  inst_std[$E3] := @ExecEXSPindHL;
  inst_std[$E4] := @ExecCALLPO;
  inst_std[$E5] := @ExecPUSHHL;
  inst_std[$E6] := @ExecANDimm;
  inst_std[$E7] := @ExecRST20;
  inst_std[$E8] := @ExecRETPE;
  inst_std[$E9] := @ExecJPHLind;
  inst_std[$EA] := @ExecJPPE;
  inst_std[$EB] := @ExecEXDEHL;
  inst_std[$EC] := @ExecCALLPE;
  inst_std[$ED] := @ExecEd;
  inst_std[$EE] := @ExecXORimm;
  inst_std[$EF] := @ExecRST28;
  inst_std[$F0] := @ExecRETP;
  inst_std[$F1] := @ExecPOPAF;
  inst_std[$F2] := @ExecJPP;
  inst_std[$F3] := @ExecDI;
  inst_std[$F4] := @ExecCALLP;
  inst_std[$F5] := @ExecPUSHAF;
  inst_std[$F6] := @ExecORimm;
  inst_std[$F7] := @ExecRST30;
  inst_std[$F8] := @ExecRETM;
  inst_std[$F9] := @ExecLDSPHL;
  inst_std[$FA] := @ExecJPM;
  inst_std[$FB] := @ExecEI;
  inst_std[$FC] := @ExecCALLM;
  inst_std[$FD] := @ExecFd;
  inst_std[$FE] := @ExecCPimm;
  inst_std[$FF] := @ExecRST38;
  // Set up CB instructions
  for b in byte do undoc_cb[b] := False;
  for b := $30 to $37 do undoc_cb[b] := True; // Undocumented SLL instructions
  for b := $00 to $3F do inst_cb[b] := @ExecCbShiftRotate;
  for b := $40 to $7F do inst_cb[b] := @ExecCbBITreg;
  for b := $80 to $BF do inst_cb[b] := @ExecCbBITres;
  for b := $C0 to $FF do inst_cb[b] := @ExecCbBITset;
  // Set up DD/FD instructions
  inst_ddfd[$09] := @ExecDdFdADDIQBC;
  inst_ddfd[$19] := @ExecDdFdADDIQDE;
  inst_ddfd[$21] := @ExecDdFdLDIQimm;
  inst_ddfd[$22] := @ExecDdFdLDaddrIQ;
  inst_ddfd[$23] := @ExecDdFdINCIQ;
  inst_ddfd[$29] := @ExecDdFdADDIQIQ;
  inst_ddfd[$2A] := @ExecDdFdLDIQaddr;
  inst_ddfd[$2B] := @ExecDdFdDECIQ;
  inst_ddfd[$34] := @ExecDdFdINCIQpdind;
  inst_ddfd[$35] := @ExecDdFdDECIQpdind;
  inst_ddfd[$36] := @ExecDdFdLDIQpdindimm;
  inst_ddfd[$39] := @ExecDdFdADDIQSP;
  inst_ddfd[$46] := @ExecDdFdLDr8IQpdind;
  inst_ddfd[$4E] := @ExecDdFdLDr8IQpdind;
  inst_ddfd[$56] := @ExecDdFdLDr8IQpdind;
  inst_ddfd[$5E] := @ExecDdFdLDr8IQpdind;
  inst_ddfd[$66] := @ExecDdFdLDr8IQpdind;
  inst_ddfd[$6E] := @ExecDdFdLDr8IQpdind;
  inst_ddfd[$70] := @ExecDdFdLDIQpdindr8;
  inst_ddfd[$71] := @ExecDdFdLDIQpdindr8;
  inst_ddfd[$72] := @ExecDdFdLDIQpdindr8;
  inst_ddfd[$73] := @ExecDdFdLDIQpdindr8;
  inst_ddfd[$74] := @ExecDdFdLDIQpdindr8;
  inst_ddfd[$75] := @ExecDdFdLDIQpdindr8;
  inst_ddfd[$77] := @ExecDdFdLDIQpdindr8;
  inst_ddfd[$7E] := @ExecDdFdLDr8IQpdind;
  inst_ddfd[$86] := @ExecDdFdADDAIQpdind;
  inst_ddfd[$8E] := @ExecDdFdADCAIQpdind;
  inst_ddfd[$96] := @ExecDdFdSUBAIQpdind;
  inst_ddfd[$9E] := @ExecDdFdSBCAIQpdind;
  inst_ddfd[$A6] := @ExecDdFdANDAIQpdind;
  inst_ddfd[$AE] := @ExecDdFdXORAIQpdind;
  inst_ddfd[$B6] := @ExecDdFdORAIQpdind;
  inst_ddfd[$BE] := @ExecDdFdCPAIQpdind;
  inst_ddfd[$CB] := @ExecDdFdCb;
  inst_ddfd[$E1] := @ExecDdFdPOPIQ;
  inst_ddfd[$E3] := @ExecDdFdEXSPindIQ;
  inst_ddfd[$E5] := @ExecDdFdPUSHIQ;
  inst_ddfd[$E9] := @ExecDdFdJPIQind;
  inst_ddfd[$F9] := @ExecDdFdLDSPIQ;
  // Set up DD/FD + CB instructions
  inst_ddfdcb[$06] := @ExecDdFdCbShiftRotate;
  inst_ddfdcb[$0E] := @ExecDdFdCbShiftRotate;
  inst_ddfdcb[$16] := @ExecDdFdCbShiftRotate;
  inst_ddfdcb[$1E] := @ExecDdFdCbShiftRotate;
  inst_ddfdcb[$26] := @ExecDdFdCbShiftRotate;
  inst_ddfdcb[$2E] := @ExecDdFdCbShiftRotate;
//inst_ddfdcb[$36] := @ExecDdFdCbShiftRotate; // Undocumented
  inst_ddfdcb[$3E] := @ExecDdFdCbShiftRotate;
  inst_ddfdcb[$46] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$4E] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$56] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$5E] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$66] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$6E] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$76] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$7E] := @ExecDdFdCbBITIQpdind;
  inst_ddfdcb[$86] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$8E] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$96] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$9E] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$A6] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$AE] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$B6] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$BE] := @ExecDdFdCbRESIQpdind;
  inst_ddfdcb[$C6] := @ExecDdFdCbSETIQpdind;
  inst_ddfdcb[$CE] := @ExecDdFdCbSETIQpdind;
  inst_ddfdcb[$D6] := @ExecDdFdCbSETIQpdind;
  inst_ddfdcb[$DE] := @ExecDdFdCbSETIQpdind;
  inst_ddfdcb[$E6] := @ExecDdFdCbSETIQpdind;
  inst_ddfdcb[$EE] := @ExecDdFdCbSETIQpdind;
  inst_ddfdcb[$F6] := @ExecDdFdCbSETIQpdind;
  inst_ddfdcb[$FE] := @ExecDdFdCbSETIQpdind;
  // Set up ED instructions
  for b in byte do undoc_ed[b] := False;
    undoc_ed[$63] := True;
    undoc_ed[$6B] := True;
    undoc_ed[$70] := True;
    undoc_ed[$71] := True;
  inst_ed[$40] := @ExecEdINBCind;
  inst_ed[$41] := @ExecEdOUTCindB;
  inst_ed[$42] := @ExecEdSBCHLBC;
  inst_ed[$43] := @ExecEdLDindBC;
  inst_ed[$44] := @ExecEdNEG;
  inst_ed[$45] := @ExecEdRETN;
  inst_ed[$46] := @ExecEdIM0;
  inst_ed[$47] := @ExecEdLDIA;
  inst_ed[$48] := @ExecEdINCCind;
  inst_ed[$49] := @ExecEdOUTCindC;
  inst_ed[$4A] := @ExecEdADCHLBC;
  inst_ed[$4B] := @ExecEdLDBCind;
  inst_ed[$4D] := @ExecEdRETI;
  inst_ed[$4F] := @ExecEdLDRA;
  inst_ed[$50] := @ExecEdINDCind;
  inst_ed[$51] := @ExecEdOUTCindD;
  inst_ed[$52] := @ExecEdSBCHLDE;
  inst_ed[$53] := @ExecEdLDindDE;
  inst_ed[$56] := @ExecEdIM1;
  inst_ed[$57] := @ExecEDLDAI;
  inst_ed[$58] := @ExecEdINECind;
  inst_ed[$59] := @ExecEdOUTCindE;
  inst_ed[$5A] := @ExecEdADCHLDE;
  inst_ed[$5B] := @ExecEdLDDEind;
  inst_ed[$5E] := @ExecEdIM2;
  inst_ed[$5F] := @ExecEdLDAR;
  inst_ed[$60] := @ExecEdINHCind;
  inst_ed[$61] := @ExecEdOUTCindH;
  inst_ed[$62] := @ExecEdSBCHLHL;
  inst_ed[$63] := @ExecEdLDindHL;
  inst_ed[$67] := @ExecEdRRD;
  inst_ed[$68] := @ExecEdINLCind;
  inst_ed[$69] := @ExecEdOUTCindL;
  inst_ed[$6A] := @ExecEdADCHLHL;
  inst_ed[$6B] := @ExecEdLDHLind;
  inst_ed[$6F] := @ExecEdRLD;
  inst_ed[$70] := @ExecEdINFCind;
  inst_ed[$71] := @ExecEdOUTCindZ;
  inst_ed[$72] := @ExecEdSBCHLSP;
  inst_ed[$73] := @ExecEdLDindSP;
  inst_ed[$78] := @ExecEdINACind;
  inst_ed[$79] := @ExecEdOUTCindA;
  inst_ed[$7A] := @ExecEdADCHLSP;
  inst_ed[$7B] := @ExecEdLDSPind;
  inst_ed[$A0] := @ExecEdLDI;
  inst_ed[$A1] := @ExecEdCPI;
  inst_ed[$A2] := @ExecEdINI;
  inst_ed[$A3] := @ExecEdOUTI;
  inst_ed[$A8] := @ExecEdLDD;
  inst_ed[$A9] := @ExecEdCPD;
  inst_ed[$AA] := @ExecEdIND;
  inst_ed[$AB] := @ExecEdOUTD;
  inst_ed[$B0] := @ExecEdLDIR;
  inst_ed[$B1] := @ExecEdCPIR;
  inst_ed[$B2] := @ExecEdINIR;
  inst_ed[$B3] := @ExecEdOTIR;
  inst_ed[$B8] := @ExecEdLDDR;
  inst_ed[$B9] := @ExecEdCPDR;
  inst_ed[$BA] := @ExecEdINDR;
  inst_ed[$BB] := @ExecEdOTDR;
  // Set up FD instructions
  // Finally do a hard reset of the processor
  Reset(True);
end;

// Cold boot or hard reset

procedure TProcessor.Reset(_initram: boolean);
var i: word;
    ri: TRegIndex;
begin
  SIO.Reset;             // Initialise SIO
  t_states := 0;
  pregIntE^ := 0;       // Disable interrupts
  pregIntM^ := 0;       // Interrupt mode 0 (IM0) like 8080
  for ri in TRegIndex do
    regset.registers[ri] := Word(Random(65536));
  pregAF^ := $FFFF;
  pregSP^ := $FFFF;
  pregPC^ := 0;
  pregIR^ := 0;
  if _initram then
    for i in word do
      ramarray[i] := Byte(Random(256));
end;

function TProcessor.CanInterrupt: boolean;
begin
  result := (not int_flag) and (pregIntE^ = 1);
end;

procedure TProcessor.Interrupt(_vec:byte);
begin
  int_vec := _vec;
  int_flag := True; // Flag the interrupt
end;

end.

