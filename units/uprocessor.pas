unit uprocessor;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, usio;

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

  TBreakpoint = (bpNone,bpTemporary,bpPermanent);

  TErrorFlag = (efIllegal,efHalt,efBadPortIn,efBadPortOut);
  TErrorFlags = set of TErrorFlag;

  TPortInFunction = function(_portno: Word): byte of object;
  TPortOutProcedure = procedure(_portno: Word; _value: byte) of object;


  TSIOtransmitProc = procedure(_value: byte) of object;

  TRAMarray = array[Word] of byte;

  TRegIndex = (regAF,regBC,regDE,regHL,regAF_,regBC_,regDE_,regHL_,regIR,
               regIX,regIY,regSP,regPC);

  {
  TReg8Index = (regA,regB,regC,regD,regE,regH,regL,regF,regI,regR,
                regIXH,regIXL,regIYH,regIYR,regSPH,regSPL,regPCH,regPCL);
  }

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
      regset:       TRegisterSet;
      ramarray:     TRAMarray;
      bparray:      array[Word] of TBreakPoint;
      portinarray:  array[Word] of TPortInFunction;
      portoutarray: array[Word] of TPortOutProcedure;
      parity_table: array[byte] of byte;
      inst_std:     array[byte] of TExecProc;
      inst_cb:      array[byte] of TExecProc;
      inst_ed:      array[byte] of TExecProc;
      int_flag:     boolean;
      int_vec:      byte;
      t_states:     int64;
      insts:        int64;
      cpu_speed:    int64;  // In Hz
      error_flag:   TErrorFlags;
      opcode:       byte;
      FProcessorState:  TProcessorState;
      run_start_time:         TDateTime;
      run_start_cycles:       int64;
      run_start_instructions: int64;
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
      SIO:      TSIO;
      FOnTransmitA: TSIOtransmitProc;
      FOnStateChange: TStateChangeProc;
      FErrorString: string;
      // Procs / funcs
      function  Fetch16: Word; inline;
      function  Fetch8: byte; inline;
      function  GetRegisterSet: TRegisterSet;
      function  GetPerfMIPS: double;
      function  GetPerfMHz: double;
      function  PopWord: Word; inline;
      function  ProcessPortIn(_port: byte): byte;
      procedure ExecAdd(_b: byte; _states: integer); inline;
      procedure ExecADDA; inline;
      procedure ExecADDB; inline;
      procedure ExecADDC; inline;
      procedure ExecADDD; inline;
      procedure ExecADDE; inline;
      procedure ExecADDH; inline;
      procedure ExecADDL; inline;
      procedure ExecADDM; inline;
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
      procedure ExecCPimm; inline;
      procedure ExecCb; inline;
      procedure ExecCbBITreg; inline;
      procedure ExecDECA; inline;
      procedure ExecDECB; inline;
      procedure ExecDECC; inline;
      procedure ExecDECD; inline;
      procedure ExecDECE; inline;
      procedure ExecDECH; inline;
      procedure ExecDECL; inline;
      procedure ExecDECM; inline;
      procedure ExecDI; inline;
      procedure ExecEI; inline;
      procedure ExecEd; inline;
      procedure ExecEdIM2; inline;
      procedure ExecEdLDIA; inline;
      procedure ExecEdRETI; inline;
      procedure ExecHALT; inline;
      procedure ExecINAport; inline;
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
      procedure ExecLDAB; inline;
      procedure ExecLDAC; inline;
      procedure ExecLDAD; inline;
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
      procedure ExecLDSPimm; inline;
      procedure ExecLDaddrA; inline;
      procedure ExecLDaddrHL; inline;
      procedure ExecORA; inline;
      procedure ExecORB; inline;
      procedure ExecORC; inline;
      procedure ExecORD; inline;
      procedure ExecORE; inline;
      procedure ExecORH; inline;
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
      procedure ExecRLCA; inline;
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
      procedure ExecSUBA; inline;
      procedure ExecSUBB; inline;
      procedure ExecSUBC; inline;
      procedure ExecSUBD; inline;
      procedure ExecSUBE; inline;
      procedure ExecSUBH; inline;
      procedure ExecSUBimm; inline;
      procedure ExecSUBL; inline;
      procedure ExecSUBM; inline;
      procedure ExecSub(_b: byte; _states: integer; _save: boolean = True); inline;
      procedure ExecXORA; inline;
      procedure ExecXORB; inline;
      procedure ExecXORC; inline;
      procedure ExecXORD; inline;
      procedure ExecXORE; inline;
      procedure ExecXORH; inline;
      procedure ExecXORL; inline;
      procedure ExecXORM; inline;
      procedure Interrupt(_vec: byte);    // Perform interrupt, use vector
      procedure ProcessPortOut(_port, _byte: byte);
      procedure PushWord(_word: Word); inline;
      procedure SetCPUspeed(_speed: int64);
      procedure SetOnTransmitA(_proc: TSIOtransmitProc);
      procedure SetPCrelative(_b: byte); inline;
      procedure SetProcessorState(_ps: TProcessorState);
      procedure SetXORflags; inline;
    protected
      procedure Execute; override;
    public
      SavedPC: Word;
      constructor Create;
      destructor Destroy; override;
      procedure ChannelReceiveA(_byte: byte);
      function  ExecuteOneInst: boolean;     // Execute one instruction
//    function  ExecuteOver: boolean;     // Execute over the next instruction
      procedure ExecuteRun;
      procedure ExecuteStepInto;          // Execute one step
      procedure ExecuteStepOver;          // Step over next instruction
      procedure ExecuteStop;              // Stop processor if it's running
      procedure Init;                     // Initialise COLD
      procedure ReadFromStream(_strm: TStream; _start, _length: integer);
      property CPUspeed: int64 read cpu_speed write SetCPUspeed;
      property ErrorFlag: TErrorFlags read error_flag;
      property ErrorString: string read FErrorString;
      property OnStateChange: TStateChangeProc read FOnStateChange write FOnStateChange;
      property OnTransmitA: TSIOtransmitProc read FOnTransmitA write SetOnTransmitA;
      property PC: Word read regset.registers[regPC];
      property PerfMHz: double read GetPerfMHz;
      property PerfMIPS: double read GetPerfMIPS;
      property ProcessorState: TProcessorState read FProcessorState write SetProcessorState;
      property RAM: TRAMarray read ramarray;
      property RegisterSet: TRegisterSet read GetRegisterSet;
      property TStates: int64 read t_states write t_states;
  end;





{$IFDEF EXEC_TRACE}
var
  exec_trace_log: array[0..EXEC_TRACE_SIZE-1] of TRegisterSet;
{$ENDIF}


implementation

constructor TProcessor.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FProcessorState := psNone;
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
end;

destructor TProcessor.Destroy;
begin
  FreeAndNil(SIO);
  inherited Destroy;
end;

{$RANGECHECKS OFF}  // Range checking is off so we can roll over registers

procedure TProcessor.ChannelReceiveA(_byte: byte);
begin
  SIO.ChannelA.Received := _byte;
end;

procedure TProcessor.ExecAdd(_b: byte; _states: integer); inline;
var _a,_c: byte;
    flags: byte;
begin
  _a    := pregA^;
  flags := pregF^;
  if (_b + _a) >= $100 then
    _c := _a + _b - $100
  else
    _c := _a + _b;
  flags := (flags and NOT_FLAG_NEGATIVE) or (_c and FLAG_NEGATIVE);
  if _c = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;
  if (_b and $0F) + (_a and $0F) >= $10 then
    flags := flags or FLAG_HALFCARRY
  else
    flags := flags and NOT_FLAG_HALFCARRY;
  flags := flags and NOT_FLAG_PV; // Clear overflow for now
  if ((_a xor _b) and $80) = 0 then // check for alike signs
      if ((_a xor _c) and $80) <> $00 then
        flags := flags or FLAG_PV;
  flags := flags and NOT_FLAG_SUBTRACT;
  if (_b + _a) >= $100 then
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

procedure TProcessor.ExecADDL; inline;
begin
  ExecAdd(pregL^,4);
end;

procedure TProcessor.ExecADDM; inline;
begin
  ExecAdd(ramarray[pregHL^],7);
end;

procedure TProcessor.ExecANDimm; inline;
begin
  pregA^ := pregA^ and Fetch8;
  SetXORflags;
  Inc(t_states,7);
end;

procedure TProcessor.ExecANDA; inline;
begin
  pregA^ := pregA^ and pregA^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDB;
begin
  pregA^ := pregA^ and pregB^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDC;
begin
  pregA^ := pregA^ and pregC^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDD;
begin
  pregA^ := pregA^ and pregD^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDE;
begin
  pregA^ := pregA^ and pregE^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDH;
begin
  pregA^ := pregA^ and pregH^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDL;
begin
  pregA^ := pregA^ and pregL^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecANDM;
begin
  pregA^ := pregA^ and ramarray[pregHL^];
  SetXORflags;
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

procedure TProcessor.ExecCPimm; inline;
begin
  ExecSub(Fetch8,7,False); // Subtract immediate from a, throw away results
end;

procedure TProcessor.ExecDECA; inline;
begin
  Dec(pregA^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecDECB; inline;
begin
  Dec(pregB^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecDECC; inline;
begin
  Dec(pregC^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecDECD; inline;
begin
  Dec(pregD^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecDECE; inline;
begin
  Dec(pregE^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecDECH; inline;
begin
  Dec(pregH^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecDECL; inline;
begin
  Dec(pregL^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecDECM; inline;
begin
  Dec(ramarray[pregHL^]);
  Inc(t_states,11);
end;

procedure TProcessor.ExecDI; inline;
begin
  pregIntE^ := 0;
  Inc(t_states,4);
end;

procedure TProcessor.ExecEI; inline;
begin
  pregIntE^ := 1;
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

procedure TProcessor.ExecINCA; inline;
begin
  Inc(pregA^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecINCB; inline;
begin
  Inc(pregB^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecINCC; inline;
begin
  Inc(pregC^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecINCD; inline;
begin
  Inc(pregD^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecINCE; inline;
begin
  Inc(pregE^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecINCH; inline;
begin
  Inc(pregH^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecINCL; inline;
begin
  Inc(pregL^);
  Inc(t_states,4);
end;

procedure TProcessor.ExecINCM; inline;
begin
  Inc(ramarray[pregHL^]);
  Inc(t_states,11);
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

procedure TProcessor.ExecLDAB; inline;
begin
  pregA^ := pregB^;
  Inc(t_states,4);
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

procedure TProcessor.ExecLDSPimm; inline;
begin
  pregSP^ := Fetch16;
  Inc(t_states,10);
end;

procedure TProcessor.ExecORA; inline;
begin
  pregA^ := pregA^ or pregA^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecORB; inline;
begin
  pregA^ := pregA^ or pregB^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecORC; inline;
begin
  pregA^ := pregA^ or pregC^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecORD; inline;
begin
  pregA^ := pregA^ or pregD^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecORE; inline;
begin
  pregA^ := pregA^ or pregE^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecORH; inline;
begin
  pregA^ := pregA^ or pregH^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecORL; inline;
begin
  pregA^ := pregA^ or pregL^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecORM; inline;
begin
  pregA^ := pregA^ or ramarray[pregHL^];
  SetXORflags;
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

procedure TProcessor.ExecSub(_b: byte; _states: integer; _save: boolean = True); inline;
var _a,_c: byte;
    flags: byte;
begin
  _a    := pregA^;
  flags := pregF^;
  if _b > _a then
    _c := _a + $100 - _b
  else
    _c := _a - _b;
  flags := (flags and NOT_FLAG_NEGATIVE) or (_c and FLAG_NEGATIVE);
  if _c = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;
  if (_b and $0F) > (_a and $0F) then
    flags := flags or FLAG_HALFCARRY
  else
    flags := flags and NOT_FLAG_HALFCARRY;
  flags := flags and NOT_FLAG_PV; // Clear overflow for now
  if ((_a xor _b) and $80) <> 0 then // check for unalike signs
      if ((_a xor _c) and $80) <> 0 then
        flags := flags or FLAG_PV;
  flags := flags or FLAG_SUBTRACT;
  if _b > _a then
    flags := flags or FLAG_CARRY
  else
    flags := flags and NOT_FLAG_CARRY;
  pregF^ := flags;
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
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORB; inline;
begin
  pregA^ := pregA^ xor pregB^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORC; inline;
begin
  pregA^ := pregA^ xor pregC^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORD; inline;
begin
  pregA^ := pregA^ xor pregD^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORE; inline;
begin
  pregA^ := pregA^ xor pregE^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORH; inline;
begin
  pregA^ := pregA^ xor pregH^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORL; inline;
begin
  pregA^ := pregA^ xor pregL^;
  SetXORflags;
  Inc(t_states,4);
end;

procedure TProcessor.ExecXORM; inline;
begin
  pregA^ := pregA^ xor ramarray[pregHL^];;
  SetXORflags;
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
  case _port of
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

procedure TProcessor.SetCPUspeed(_speed: int64);
begin
  cpu_speed := _speed;
  run_start_time := Now();
  run_start_cycles := TStates;
  run_start_instructions := insts;
end;

procedure TProcessor.SetPCrelative(_b: byte); inline;
var newpc: Word;
begin
  newpc := pregPC^;  // Start off with current PC
  if (_b and $80) <> 0 then
    newpc := newpc + _b - $100
  else
    newpc := newpc + _b;
  pregPC^ := newpc;
end;

procedure TProcessor.SetProcessorState(_ps: TProcessorState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(_ps);
  FProcessorState := _ps;
end;

procedure TProcessor.SetOnTransmitA(_proc: TSIOtransmitProc);
begin
  SIO.ChannelA.OnTransmit := _proc;
end;

procedure TProcessor.SetXORflags; inline;
var flags: byte;
begin
  flags := pregF^ and (NOT_FLAG_HALFCARRY and NOT_FLAG_NEGATIVE and NOT_FLAG_CARRY); // Reset H/N/C
  flags := (flags and $7F) or (pregA^ and FLAG_NEGATIVE); // Bit 7 is neg flag
  if pregA^ = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;      // Bit 6 is zero flag
  flags := flags and NOT_FLAG_PV;        // Kill parity flag for now
  flags := flags or parity_table[pregA^];
  pregF^ := flags;
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
  opcode := ramarray[pregPC^];
  Inc(pregPC^);
  proc := inst_cb[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
end;

procedure TProcessor.ExecCbBITreg; inline;
var _src: byte;
    _mask: byte;
    _reg: byte;
    _bit: byte;
    flags: byte;
begin
  _reg := (opcode shr 3) and $07;
  _bit := opcode and $07;
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
    flags := flags or FLAG_ZERO;
  pregF^ := flags;
  // Bump the t_states
  if _reg <> 6 then
    Inc(t_states,8)
  else
    Inc(t_states,12);
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
  opcode := ramarray[pregPC^];
  Inc(pregPC^);
  proc := inst_ed[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
end;

procedure TProcessor.ExecEdIM2; inline;
begin
  pregIntM^ := 2;
  Inc(t_states,8);
end;

procedure TProcessor.ExecEdLDIA; inline;
begin
  pregI^ := pregA^;
  Inc(t_states,9);
end;

procedure TProcessor.ExecEdRETI; inline;
begin
  pregPC^ := PopWord;
  Inc(t_states,14);
end;


//-----------------------------------------------------------------------------
//
// Processor main execution loop
//
//-----------------------------------------------------------------------------

procedure TProcessor.Execute;
var elapsed_time:   double; // Number of seconds elapsed since run started
    simulated_time: double; // Number of simulated seconds elapsed since run started
    check:          integer;
    i:              integer;
    check_every:    integer;
begin
  check := 0;
  while (not Terminated) do
    begin
      case FProcessorState of
        psStepInto:
          begin
            ExecuteOneInst;
            if error_flag <> [] then
              ProcessorState := psFault;
            ProcessorState := psPaused;
          end;
        psRunning:
          begin
            i := 0;
            if CPUspeed > 10000000 then
              check_every := 100000
            else if CPUspeed > 1000000 then
              check_every := 10000
            else if CPUspeed > 100000 then
              check_every := 1000
            else
              check_every := 250;
            while (i < CHECK_EVERY) and (FProcessorState = psRunning) and (error_flag = []) do
              begin
                ExecuteOneInst;
                Inc(i);
              end;
            if error_flag <> [] then
              ProcessorState := psFault
            else
              begin
                // Check timing and see if we need to insert a sleep()
                check := 0;
                elapsed_time := 86400.0 * (Now() - run_start_time);
                simulated_time := (TStates - run_start_cycles) / CpuSpeed;
                if simulated_time > elapsed_time then
                  sleep(Trunc((simulated_time-elapsed_time)*1000.0+0.5));
              end;
          end;
        otherwise
          sleep(20);
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
  // Check if interrupt waiting
  if int_flag and (pregIntE^ <> 0) then
    begin
      int_flag := False; // Reset the flag!
      vector := (pregI^ shl 8) or (int_vec and $FE);
      addr := ramarray[vector] or (ramarray[vector+1] shl 8);
      PushWord(pregPC^);
      pregPC^ := addr;
      Inc(t_states,17); // @@@@@ Find out t states for processing interrupt
    end
  else
    begin
      // Get first byte to execute
      opcode := ramarray[pregPC^];
      Inc(pregPC^);
      proc := inst_std[opcode];
      if proc <> nil then
        proc
      else
        error_flag := error_flag + [efIllegal];
    end;
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
      ProcessorState := psStepOver;
    end;
end;

procedure TProcessor.ExecuteStop;
begin
  if ProcessorState = psRunning then
    ProcessorState := psPaused;
end;

// Cold boot or hard reset

procedure TProcessor.Init;
var i: word;
    b: byte;
    n: byte;
    ri: TRegIndex;
begin
  SIO.Init;             // Initialise SIO
  t_states := 0;
  cpu_speed := 4000000; // Default to 4MHz device
  pregIntE^ := 1;       // Enable interrupts
  pregIntM^ := 0;       // Interrupt mode 0 (IM0) like 8080
  for ri in TRegIndex do
    regset.registers[ri] := Word(Random(65536));
  pregPC^ := 0;
  pregIR^ := 0;
  for i in word do
    ramarray[i] := Byte(Random(256));
  // Set up parity table
  for b in byte do
    begin
      n := (b shr 4) xor (b and $0F);
      n := (n shr 2) xor (n and $03);
      n := (n shr 1) xor (n and $01);
      parity_table[b] := (n shl 2) xor FLAG_PV;
    end;

  for b in byte do inst_std[b] := nil;
  for b in byte do inst_cb[b]  := nil;
  for b in byte do inst_ed[b]  := nil;
  // Set up standard instructions
  inst_std[$01] := @ExecLDBCimm;
  inst_std[$03] := @ExecINCBC;
  inst_std[$04] := @ExecINCB;
  inst_std[$05] := @ExecDECB;
  inst_std[$06] := @ExecLDBimm;
  inst_std[$07] := @ExecRLCA;
  inst_std[$0C] := @ExecINCC;
  inst_std[$0D] := @ExecDECC;
  inst_std[$0E] := @ExecLDCimm;
  inst_std[$0F] := @ExecRRCA;
  inst_std[$11] := @ExecLDDEimm;
  inst_std[$13] := @ExecINCDE;
  inst_std[$14] := @ExecINCD;
  inst_std[$15] := @ExecDECD;
  inst_std[$16] := @ExecLDDimm;
  inst_std[$18] := @ExecJR;
  inst_std[$1C] := @ExecINCE;
  inst_std[$1D] := @ExecDECE;
  inst_std[$1E] := @ExecLDEimm;
  inst_std[$20] := @ExecJRNZ;
  inst_std[$21] := @ExecLDHLimm;
  inst_std[$22] := @ExecLDaddrHL;
  inst_std[$23] := @ExecINCHL;
  inst_std[$24] := @ExecINCH;
  inst_std[$25] := @ExecDECH;
  inst_std[$26] := @ExecLDHimm;
  inst_std[$28] := @ExecJRZ;
  inst_std[$2A] := @ExecLDHLaddr;
  inst_std[$2C] := @ExecINCL;
  inst_std[$2D] := @ExecDECL;
  inst_std[$2E] := @ExecLDLimm;
  inst_std[$30] := @ExecJRNC;
  inst_std[$31] := @ExecLDSPimm;
  inst_std[$32] := @ExecLDaddrA;
  inst_std[$33] := @ExecINCSP;
  inst_std[$34] := @ExecINCM;
  inst_std[$35] := @ExecDECM;
  inst_std[$38] := @ExecJRC;
  inst_std[$3A] := @ExecLDAaddr;
  inst_std[$3C] := @ExecINCA;
  inst_std[$3D] := @ExecDECA;
  inst_std[$3E] := @ExecLDAimm;
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
  inst_std[$80] := @ExecADDB;
  inst_std[$81] := @ExecADDC;
  inst_std[$82] := @ExecADDD;
  inst_std[$83] := @ExecADDE;
  inst_std[$84] := @ExecADDH;
  inst_std[$85] := @ExecADDL;
  inst_std[$86] := @ExecADDM;
  inst_std[$87] := @ExecADDA;
  inst_std[$90] := @ExecSUBB;
  inst_std[$91] := @ExecSUBC;
  inst_std[$92] := @ExecSUBD;
  inst_std[$93] := @ExecSUBE;
  inst_std[$94] := @ExecSUBH;
  inst_std[$95] := @ExecSUBL;
  inst_std[$96] := @ExecSUBM;
  inst_std[$97] := @ExecSUBA;
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
  inst_std[$C0] := @ExecRETNZ;
  inst_std[$C1] := @ExecPOPBC;
  inst_std[$C2] := @ExecJPNZ;
  inst_std[$C3] := @ExecJPabs;
  inst_std[$C5] := @ExecPUSHBC;
  inst_std[$C7] := @ExecRST00;
  inst_std[$C8] := @ExecRETZ;
  inst_std[$C9] := @ExecRET;
  inst_std[$CA] := @ExecJPZ;
  inst_std[$CB] := @ExecCb;
  inst_std[$CD] := @ExecCALLabs;
  inst_std[$CF] := @ExecRST08;
  inst_std[$D0] := @ExecRETNC;
  inst_std[$D1] := @ExecPOPDE;
  inst_std[$D2] := @ExecJPNC;
  inst_std[$D3] := @ExecOUTportA;
  inst_std[$D5] := @ExecPUSHDE;
  inst_std[$D6] := @ExecSUBimm;
  inst_std[$D7] := @ExecRST10;
  inst_std[$D8] := @ExecRETC;
  inst_std[$DA] := @ExecJPC;
  inst_std[$DB] := @ExecINAport;
  inst_std[$DF] := @ExecRST18;
  inst_std[$E0] := @ExecRETPO;
  inst_std[$E1] := @ExecPOPHL;
  inst_std[$E2] := @ExecJPPO;
  inst_std[$E5] := @ExecPUSHHL;
  inst_std[$E6] := @ExecANDimm;
  inst_std[$E7] := @ExecRST20;
  inst_std[$E8] := @ExecRETPE;
  inst_std[$EA] := @ExecJPPE;
  inst_std[$ED] := @ExecEd;
  inst_std[$EF] := @ExecRST28;
  inst_std[$F0] := @ExecRETP;
  inst_std[$F1] := @ExecPOPAF;
  inst_std[$F2] := @ExecJPP;
  inst_std[$F3] := @ExecDI;
  inst_std[$F5] := @ExecPUSHAF;
  inst_std[$F7] := @ExecRST30;
  inst_std[$F8] := @ExecRETM;
  inst_std[$FA] := @ExecJPM;
  inst_std[$FB] := @ExecEI;
  inst_std[$FE] := @ExecCPimm;
  inst_std[$FF] := @ExecRST38;
  // Set up CB instructions
  for b := $40 to $7F do inst_cb[b] := @ExecCbBITreg;
  // Set up ED instructions
  inst_ed[$47] := @ExecEdLDIA;
  inst_ed[$4D] := @ExecEdRETI;
  inst_ed[$5E] := @ExecEdIM2;
end;

procedure TProcessor.Interrupt(_vec:byte);
begin
  int_vec := _vec;
  int_flag := True; // Flag the interrupt
end;

end.

