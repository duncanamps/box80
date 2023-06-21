unit uprocessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

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



type
  TBreakpoint = (bpNone,bpTemporary,bpPermanent);

  TErrorFlag = (efIllegal);
  TErrorFlags = set of TErrorFlag;

  TPortInFunction = function(_portno: Word): byte of object;
  TPOrtOutProcedure = procedure(_portno: Word; _value: byte) of object;


var
  ramarray:     array[Word] of byte;
  bparray:      array[Word] of TBreakPoint;
  portinarray:  array[Word] of TPortInFunction;
  portoutarray: array[Word] of TPortOutProcedure;
  af:  word;
  bc:  word;
  de:  word;
  hl:  word;
  af_: word;
  bc_: word;
  de_: word;
  hl_: word;
  ir:  word;
  ix:  word;
  iy:  word;
  sp:  word;
  pc:  word;

  int_enabled:   boolean;
  int_mode:      byte;
  t_states:      int64;
  cpu_speed:     int64;  // In Hz
  error_flag:    TErrorFlags;
  opcode:        byte;

  parity_table: array[byte] of byte;
  inst_std: array[byte] of procedure;
  inst_cb:  array[byte] of procedure;
  inst_ed:  array[byte] of procedure;


function  processor_execute: boolean;   // Execute one instruction
procedure processor_init;               // Initialise COLD


implementation

// Utilities

procedure BumpPC; inline;
begin
  if pc = $FFFF then
    pc := 0
  else
    Inc(pc);
end;

procedure BumpPCrelative(_b: byte); inline;
begin
  if (_b and $80) <> 0 then
    pc := pc + _b - 256
  else
    pc := pc + _b;
end;

function Fetch8: Word; inline;
begin
  Result := ramarray[PC];
  BumpPC;
end;

function Fetch16: Word; inline;
begin
  Result := ramarray[PC] + (ramarray[PC+1] shl 8);
  BumpPC;
  BumpPC;
end;

// Helper routines

procedure ExecJRcond(_mask, _required: byte); inline;
var b:     byte;
begin
  b := Fetch8;
  if (af and _mask) = _required then
    begin // Succeeded
      BumpPCrelative(b);
      Inc(t_states,12);
    end
  else
    Inc(t_states,7); // Failed
end;

procedure ExecRETcond(_mask, _required: byte); inline;
var newpc: word;
begin
  if (af and _mask) = _required then
    begin // Succeeded
      newpc := ramarray[sp];
      Inc(sp);
      newpc := newpc or ramarray[sp] shl 8;
      Inc(sp);
      pc := newpc;
      Inc(t_states,11);
    end
  else
    Inc(t_states,5); // Failed
end;

procedure ExecRST(_addr: byte); inline;
begin
  Dec(sp);
  ramarray[sp] := pc shr 8;
  Dec(sp);
  ramarray[sp] := pc and $00FF;
  pc := _addr;
  Inc(t_states,11);
end;

procedure ExecSub(_b: byte; _states: integer; _save: boolean = True);
var _a,_c: byte;
    flags: byte;
begin
  _a := af shr 8;
  flags := af and $00FF;
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
  af := (af and $FF00) or flags;
  if _save then
    af := (af and $00FF) or (_a shl 8);
  Inc(t_states,_states);
end;

procedure SetXORflags; inline;
var flags: byte;
begin
  flags := af and (NOT_FLAG_HALFCARRY and NOT_FLAG_NEGATIVE and NOT_FLAG_CARRY); // Reset H/N/C
  flags := (flags and $7F) or ((af shr 8) and FLAG_NEGATIVE); // Bit 7 is neg flag
  if (af and $FF00) = 0 then
    flags := flags or FLAG_ZERO
  else
    flags := flags and NOT_FLAG_ZERO;      // Bit 6 is zero flag
  flags := flags and NOT_FLAG_PV;        // Kill parity flag for now
  flags := flags or parity_table[af shr 8];
  af := (af and $FF00) or flags;
end;

//=============================================================================

procedure ExecANDA;
begin
  af := af and ($00FF or (af and $FF00));
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecANDB;
begin
  af := af and ($00FF or (bc and $FF00));
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecANDC;
begin
  af := af and ($00FF or ((bc and $00FF) shl 8));
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecANDD;
begin
  af := af and ($00FF or (de and $FF00));
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecANDE;
begin
  af := af and ($00FF or ((de and $00FF) shl 8));
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecANDH;
begin
  af := af and ($00FF or (hl and $FF00));
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecANDL;
begin
  af := af and ($00FF or ((hl and $00FF) shl 8));
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecANDM;
begin
  af := af and ($00FF or (ramarray[hl] shl 8));
  SetXORflags;
  Inc(t_states,7);
end;

procedure ExecCALLabs;
var addr: Word;
begin
  addr := Fetch16;
  Dec(sp);
  ramarray[sp] := pc shr 8;
  Dec(sp);
  ramarray[sp] := pc and $00FF;
  pc := addr;
  Inc(t_states,17);
end;

procedure ExecCPimm;
begin
  ExecSub(Fetch8,7,False); // Subtract immediate from a, throw away results
end;

procedure ExecDI;
begin
  int_enabled := False;
  Inc(t_states,4);
end;

procedure ExecEI;
begin
  int_enabled := True;
  Inc(t_states,4);
end;

procedure ExecINAport;
var port: byte;
begin
  // @@@@@ DO PORT HANDLING FUNCTIONS
  // e.g.  ProcessPortOut(port,af shr 8);
  port := Fetch8;
  Inc(t_states,11);
end;

procedure ExecINCBC;
begin
  Inc(bc);
  Inc(t_states,6);
end;

procedure ExecINCDE;
begin
  Inc(de);
  Inc(t_states,6);
end;

procedure ExecINCHL;
begin
  Inc(hl);
  Inc(t_states,6);
end;

procedure ExecINCSP;
begin
  Inc(sp);
  Inc(t_states,6);
end;

procedure ExecJPabs;
var addr: Word;
begin
  addr := Fetch16;
  pc := addr;
  Inc(t_states,10);
end;

procedure ExecJR;
var b: byte;
begin
  b := Fetch8;
  BumpPCrelative(b);
  Inc(t_states,12);
end;

procedure ExecJRC;
begin
  ExecJRcond(FLAG_CARRY,FLAG_CARRY);
end;

procedure ExecJRNC;
begin
  ExecJRcond(FLAG_CARRY,0);
end;

procedure ExecJRNZ;
begin
  ExecJRcond(FLAG_ZERO,0);
end;

procedure ExecJRZ;
begin
  ExecJRcond(FLAG_ZERO,FLAG_ZERO);
end;

procedure ExecLDaddrA;
var addr: Word;
begin
  addr := Fetch16;
  ramarray[addr] := af shr 8;
  Inc(t_states,13);
end;

procedure ExecLDaddrHL;
var addr: Word;
begin
  addr := Fetch16;
  ramarray[addr]   := hl and $FF;
  ramarray[addr+1] := hl shr 8;
  Inc(t_states,16);
end;

procedure ExecLDAaddr;
var addr: Word;
begin
  addr := Fetch16;
  af := (af and $00FF) or (ramarray[addr] shl 8);
  Inc(t_states,13);
end;

procedure ExecLDAimm;
begin
  af := (af and $00FF) or (Fetch8 shl 8);
  Inc(t_states,7);
end;

procedure ExecLDAM;
begin
  af := (af and $00FF) or (ramarray[hl] shl 8);
  Inc(t_states,7);
end;

procedure ExecLDBimm;
begin
  bc := (bc and $00FF) or (Fetch8 shl 8);
  Inc(t_states,7);
end;

procedure ExecLDBCimm;
begin
  bc := Fetch16;
  Inc(t_states,10);
end;

procedure ExecLDCimm;
begin
  bc := (bc and $FF00) or Fetch8;
  Inc(t_states,7);
end;

procedure ExecLDDimm;
begin
  de := (de and $00FF) or (Fetch8 shl 8);
  Inc(t_states,7);
end;

procedure ExecLDDEimm;
begin
  de := Fetch16;
  Inc(t_states,10);
end;

procedure ExecLDEimm;
begin
  de := (de and $FF00) or Fetch8;
  Inc(t_states,7);
end;

procedure ExecLDHimm;
begin
  hl := (hl and $00FF) or (Fetch8 shl 8);
  Inc(t_states,7);
end;

procedure ExecLDHLimm;
begin
  hl := Fetch16;
  Inc(t_states,10);
end;

procedure ExecLDLimm;
begin
  hl := (hl and $FF00) or Fetch8;
  Inc(t_states,7);
end;

procedure ExecLDSPimm;
begin
  sp := Fetch16;
  Inc(t_states,10);
end;

procedure ExecORA;
begin
  af := af or (af and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecORB;
begin
  af := af or (bc and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecORC;
begin
  af := af or ((bc and $00FF) shl 8);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecORD;
begin
  af := af or (de and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecORE;
begin
  af := af or ((de and $00FF) shl 8);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecORH;
begin
  af := af or (hl and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecORL;
begin
  af := af or ((hl and $00FF) shl 8);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecORM;
begin
  af := af or (ramarray[hl] shl 8);
  SetXORflags;
  Inc(t_states,7);
end;

procedure ExecOUTportA;
var port: byte;
begin
  // @@@@@ DO PORT HANDLING FUNCTIONS
  // e.g.  ProcessPortOut(port,af shr 8);
  port := Fetch8;
  Inc(t_states,11);
end;

procedure ExecPOPAF;
begin
  af := ramarray[sp];
  Inc(sp);
  af := af or (ramarray[sp] shl 8);
  Inc(sp);
  Inc(t_states,10);
end;

procedure ExecPOPBC;
begin
  bc := ramarray[sp];
  Inc(sp);
  bc := bc or (ramarray[sp] shl 8);
  Inc(sp);
  Inc(t_states,10);
end;

procedure ExecPOPDE;
begin
  de := ramarray[sp];
  Inc(sp);
  de := de or (ramarray[sp] shl 8);
  Inc(sp);
  Inc(t_states,10);
end;

procedure ExecPOPHL;
begin
  hl := ramarray[sp];
  Inc(sp);
  hl := hl or (ramarray[sp] shl 8);
  Inc(sp);
  Inc(t_states,10);
end;

procedure ExecPUSHAF;
begin
  Dec(sp);
  ramarray[sp] := af shr 8;
  Dec(sp);
  ramarray[sp] := af and $00ff;
  Inc(t_states,11);
end;

procedure ExecPUSHBC;
begin
  Dec(sp);
  ramarray[sp] := bc shr 8;
  Dec(sp);
  ramarray[sp] := bc and $00ff;
  Inc(t_states,11);
end;

procedure ExecPUSHDE;
begin
  Dec(sp);
  ramarray[sp] := de shr 8;
  Dec(sp);
  ramarray[sp] := de and $00ff;
  Inc(t_states,11);
end;

procedure ExecPUSHHL;
begin
  Dec(sp);
  ramarray[sp] := hl shr 8;
  Dec(sp);
  ramarray[sp] := hl and $00ff;
  Inc(t_states,11);
end;

procedure ExecRET;
var newpc: word;
begin
  newpc := ramarray[sp];
  Inc(sp);
  newpc := newpc or ramarray[sp] shl 8;
  Inc(sp);
  pc := newpc;
  Inc(t_states,10);
end;

procedure ExecRETC;
begin
  ExecRETcond(FLAG_CARRY,FLAG_CARRY);
end;

procedure ExecRETM;
begin
  ExecRETcond(FLAG_NEGATIVE,FLAG_NEGATIVE);
end;

procedure ExecRETNC;
begin
  ExecRETcond(FLAG_CARRY,0);
end;

procedure ExecRETP;
begin
  ExecRETcond(FLAG_NEGATIVE,0);
end;

procedure ExecRETPE;
begin
  ExecRETcond(FLAG_PV,FLAG_PV);
end;

procedure ExecRETPO;
begin
  ExecRETcond(FLAG_PV,0);
end;

procedure ExecRETNZ;
begin
  ExecRETcond(FLAG_ZERO,0);
end;

procedure ExecRETZ;
begin
  ExecRETcond(FLAG_ZERO,FLAG_ZERO);
end;

procedure ExecRRCA;
var bit0: word;
begin
  bit0 := (af and $0100);
  af := (af and $00FF) or ((af and $FE) shr 1) or (bit0 shl 7);
  af := (af and ($FF00 or NOT_FLAG_CARRY)) or (bit0 shr 8); // Set C flag
  Inc(t_states,4);
end;

procedure ExecRST00;
begin
  ExecRST($00);
end;

procedure ExecRST08;
begin
  ExecRST($08);
end;

procedure ExecRST10;
begin
  ExecRST($10);
end;

procedure ExecRST18;
begin
  ExecRST($18);
end;

procedure ExecRST20;
begin
  ExecRST($20);
end;

procedure ExecRST28;
begin
  ExecRST($28);
end;

procedure ExecRST30;
begin
  ExecRST($30);
end;

procedure ExecRST38;
begin
  ExecRST($38);
end;

procedure ExecSUBA;
begin
  ExecSub(af shr 8,4);
end;

procedure ExecSUBB;
begin
  ExecSub(bc shr 8,4);
end;

procedure ExecSUBC;
begin
  ExecSub(bc and $00FF,4);
end;

procedure ExecSUBD;
begin
  ExecSub(de shr 8,4);
end;

procedure ExecSUBE;
begin
  ExecSub(de and $00FF,4);
end;

procedure ExecSUBH;
begin
  ExecSub(hl shr 8,4);
end;

procedure ExecSUBL;
begin
  ExecSub(hl and $00FF,4);
end;

procedure ExecSUBM;
begin
  ExecSub(ramarray[hl],7);
end;

procedure ExecXORA;
begin
  af := af xor (af and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecXORB;
begin
  af := af xor (bc and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecXORC;
begin
  af := af xor ((bc and $00FF) shl 8);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecXORD;
begin
  af := af xor (de and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecXORE;
begin
  af := af xor ((de and $00FF) shl 8);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecXORH;
begin
  af := af xor (hl and $FF00);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecXORL;
begin
  af := af xor ((hl and $00FF) shl 8);
  SetXORflags;
  Inc(t_states,4);
end;

procedure ExecXORM;
begin
  af := af xor (ramarray[hl] shl 8);
  SetXORflags;
  Inc(t_states,7);
end;

//=============================================================================
//
// CB instructions
//
//=============================================================================

procedure ExecCb;
var proc: procedure;
begin
  // Get byte to execute
  opcode := ramarray[pc];
  BumpPC;
  proc := inst_cb[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
end;

procedure ExecCbBITreg;
var _src: byte;
    _mask: byte;
    _reg: byte;
    _bit: byte;
    flags: byte;
begin
  _reg := (opcode shr 3) and $07;
  _bit := opcode and $07;
  case _reg of
    0: _src := bc shr 8;
    1: _src := bc and $00FF;
    2: _src := de shr 8;
    3: _src := de and $00FF;
    4: _src := hl shr 8;
    5: _src := hl and $00FF;
    6: _src := ramarray[hl];
    7: _src := af and $00FF;
  end;
  _mask := 1 shl _bit;
  // Set the flags
  flags := af and $00FF;
  flags := flags and NOT_FLAG_ZERO and NOT_FLAG_SUBTRACT; // Reset flags
  flags := flags or FLAG_HALFCARRY;
  if (_src and _mask) = 0 then
    flags := flags or FLAG_ZERO;
  af := (af and $FF00) or flags;
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

procedure ExecEd;
var proc: procedure;
begin
  // Get byte to execute
  opcode := ramarray[pc];
  BumpPC;
  proc := inst_ed[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
end;

procedure ExecEdIM2;
begin
  int_mode := 2;
  Inc(t_states,8);
end;

procedure ExecEdLDIA;
begin
  ir := (ir and $00FF) or (af and $FF00);
  Inc(t_states,9);
end;

// Processor execute 1 instruction. If the instruction is a CPIR LDIR etc.,
// one cycle will be executed. Returns False if the instruction could not
// be executed

function processor_execute: boolean;
var proc: procedure;
begin
  error_flag := [];
  // Get first byte to execute
  opcode := ramarray[pc];
  BumpPC;
  proc := inst_std[opcode];
  if proc <> nil then
    proc
  else
    error_flag := error_flag + [efIllegal];
  Result := (error_flag = []);
end;

// Cold boot or hard reset

procedure processor_init;
var i: word;
    b: byte;
    n: byte;
begin
  t_states := 0;
  cpu_speed := 4000000;
  int_enabled := True;
  int_mode := 0; // Interrupt mode 0 (IM0) like 8080
  pc := 0;
  ir := 0;
  ix := Word(Random(65536));
  iy := Word(Random(65536));
  sp := Word(Random(65536));
  for i in word do
    ramarray[i] := Byte(Random(256));
  af  := Word(Random(65536));
  bc  := Word(Random(65536));
  de  := Word(Random(65536));
  hl  := Word(Random(65536));
  af_ := Word(Random(65536));
  bc_ := Word(Random(65536));
  de_ := Word(Random(65536));
  hl_ := Word(Random(65536));
  // Set up parity table
  for b in byte do
    begin
      n := (b shr 4) xor (b and $0F);
      n := (n shr 2) xor (n and $03);
      n := (n shr 1) xor (n and $01);
      parity_table[b] := n shl 2;
    end;

  for b in byte do inst_std[b] := nil;
  for b in byte do inst_cb[b]  := nil;
  for b in byte do inst_ed[b]  := nil;
  // Set up standard instructions
  inst_std[$01] := @ExecLDBCimm;
  inst_std[$03] := @ExecINCBC;
  inst_std[$06] := @ExecLDBimm;
  inst_std[$0E] := @ExecLDCimm;
  inst_std[$0F] := @ExecRRCA;
  inst_std[$11] := @ExecLDDEimm;
  inst_std[$13] := @ExecINCDE;
  inst_std[$16] := @ExecLDDimm;
  inst_std[$18] := @ExecJR;
  inst_std[$1E] := @ExecLDEimm;
  inst_std[$20] := @ExecJRNZ;
  inst_std[$21] := @ExecLDHLimm;
  inst_std[$22] := @ExecLDaddrHL;
  inst_std[$23] := @ExecINCHL;
  inst_std[$26] := @ExecLDHimm;
  inst_std[$28] := @ExecJRZ;
  inst_std[$2E] := @ExecLDLimm;
  inst_std[$30] := @ExecJRNC;
  inst_std[$31] := @ExecLDSPimm;
  inst_std[$32] := @ExecLDaddrA;
  inst_std[$33] := @ExecINCSP;
  inst_std[$38] := @ExecJRC;
  inst_std[$3A] := @ExecLDAaddr;
  inst_std[$3E] := @ExecLDAimm;
  inst_std[$C3] := @ExecJPabs;
  inst_std[$7E] := @ExecLDAM;
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
  inst_std[$C5] := @ExecPUSHBC;
  inst_std[$C7] := @ExecRST00;
  inst_std[$C8] := @ExecRETZ;
  inst_std[$C9] := @ExecRET;
  inst_std[$CB] := @ExecCb;
  inst_std[$CD] := @ExecCALLabs;
  inst_std[$CF] := @ExecRST08;
  inst_std[$D0] := @ExecRETNC;
  inst_std[$D1] := @ExecPOPDE;
  inst_std[$D3] := @ExecOUTportA;
  inst_std[$D5] := @ExecPUSHDE;
  inst_std[$D7] := @ExecRST10;
  inst_std[$D8] := @ExecRETC;
  inst_std[$DB] := @ExecINAport;
  inst_std[$DF] := @ExecRST18;
  inst_std[$E0] := @ExecRETPO;
  inst_std[$E1] := @ExecPOPHL;
  inst_std[$E5] := @ExecPUSHHL;
  inst_std[$E7] := @ExecRST20;
  inst_std[$E8] := @ExecRETPE;
  inst_std[$ED] := @ExecEd;
  inst_std[$EF] := @ExecRST28;
  inst_std[$F0] := @ExecRETP;
  inst_std[$F1] := @ExecPOPAF;
  inst_std[$F3] := @ExecDI;
  inst_std[$F5] := @ExecPUSHAF;
  inst_std[$F7] := @ExecRST30;
  inst_std[$F8] := @ExecRETM;
  inst_std[$FB] := @ExecEI;
  inst_std[$FE] := @ExecCPimm;
  inst_std[$FF] := @ExecRST38;
  // Set up CB instructions
  for b := $40 to $7F do inst_cb[b] := @ExecCbBITreg;
  // Set up ED instructions
  inst_ed[$47] := @ExecEdLDIA;
  inst_ed[$5E] := @ExecEdIM2;
end;

end.

