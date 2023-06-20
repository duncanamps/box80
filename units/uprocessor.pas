unit uprocessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBreakpoint = (bpNone,bpTemporary,bpPermanent);

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
  t_states:      int64;
  cpu_speed:     int64;  // In Hz

  parity_table: array[byte] of byte;
  inst_std: array[byte] of procedure;


function  processor_execute: boolean;   // Execute one instruction
procedure processor_init;               // Initialise COLD


implementation

procedure BumpPC; inline;
begin
  if pc = $FFFF then
    pc := 0
  else
    Inc(pc);
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

procedure SetXORflags; inline;
var flags: byte;
begin
  flags := af and $00EC; // Reset H/N/C
  flags := (flags and $7F) or ((af shr 8) and $80); // Bit 7 is neg flag
  if (af and $FF00) = 0 then
    flags := flags or $40
  else
    flags := flags and $BF;      // Bit 6 is zero flag
  flags := flags and $FB;        // Kill parity flag for now
  flags := flags or parity_table[af shr 8];
  af := (af and $FF00) or flags;
end;

//=============================================================================

procedure ExecDI;
begin
  int_enabled := False;
  Inc(t_states,4);
end;

procedure ExecJPabs;
var addr: Word;
begin
  addr := Fetch16;
  pc := addr;
  Inc(t_states,10);
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

procedure ExecLDAimm;
begin
  af := (af and $00FF) or (Fetch8 shl 8);
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

procedure ExecOUTportA;
var port: byte;
begin
  // @@@@@ DO PORT HANDLING FUNCTIONS
  // e.g.  ProcessPortOut(port,af shr 8);
  port := Fetch8;
  Inc(t_states,11);
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

// Processor execute 1 instruction. If the instruction is a CPIR LDIR etc.,
// one cycle will be executed. Returns False if the instruction could not
// be executed

function processor_execute: boolean;
var inst: byte;
    proc: procedure;
begin
  Result := True;
  // Get first byte to execute
  inst := ramarray[pc];
  BumpPC;
  proc := inst_std[inst];
  if proc <> nil then
    proc
  else
    Result := False;
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

  for b in byte do
    inst_std[b] := nil;
  // Set up standard instructions
  inst_std[$01] := @ExecLDBCimm;
  inst_std[$06] := @ExecLDBimm;
  inst_std[$0E] := @ExecLDCimm;
  inst_std[$11] := @ExecLDDEimm;
  inst_std[$16] := @ExecLDDimm;
  inst_std[$1E] := @ExecLDEimm;
  inst_std[$21] := @ExecLDHLimm;
  inst_std[$22] := @ExecLDaddrHL;
  inst_std[$26] := @ExecLDHimm;
  inst_std[$2E] := @ExecLDLimm;
  inst_std[$31] := @ExecLDSPimm;
  inst_std[$32] := @ExecLDaddrA;
  inst_std[$3E] := @ExecLDAimm;
  inst_std[$C3] := @ExecJPabs;
  inst_std[$A8] := @ExecXORB;
  inst_std[$A9] := @ExecXORC;
  inst_std[$AA] := @ExecXORD;
  inst_std[$AB] := @ExecXORE;
  inst_std[$AC] := @ExecXORH;
  inst_std[$AD] := @ExecXORL;
  inst_std[$AF] := @ExecXORA;
  inst_std[$D3] := @ExecOUTportA;
  inst_std[$F3] := @ExecDI;
end;

end.

