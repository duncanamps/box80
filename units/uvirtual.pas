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

unit uvirtual;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uprocessor, uterminal;

function  LoadVM(const _filename: string; _proc: TProcessor; _terminal: TTerminal): boolean;
procedure SaveVM(const _filename: string; _proc: TProcessor; _terminal: TTerminal);

implementation

uses
  DOM, XMLWrite, XMLRead, usio;

const BYTES_PER_LINE = 32;

function LoadVM(const _filename: string; _proc: TProcessor; _terminal: TTerminal): boolean;
type PWord = ^word;
     PByte = ^byte;
var doc: TXMLDocument;
    rootnode, l1, l2, l3: TDOMNode;
    s: string;
    i,j: integer;
    p: PByte;
    b: byte;
    was_running: boolean;

  procedure GetWord(const _text: string; _pw: PWord);
  begin
    l2 := l1.FindNode(_text);
    s := l2.FirstChild.NodeValue;
    _pw^ := StrToInt('$' + s);
  end;

  procedure GetByte(const _text: string; _pb: PByte);
  begin
    l2 := l1.FindNode(_text);
    s := l2.FirstChild.NodeValue;
    _pb^ := StrToInt('$' + s);
  end;

  procedure LoadSIOchannel(const _text: string; _chan: TSIOchannel);
  var r: integer;
  begin
    l1 := doc.DocumentElement.FindNode('sio' + _text);
    for r := 0 to 2 do
      GetByte('read' + IntToStr(r), @_chan.RegRead[r]);
    for r := 0 to 7 do
      GetByte('write' + IntToStr(r), @_chan.RegWrite[r]);
  end;

begin
  // Make sure we stop the processor first
  if _proc.ProcessorState = psRunning then
    begin
      _proc.ExecuteStop;
      while not _proc.Idle do
        Sleep(5);
    end;
  _proc.Init;
  try
    ReadXMLfile(doc, _filename);
    // Read the environment
    l1 := doc.DocumentElement.FindNode('environment');
    l2 := l1.FindNode('was_running');
    if (l2 <> nil) and (l2.FirstChild.NodeValue <> '') then
      was_running := StrToBool(l2.FirstChild.NodeValue)
    else
      was_running := False;;
    {attachmentvar := l2.FirstChild.NodeValue}
    // Read the registers
    l1 := doc.DocumentElement.FindNode('registers');
    GetWord('reg__af',_proc.pregAF);
    GetWord('reg__bc',_proc.pregBC);
    GetWord('reg__de',_proc.pregDE);
    GetWord('reg__hl',_proc.pregHL);
    GetWord('reg_xaf',_proc.pregAF_);
    GetWord('reg_xbc',_proc.pregBC_);
    GetWord('reg_xde',_proc.pregDE_);
    GetWord('reg_xhl',_proc.pregHL_);
    GetWord('reg__ir',_proc.pregIR);
    GetWord('reg__ix',_proc.pregIX);
    GetWord('reg__iy',_proc.pregIY);
    GetWord('reg__sp',_proc.pregSP);
    GetWord('reg__pc',_proc.pregPC);
    GetByte('int_enabled',_proc.pregIntE);
    GetByte('int_mode',_proc.pregIntM);
    // Read the memory
    l1 := doc.DocumentElement.FindNode('memory');
    i := 0;
    p := @_proc.RAM[0];
    while i < 65536 do
      begin
        l2 := l1.FindNode(Format('memory_%4.4X',[i]));
        s := l2.FirstChild.NodeValue;
        for j := 0 to BYTES_PER_LINE-1 do
          begin
            p^ := StrToInt('$' + Copy(s,1+j*2,2));
            Inc(p);
            Inc(i);
          end;
      end;
    // Load the SIO sections
    LoadSIOchannel('a',_proc.SIO.ChannelA);
    LoadSIOchannel('b',_proc.SIO.ChannelB);
    // Read the terminal
    l1 := doc.DocumentElement.FindNode('terminal');
    GetByte('cursor_col',@b); _terminal.CursorCol := b;
    GetByte('cursor_row',@b); _terminal.CursorRow := b;
    p := @_terminal.Screen[0];
    for i := 0 to 24 do
      begin
        l2 := l1.FindNode(Format('terminal_%2.2X',[i]));
        s := l2.FirstChild.NodeValue;
        for j := 0 to 79 do
          begin
            p^ := StrToInt('$' + Copy(s,1+j*2,2));
            Inc(p);
          end;
      end;
  finally
    FreeAndNil(doc);
  end;
  if was_running then
    _proc.ExecuteRun;
  Result := was_running;
end;

procedure SaveVM(const _filename: string; _proc: TProcessor; _terminal: TTerminal);
var doc: TXMLDocument;
    rootnode, l1, l2, l3: TDOMNode;
    i,j: integer;
    s: string;
    was_running: boolean;

  procedure PumpL3(const _text, _value: string);
  begin
    l2 := doc.CreateElement(_text);
    l3 := doc.CreateTextNode(_value);
    l2.AppendChild(l3);
    l1.AppendChild(l2);
  end;

  procedure PumpL3hex(const _text: string; _word: Word; _digits: integer = 4);
  begin
    PumpL3(_text,IntToHex(_word,_digits));
  end;

  procedure SaveSIOchannel(const _id: string; _chan: TSIOchannel);
  var r: integer;
  begin
    l1 := doc.CreateElement('sio' + _id);
    for r := 0 to 2 do
      PumpL3hex('read' + IntToStr(r), _chan.RegRead[r], 2);
    for r := 0 to 7 do
      PumpL3hex('write' + IntToStr(r), _chan.RegWrite[r], 2);
    rootnode.AppendChild(l1);
  end;

begin
  was_running := (_proc.ProcessorState = psRunning);
  if was_running then
    begin
      _proc.ExecuteStop;
      while not _proc.Idle do
        Sleep(5);
    end;
  doc := TXMLDocument.Create;
  try
    rootnode := doc.CreateElement('vm');
    doc.AppendChild(rootnode);
    rootnode := doc.DocumentElement;
    // Create the environment section
    l1 := doc.CreateElement('environment');
    l2 := doc.CreateElement('was_running');
    l3 := doc.CreateTextNode(BoolToStr(was_running));
    l2.AppendChild(l3);
    l1.AppendChild(l2);
    rootnode.AppendChild(l1);
    // Create the register section
    l1 := doc.CreateElement('registers');
    PumpL3hex('reg__af',_proc.RegisterSet.registers[regAF]);
    PumpL3hex('reg__bc',_proc.RegisterSet.registers[regBC]);
    PumpL3hex('reg__de',_proc.RegisterSet.registers[regDE]);
    PumpL3hex('reg__hl',_proc.RegisterSet.registers[regHL]);
    PumpL3hex('reg_xaf',_proc.RegisterSet.registers[regAF_]);
    PumpL3hex('reg_xbc',_proc.RegisterSet.registers[regBC_]);
    PumpL3hex('reg_xde',_proc.RegisterSet.registers[regDE_]);
    PumpL3hex('reg_xhl',_proc.RegisterSet.registers[regHL_]);
    PumpL3hex('reg__ir',_proc.RegisterSet.registers[regIR]);
    PumpL3hex('reg__ix',_proc.RegisterSet.registers[regIX]);
    PumpL3hex('reg__iy',_proc.RegisterSet.registers[regIY]);
    PumpL3hex('reg__sp',_proc.RegisterSet.registers[regSP]);
    PumpL3hex('reg__pc',_proc.RegisterSet.registers[regPC]);
    PumpL3hex('int_enabled',_proc.RegisterSet.int_enabled,2);
    PumpL3hex('int_mode',_proc.RegisterSet.int_mode,2);
    rootnode.AppendChild(l1);
    // Create the memory section
    l1 := doc.CreateElement('memory');
    i := 0;
    while (i < 65536) do
      begin
        l2 := doc.CreateElement(Format('memory_%4.4X',[i]));
        s := '';
        for j := 0 to BYTES_PER_LINE-1 do
          begin
            s := s + IntToHex(_proc.RAM[i]);
            Inc(i);
          end;
        l3 := doc.CreateTextNode(s);
        l2.AppendChild(l3);
        l1.AppendChild(l2);
      end;
    rootnode.AppendChild(l1);
    // Save the SIO sections
    SaveSIOchannel('a',_proc.SIO.ChannelA);
    SaveSIOchannel('b',_proc.SIO.ChannelB);
    // Create the terminal section
    l1 := doc.CreateElement('terminal');
    PumpL3hex('cursor_col',_terminal.CursorCol);
    PumpL3hex('cursor_row',_terminal.CursorRow);
    for i := 0 to 24 do
      begin
        l2 := doc.CreateElement(Format('terminal_%2.2X',[i]));
        s := '';
        for j := 0 to 79 do
          begin
            s := s + IntToHex(_terminal.Screen[j + i*80]);
          end;
        l3 := doc.CreateTextNode(s);
        l2.AppendChild(l3);
        l1.AppendChild(l2);
      end;
    rootnode.AppendChild(l1);
    // Finally save the file
    writeXMLFile(doc,_filename);
  finally
    FreeAndNil(doc);
  end;
  if was_running then
    _proc.ExecuteRun;
end;

end.

