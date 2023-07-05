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
  Classes, SysUtils, uprocessor;

procedure LoadVM(const _filename: string; _proc: TProcessor);
procedure SaveVM(const _filename: string; _proc: TProcessor);

implementation

uses
  DOM, XMLWrite, XMLRead;

const BYTES_PER_LINE = 32;

procedure LoadVM(const _filename: string; _proc: TProcessor);
type PWord = ^word;
     PByte = ^byte;
var doc: TXMLDocument;
    rootnode, l1, l2, l3: TDOMNode;
    s: string;
    i,j: integer;
    p: PByte;

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

begin
  try
    ReadXMLfile(doc, _filename);
    // Read the environment
    l1 := doc.DocumentElement.FindNode('environment');
    l2 := l1.FindNode('attachment');
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
  finally
    FreeAndNil(doc);
  end;
end;

procedure SaveVM(const _filename: string; _proc: TProcessor);
var doc: TXMLDocument;
    rootnode, l1, l2, l3: TDOMNode;
    i,j: integer;
    s: string;

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

begin
  doc := TXMLDocument.Create;
  try
    rootnode := doc.CreateElement('vm');
    doc.AppendChild(rootnode);
    rootnode := doc.DocumentElement;
    // Create the environment section
    l1 := doc.CreateElement('environment');
    l2 := doc.CreateElement('attachment');
    l3 := doc.CreateTextNode('C:\blah blah');
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
    // Finally save the file
    writeXMLFile(doc,_filename);
  finally
    FreeAndNil(doc);
  end;
end;

end.

