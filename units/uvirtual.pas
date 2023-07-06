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
  DOM, XMLWrite, XMLRead, uxml;

function LoadVM(const _filename: string; _proc: TProcessor; _terminal: TTerminal): boolean;
var doc: TXMLDocument;
    l1: TDOMnode;
    was_running: boolean;
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
    was_running := GetXmlBoolean(l1,'was_running',False);
    // Read the processor
    _proc.ReadFromXml(doc);
    // Read the terminal
    _terminal.ReadFromXml(doc);
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
    was_running: boolean;
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
    l3 := doc.CreateTextNode(BoolToStr(was_running){%H-});
    l2.AppendChild(l3);
    l1.AppendChild(l2);
    rootnode.AppendChild(l1);
    // Create the register section
    _proc.WriteToXml(doc);
    // Create the terminal section
    _terminal.WriteToXml(doc);
    // Finally save the file
    writeXMLFile(doc,_filename);
  finally
    FreeAndNil(doc);
  end;
  if was_running then
    _proc.ExecuteRun;
end;

end.

