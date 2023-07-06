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

unit uxml;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead;


function  GetXmlBoolean(_parent: TDOMnode; const _label: string; _default: boolean = False): boolean;
function  GetXmlByte(_parent: TDOMnode; const _label: string; _default: Byte = 0): Byte;
procedure GetXmlByteP(_parent: TDOMnode; const _label: string; _pb: PByte; const _default: Byte = 0);
function  GetXmlString(_parent: TDOMnode; const _label: string; const _default: string = ''): string;
function  GetXmlWord(_parent: TDOMnode; const _label: string; _default: Word = 0): Word;
procedure GetXmlWordP(_parent: TDOMnode; const _label: string; _pw: PWord; _default: Word = 0);
procedure PutXmlByte(_parent: TDOMnode; const _label: string; _b: Byte);
procedure PutXmlString(_parent: TDOMnode; const _label: string; const _s: string);
procedure PutXmlWord(_parent: TDOMnode; const _label: string; _w: Word);


implementation

function GetXmlBoolean(_parent: TDOMnode; const _label: string; _default: boolean = False): boolean;
begin
  try
    Result := StrToBool(GetXmlString(_parent,_label,BoolToStr(_default)));
  except
    Result := _default;
  end;
end;

function GetXmlByte(_parent: TDOMnode; const _label: string; _default: Byte = 0): Byte;
begin
  try
    Result := StrToInt('$' + GetXmlString(_parent,_label,IntToHex(_default,2)));
  except
    Result := _default;
  end;
end;

procedure GetXmlByteP(_parent: TDOMnode; const _label: string; _pb: PByte; const _default: Byte = 0);
var b: byte;
begin
  b := GetXmlByte(_parent,_label,_default);
  _pb^ := b;
end;

function GetXmlString(_parent: TDOMnode; const _label: string; const _default: string = ''): string;
var node: TDOMnode;
begin
  node := _parent.FindNode(_label{%H-});
  if (node = nil) then
    Result := _default
  else
    Result := node{%H-}.FirstChild.NodeValue;
end;

function GetXmlWord(_parent: TDOMnode; const _label: string; _default: Word = 0): Word;
begin
  try
    Result := StrToInt('$' + GetXmlString(_parent,_label,IntToHex(_default,4)));
  except
    Result := _default;
  end;
end;

procedure GetXmlWordP(_parent: TDOMnode; const _label: string; _pw: PWord; _default: Word = 0);
var w: Word;
begin
  w := GetXmlWord(_parent,_label,_default);
  _pw^ := w;
end;

procedure PutXmlByte(_parent: TDOMnode; const _label: string; _b: Byte);
var node_title: TDOMnode;
    node_text:  TDOMnode;
begin
  node_title := _parent.OwnerDocument.CreateElement(_label);
  node_text  := _parent.OwnerDocument.CreateTextNode(IntToHex(_b,2));
  node_title.AppendChild(node_text);
  _parent.AppendChild(node_title);
end;

procedure PutXmlString(_parent: TDOMnode; const _label: string; const _s: string);
var node_title: TDOMnode;
    node_text:  TDOMnode;
begin
  node_title := _parent.OwnerDocument.CreateElement(_label);
  node_text  := _parent.OwnerDocument.CreateTextNode(_s);
  node_title.AppendChild(node_text);
  _parent.AppendChild(node_title);
end;

procedure PutXmlWord(_parent: TDOMnode; const _label: string; _w: Word);
var node_title: TDOMnode;
    node_text:  TDOMnode;
begin
  node_title := _parent.OwnerDocument.CreateElement(_label);
  node_text  := _parent.OwnerDocument.CreateTextNode(IntToHex(_w,4));
  node_title.AppendChild(node_text);
  _parent.AppendChild(node_title);
end;

end.

