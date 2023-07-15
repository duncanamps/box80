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

unit ucircular;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  CB_CMD_RESET    = $00;
  CB_CMD_READ     = $01;
  CB_CMD_WRITE    = $02;
  CB_CMD_CONTAINS = $03;
  CB_CMD_CAPACITY = $04;


type
  TCircularBuffer = class(TObject)
    private
      FBuf:      array of byte;
      FContains: integer;
      FCS:       TRTLCriticalSection;
      FSize:     integer;
      FPtrIn:    integer;
      FPtrOut:   integer;
      procedure Bump(var _ptr: integer);
      function  GetRemaining: integer;
      function  GetContains: integer;
      function  GetPercentUsed: double;
      procedure Reset;
    public
      constructor Create(_maxsize: integer);
      destructor Destroy; override;
      function DoCmd(_cmd: byte; var _payload: integer): boolean;
      property Remaining:   integer read GetRemaining;
      property Contains:    integer read GetContains;
      property PercentUsed: double  read GetPercentUsed;
  end;


implementation

constructor TCircularBuffer.Create(_maxsize: integer);
begin
  inherited Create;
  FSize := _maxsize;
  SetLength(FBuf,_maxsize);
  Reset;
  InitCriticalSection(FCS);
end;

destructor TCircularBuffer.Destroy;
begin
  DoneCriticalSection(FCS);
  inherited Destroy;
end;

procedure TCircularBuffer.Bump(var _ptr: integer);
begin
  _ptr := (_ptr + 1) mod FSize;
end;

function TCircularBuffer.DoCmd(_cmd: byte; var _payload: integer): boolean;
var i: integer;
begin
  EnterCriticalSection(FCS);
  try
    Result := True; // Assume all good for now
    case _cmd of
      CB_CMD_READ:
        if FContains = 0 then
          Result := False  // Buffer empty
        else
          begin
            _payload := FBuf[FPtrOut];
            Bump(FPtrOut);
            Dec(FContains);
          end;
      CB_CMD_WRITE:
        if FContains = FSize then
          Result := False // Buffer full
        else
          begin
            FBuf[FPtrIn] := Byte(_payload);
            Bump(FPtrIn);
            Inc(FContains);
          end;
      CB_CMD_CONTAINS:
        begin
          i := FContains;
          _payload := i;
        end;
      CB_CMD_CAPACITY:
        begin
          i := (FSize-FContains);
          _payload := i;
        end;
      CB_CMD_RESET:
        Reset;
      otherwise
        raise Exception.Create(Format('Circular buffer unknown command $2.2X',[_cmd]));
    end;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

function TCircularBuffer.GetRemaining: integer;
var b: integer;
begin
  DoCmd(CB_CMD_CAPACITY,b);
  Result := b;
end;

function TCircularBuffer.GetContains: integer;
var b: integer;
begin
  DoCmd(CB_CMD_CONTAINS,b);
  Result := b;
end;

function TCircularBuffer.GetPercentUsed: double;
var b: integer;
begin
  DoCmd(CB_CMD_CONTAINS,b);
  Result := b * 100.00 / FSize;
end;

procedure TCircularBuffer.Reset;
begin
  FContains := 0;
  FPtrIn := 0;
  FPtrOut := 0;
end;

end.

