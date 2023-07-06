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

unit ucflash;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCFmode = (cmNone,cmInRead,cmInWrite);

  TCompactFlashInterface = class(TObject)
    private
      FSectors:  byte;
      FLBA0:     byte;
      FLBA1:     byte;
      FLBA2:     byte;
      FLBA3:     byte;

      FBuf:      array of byte;
      FBufSize:  integer;
      FCmdMode:  byte;
      FPtrR:     word;
      FPtrW:     word;
      FFilename: string;
      FPortBase: byte;
      FMode:     TCFmode;
      FStream:   TFileStream;
      procedure SetFilename(const _filename: string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure AttachImage(const _filename: string);
      procedure CloseImage;
      procedure CreateImage(const _filename: string; _cfsize: integer);
      function  GetReadByte: byte;
      function  GetStatus: byte;
      function  PortRead(_port: byte): byte;
      procedure PortWrite(_port, _data: byte);
      procedure PutWriteByte(_b: byte);
      procedure ReadSectorsWithRetry;
      procedure WriteSectorsWithRetry;
      property Filename: string read FFilename write SetFilename;
      property PortBase: byte read FPortBase write FPortBase;
      property Stream: TFileStream read FStream;
  end;

implementation

const
  SECTOR_SIZE = 512; // The standard IDE sector size

  PORT_0_DATA = 0;
  PORT_1_ERROR_FEATURE = 1;
  PORT_2_SECTORS = 2;
  PORT_3_LBA0 = 3;
  PORT_4_LBA1 = 4;
  PORT_5_LBA2 = 5;
  PORT_6_LBA3 = 6;
  PORT_7_STATUS_CMD = 7;

  IDE_CMD_RECAL    = $10;
  IDE_CMD_READ     = $20;
  IDE_CMD_WRITE    = $30;
  IDE_CMD_INIT     = $91;
  IDE_CMD_ID       = $EC;
  IDE_CMD_SPINDOWN = $E0;
  IDE_CMD_SPINUP   = $E1;

  IDE_STATUS_BUSY       = $80;
  IDE_STATUS_DRIVEREADY = $40;
  IDE_STATUS_DATAREQ    = $08;
  IDE_STATUS_ERROR      = $01;

  CMD_20_READ_SECTORS_WR  = $20;
  CMD_30_WRITE_SECTORS_WR = $30;
  CMD_EF_SET_FEATURES     = $EF;


constructor TCompactFlashInterface.Create;
begin
  inherited Create;
  FPortBase := $00;
  FMode := cmNone;
  FPtrR := 0;
  FPtrW := 0;
end;

destructor TCompactFlashInterface.Destroy;
begin
  CloseImage;
  inherited Destroy;
end;

procedure TCompactFlashInterface.AttachImage(const _filename: string);
begin
  CloseImage;
  if _filename <> '' then
    FStream := TFileStream.Create(_filename,fmOpenReadWrite);
end;

procedure TCompactFlashInterface.CloseImage;
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
end;

procedure TCompactFlashInterface.CreateImage(const _filename: string; _cfsize: integer);
const BUFSIZE = 32768;
var strm:      TFileStream;
    buf:       array[0..BUFSIZE-1] of byte;
    blksize:   integer;
    i:         integer;
begin
  strm := TFileStream.Create(_filename,fmCreate);
  try
    for i := 0 to BUFSIZE-1 do
      buf[i] := 0;
    while _cfsize > 0 do
      begin
        blksize := _cfsize;
        if blksize > BUFSIZE then
          blksize := BUFSIZE;
        strm.Write(buf[0],blksize);
        _cfsize := _cfsize - blksize;
      end;
  finally
    FreeAndNil(strm);
  end;
end;

function TCompactFlashInterface.GetReadByte: byte;
begin
  Result := 0;
  if FPtrR < FBufSize then
    begin
      Result := FBuf[FPtrR];
      Inc(FPtrR);
    end
  else
    FMode := cmNone;
end;

function TCompactFlashInterface.GetStatus: byte;
begin
  Result := IDE_STATUS_DRIVEREADY;
  if FMode in [cmInRead,cmInWrite] then
    Result := Result or IDE_STATUS_DATAREQ;
end;

function TCompactFlashInterface.PortRead(_port: byte): byte;
begin // Ports have already been changed to the range $00-$07
  Result := 0;
  case _port of
    PORT_0_DATA:
      Result := GetReadByte;
    PORT_7_STATUS_CMD:
      Result := GetStatus;
    otherwise
      raise Exception.Create(Format('Attempt to read from CF port %d',[_port]));
  end;
end;

procedure TCompactFlashInterface.PortWrite(_port, _data: byte);
begin // Ports have already been changed to the range $00-$07
  case _port of
    PORT_0_DATA:    PutWriteByte(_data);
    PORT_1_ERROR_FEATURE: ; // Write feature request, ignore
    PORT_2_SECTORS: FSectors := _data;
    PORT_3_LBA0:    FLBA0 := _data;
    PORT_4_LBA1:    FLBA1 := _data;
    PORT_5_LBA2:    FLBA2 := _data;
    PORT_6_LBA3:    FLBA3 := _data and $07;
    PORT_7_STATUS_CMD:
      case _data of
        CMD_20_READ_SECTORS_WR:  ReadSectorsWithRetry;
        CMD_30_WRITE_SECTORS_WR: WriteSectorsWithRetry;
        CMD_EF_SET_FEATURES: ;
        otherwise
          raise Exception.Create(Format('Attempt to write to CF port %d with data %2.2X',[_port,_data]));
      end;
    otherwise
      raise Exception.Create(Format('Attempt to write to CF port %d with data %2.2X',[_port,_data]));
  end;
end;

procedure TCompactFlashInterface.PutWriteByte(_b: byte);
var _posn: dword;
begin
  FBuf[FPtrW] := _b;
  Inc(FPtrW);
  if FPtrW >= FBufSize then
    begin
      _posn := FLBA0 or (FLBA1 shl 8) or (FLBA2 shl 16) or (FLBA3 shl 24);
      FStream.Position := _posn;
      FStream.Write(FBuf[0],FBufSize);
      FMode := cmNone;
    end;
end;

procedure TCompactFlashInterface.ReadSectorsWithRetry;
var _posn: dword;
begin
  // Calculate required size
  FBufSize := SECTOR_SIZE * FSectors;
  if Length(FBuf) <> FBufSize then
    SetLength(FBuf,FBufSize);
  _posn := FLBA0 or (FLBA1 shl 8) or (FLBA2 shl 16) or (FLBA3 shl 24);
  FStream.Position := _posn;
  FStream.Read(FBuf[0],FBufSize);
  FMode := cmInRead;
  FPtrR := 0;
end;

procedure TCompactFlashInterface.SetFilename(const _filename: string);
begin
  FFilename := _filename;
  AttachImage(FFilename);
end;

procedure TCompactFlashInterface.WriteSectorsWithRetry;
begin
  // Calculate required size
  FBufSize := SECTOR_SIZE * FSectors;
  if Length(FBuf) <> FBufSize then
    SetLength(FBuf,FBufSize);
  FMode := cmInWrite;
  FPtrW := 0;
end;

end.

