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

unit uglobals;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  BYTES_PER_LINE = 32;
  CF_BLOCK_SIZE = 4096;
  CF_DEFAULT_SIZE_64  = (CF_BLOCK_SIZE * 2048 *  7 + CF_BLOCK_SIZE * 1280);
  CF_DEFAULT_SIZE_128 = (CF_BLOCK_SIZE * 2048 * 15 + CF_BLOCK_SIZE *  512);
  MAXIMUM_CPU_SPEED = 9999999999; // 10GHz
  MAXIMUM_MRU = 10;

implementation

end.

