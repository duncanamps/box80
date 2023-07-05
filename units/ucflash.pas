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
  TCompactFlashInterface = class(TObject)
    public
      procedure CreateAndAttach(_cfsize: integer; _force: boolean = False);
  end;

implementation

procedure TCompactFlashInterface.CreateAndAttach(_cfsize: integer; _force: boolean = False);
begin
end;

end.

