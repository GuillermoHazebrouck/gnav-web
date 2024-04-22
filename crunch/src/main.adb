--//////////////////////////////////////////////////////////////////////////////
-- G-NAV PROJECT
-- Written by Guillermo HAZEBROUCK - gahazebrouck@gmail.com
--\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- This file is part of "G-NAV".
--
-- G-NAV is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- G-NAV is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with G-NAV.  If not, see <https://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

-- Depencencies
--//////////////////////////////////////////////////////////////////////////////
-- Standard
with Ada.Command_Line;
with Ada.Text_IO;
-- Gnav
with Terrain;
with References;
with Layers;
with Layers.Airspaces;
with Aircraft;
with Utility;
use  Utility;

--//////////////////////////////////////////////////////////////////////////////
-- This special program is able to cut ESRI or binary terrain files in smaller
-- regions.
--//////////////////////////////////////////////////////////////////////////////
procedure Main is
begin

   if Ada.Command_Line.Argument_Count = 0 then

      Ada.Text_IO.Put_Line ("Please specify a dataset to process:");
      Ada.Text_IO.Put_Line ("TERRAIN");
      Ada.Text_IO.Put_Line ("REFERENCES");
      Ada.Text_IO.Put_Line ("LAYERS");
      Ada.Text_IO.Put_Line ("AIRCRAFT");

      return;

   end if;

   for I in 1..Ada.Command_Line.Argument_Count loop

      declare
         Argument : String_Buffer (100);
      begin

         Argument.Load (Ada.Command_Line.Argument (I));

         declare
            Key : String := Argument.Read_Next ('=');
            Val : String := Argument.Read_Next ('=');
         begin

            if    Key = "TERRAIN" then

               Ada.Text_IO.Put_Line ("compiling map topography...");

               Terrain.Compile_Data;

            elsif Key = "REFERENCES" then

               Ada.Text_IO.Put_Line ("compiling map references...");

               References.Compile_Data;

            elsif Key = "LAYERS" then

               Ada.Text_IO.Put_Line ("compiling map layers...");

               Layers.Load_Shape_Files;

               Layers.Compile_Data;

               Layers.Airspaces.Generate_Airspaces;

               Layers.Airspaces.Compile_Data;

            elsif Key = "AIRCRAFT" then

               Ada.Text_IO.Put_Line ("compiling aircraft data...");

               Aircraft.Load_Aircraft_Data;

               Aircraft.Compile_Data;

            else

               Ada.Text_IO.Put_Line ("invalid instruction " & Key);

            end if;

         end;

      end;

   end loop;


end Main;
