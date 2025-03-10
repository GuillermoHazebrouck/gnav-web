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
with Traffic;
with Utility;
use  Utility;
with Utility.Maps;

--//////////////////////////////////////////////////////////////////////////////
-- This special program is able to cut ESRI or binary terrain files in smaller
-- regions.
--//////////////////////////////////////////////////////////////////////////////
procedure Crunch is
begin

   Ada.Text_IO.Put_Line ("---------------------------------");
   Ada.Text_IO.Put_Line ("-- G-NAV PROJECT DATA COMPILER --");
   Ada.Text_IO.Put_Line ("-- VERSION 1D                  --");
   Ada.Text_IO.Put_Line ("---------------------------------");

   if Ada.Command_Line.Argument_Count = 0 then

      Ada.Text_IO.Put_Line ("Please specify a dataset to process:");
      Ada.Text_IO.Put_Line ("TERRAIN");
      Ada.Text_IO.Put_Line ("REFERENCES");
      Ada.Text_IO.Put_Line ("LAYERS");
      Ada.Text_IO.Put_Line ("AIRCRAFT");
      Ada.Text_IO.Put_Line ("TRAFFIC");

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

               Ada.Text_IO.Put_Line ("processing map topography");

               Terrain.Compile_Data;

            elsif Key = "REFERENCES" then

               Ada.Text_IO.Put_Line ("processing map references");

               References.Compile_Data;

               return;

            elsif Key = "LAYERS" then

               Ada.Text_IO.Put_Line ("processing map layers");

               Layers.Load_Shape_Files;

               Layers.Compile_Data;

            elsif Key = "AIRSPACE" then

               Ada.Text_IO.Put_Line ("processing airspaces");

               Layers.Airspaces.Parse_Open_Air_File;

               Layers.Airspaces.Compile_Data;

            elsif Key = "AIRCRAFT" then

               Ada.Text_IO.Put_Line ("processing aircraft data");

               Aircraft.Load_Aircraft_Data;

               Aircraft.Compile_Data;

            end if;

         end;

      end;

   end loop;

end Crunch;
--------------------------------------------------------------------------------
