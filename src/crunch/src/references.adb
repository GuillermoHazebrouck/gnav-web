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
with Ada.Directories;
with Ada.Text_Io;
with Ada.Streams;
with Ada.Streams.Stream_IO;
-- Gnav
with Utility;
use  Utility;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body References is

   R : Natural := 0;

   --===========================================================================
   -- (See referencec file)
   --===========================================================================
   procedure Load_References is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Name  : String := "files/reference.dat";

      File_Id    : File_Type;

      Reader     : Utility.String_Buffer (200);

      Kind       : Reference_Types := Reference_City;

   begin

      Ada.Text_IO.Put_Line ("loading reference points");

      R          := 0;
      References := (others => No_Reference_Record);

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         while not End_Of_File (File_Id) and R < References'Last loop

            declare
               Line : String := Trim (Ada.Text_IO.Get_Line (File_Id));
            begin

               if Line'Length > 0 then

                  if Line (Line'First) = '#' then

                     if Line = "#AIRFIELDS" then

                        Kind := Reference_Airfield;

                     elsif Line = "#LANDMARKS" then

                        Kind := Reference_Landmark;

                     elsif Line = "#WATER" then

                        Kind := Reference_Water;

                     elsif Line = "#CITIES" then

                        Kind := Reference_City;

                     elsif Line = "#VILLAGES" then

                        Kind := Reference_Village;

                     elsif Line = "#WOODS" then

                        Kind := Reference_Woods;

                     elsif Line = "#TURBINES" then

                        Kind := Reference_Turbine;

                     elsif Line = "#LANDOUTS" then

                        Kind := Reference_Landouts;

                     end if;

                  else

                     Reader.Load (Line);

                     R := R + 1;

                     References (R).Kind := Kind;

                     Override (References (R).Id,   Trim (Reader.Read_Next ('@')));

                     Override (References (R).Name, Trim (Reader.Read_Next ('@')));

                     References (R).Position := Utility.Maps.Value (Trim (Reader.Read_Next ('@')));

                     References (R).Is_Loaded := True;

                     -- Ada.Text_IO.Put_Line (References (R).Id & "=>" & Utility.Maps.Image (References (R).Position));

                  end if;

               end if;

            end;

         end loop;

         Close (File_Id);

      end if;

      if R > 0 then

         Ada.Text_IO.Put_Line ("size  :" & Natural'Image (R));

      else

         Ada.Text_IO.Put_Line ("no reference locations loaded");

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("while reading reference file");

         Close (File_Id);

   end Load_References;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Saves the chart in binary format
   --===========================================================================
   procedure Save_Binary is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;
      Stream  : Stream_Access;

   begin

      if R > 0 then

         Ada.Text_IO.Put_Line ("usage :" & Natural'Image (R * 100 / References'Length) & "%");

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "files/reference.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Natural'Write (Stream, R); -- 4 bytes

         for Reference of References loop

            if Reference.Is_Loaded then

               Character'Write      (Stream, Reference_Kind_Key (Reference.Kind)); -- 1 byte
               Reference_Id'Write   (Stream, Reference.Id);                        -- 4 bytes
               Reference_Name'Write (Stream, Reference.Name);                      --14
               Long_Float'Write     (Stream, Reference.Position.Lat);              -- 8
               Long_Float'Write     (Stream, Reference.Position.Lon);              -- 8

            end if;

         end loop;

         Close (File_Id);

      end if;

   end Save_Binary;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See referencec file)
   --===========================================================================
   procedure Compile_Data is
   begin

      Load_References;

      Save_Binary;

   end Compile_Data;
   -----------------------------------------------------------------------------

end References;
--------------------------------------------------------------------------------
