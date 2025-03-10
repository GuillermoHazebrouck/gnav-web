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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Numerics;
with Ada.Numerics.Short_Elementary_Functions;
with Ada.Numerics.Elementary_Functions;
-- Local
with Utility;
use  Utility;
with Utility.Maps;
use  Utility.Maps;


--//////////////////////////////////////////////////////////////////////////////
-- This special program is able to cut ESRI or binary terrain files in smaller
-- regions.
--//////////////////////////////////////////////////////////////////////////////
package body Terrain is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Short_Math renames Ada.Numerics.Short_Elementary_Functions;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Terrain_Range is Natural range 1 .. 20_000_000;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Up to 320MiB static buffer containing the terrain data grid
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Terrain : array (Terrain_Range) of Short_Float; -- := (others => 0.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Map grid data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   -- Lower left corner
   --------------------------------
   Reference : Position_Record := No_Position_Record;

   -- The middle point of the chart
   --------------------------------
   Middle : Position_Record := No_Position_Record;

   -- The size of a cell (degrees)
   --------------------------------
   Cell_Size : Float;

   -- No data value
   --------------------------------
   No_Data : Float := 0.0;

   -- Lower vertical limit (meters)
   --------------------------------
   Z_Min_Global : Float := 0.0;

   -- Upper vertical limit (meters)
   --------------------------------
   Z_Max_Global : Float := 0.0;

   -- Number of rows (latitude)
   --------------------------------
   M : Natural := 0;

   -- Number of columns (longitude)
   --------------------------------
   N : Natural := 0;

   -- The map has been loaded
   --------------------------------
   Loaded : Boolean := False;

   -- Forces a map reload
   --------------------------------
   Reload : Boolean;

   -- Forces a map reload
   --------------------------------
   Terrain_Too_Large_Message : String := "unable to load the given terrain file, the size is too large";




   --===========================================================================
   -- Clears the grid
   --===========================================================================
   procedure Clear_Grid is
   begin

      Ada.Text_Io.Put_Line ("clearing the terrain grid");

      N         := 0;

      M         := 0;

      Cell_Size := 0.0;

      Terrain   := (others => 0.0);

      Reload    := True;

   end Clear_Grid;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Bin_File_Name return String is
   begin

      return "terrain.bin";

   end Bin_File_Name;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Dat_File_Name return String is
   begin

      return "terrain.asc";

   end Dat_File_Name;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Opens an Esri ASCII grid file and loads the terrain data into the buffer
   --===========================================================================
   procedure Load_Esri_Grid_File is

      use Ada.Text_IO;
      use Utility;

      File_Id : File_Type;

      C, I, U : Natural := 0;

      Z : Float := 0.0;

      Reader : Utility.String_Buffer (2000000);

   begin

      Ada.Text_Io.Put_Line ("loading terrain grid");

      if Ada.Directories.Exists (Dat_File_Name) then

         Ada.Text_Io.Put_Line ("file exists");

         -- Reset
         -----------------------------------------------------------------------
         Loaded := False;

         Reference := No_Position_Record;
         N         := 0;
         M         := 0;
         Cell_Size := 0.0;
         No_Data   := 0.0;

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, Dat_File_Name);

         while not End_Of_File (File_Id) loop

            C := C + 1;

            Reader.Load (Ada.Text_IO.Get_Line (File_Id));

            -- This is the header
            --------------------------------------------------------------------
            if C <= 6 then

               declare
                  Key   : String := Get_Lower_Case (Reader.Read_Next);
                  Value : String := Reader.Read_Next (Multiple => True);
               begin

                  Ada.Text_Io.Put_Line ("line: " & Key & " / " & Value);

                  if Key = "ncols" then

                     N := Natural'Value (Value);

                     Ada.Text_Io.Put_Line ("N    :" & Natural'Image (N));

                  elsif Key = "nrows" then

                     M := Natural'Value (Value);

                     Ada.Text_Io.Put_Line ("M    :" & Natural'Image (M));

                     I := M - 1;

                  elsif Key = "xllcorner" then

                     Reference.Lon := Long_Float'Value (Value);

                  elsif Key = "yllcorner" then

                     Reference.Lat := Long_Float'Value (Value);

                  elsif Key = "cellsize" then

                     Cell_Size := Float'Value (Value);

                  elsif Key = "nodata_value" then

                     No_Data := Float'Value (Value);

                  end if;

               end;

               -- Check if the buffer is sufficiently large
               -----------------------------------------------------------------

               if N * M > Terrain'Length then

                  Ada.Text_Io.Put_Line (Terrain_Too_Large_Message);

                  Clear_Grid;

                  return;

               end if;

            -- This is the data
            --------------------------------------------------------------------
            else

               -- Read a full row
               --------------------------------------------------------------
               for J in 1.. N loop

                  loop
                     declare
                        Value : String := Reader.Read_Next (Multiple => True);
                     begin

                        if Value /= "" then
                           Z := Float'Value (Value);
                           exit;
                        end if;

                     end;
                  end loop;

                  if Z = No_Data then

                     Z := 0.0;

                     U := U + 1;

                  end if;

                  Z_Min_Global := Float'Min (Z, Z_Min_Global);

                  Z_Max_Global := Float'Max (Z, Z_Max_Global);

                  -- Load a vertex
                  --------------------------------------------------------------
                  Terrain (I * N + J) := Short_Float (Z);

               end loop;

               if I > 0 then

                  I := I - 1;

               else

                  exit;

               end if;

            end if;

         end loop;

         Ada.Text_IO.Close (File_Id);

         Ada.Text_Io.Put_Line ("no-data points:" & Natural'Image (U));

         Ada.Text_Io.Put_Line ("terrain data loaded");

         -- Normalize altitude
         ----------------------------------------------
         Ada.Text_Io.Put_Line ("normalizing altitude");

         Loaded := True;

         -- Report limits, size and unknown points
         ----------------------------------------------
         Middle.Lat := Reference.Lat + Long_Float (0.5 * Float (M) * Cell_Size);

         Middle.Lon := Reference.Lon + Long_Float (0.5 * Float (N) * Cell_Size);

      else

         Ada.Text_Io.Put_Line ("terrain data not found");

         Clear_Grid;

      end if;

   exception

      when E : others =>

         Ada.Text_Io.Put_Line ("failed to load the terrain: " & Ada.Exceptions.Exception_Message (E));

         Ada.Text_Io.Put_Line ("last line:" & Natural'Image (C));

         Loaded := False;

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Load_Esri_Grid_File;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Saves the chart in binary format
   --===========================================================================
   procedure Save_Binary is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;
      Stream  : Stream_Access;

   begin

      if N * M <= Terrain'Length then

         Ada.Text_Io.Put_Line ("saving terrain data in binary");

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => Bin_File_Name);

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Natural'Write (Stream, N);
         Natural'Write (Stream, M);

         Long_Float'Write (Stream, Reference.Lat);
         Long_Float'Write (Stream, Reference.Lon);
         Long_Float'Write (Stream, Middle.Lat);
         Long_Float'Write (Stream, Middle.Lon);
         Float'Write (Stream, Cell_Size);
         Float'Write (Stream, Z_Min_Global);
         Float'Write (Stream, Z_Max_Global);

         for I in 1 .. N * M loop

            Short_Float'Write (Stream, Terrain (I));

         end loop;

         Close (File_Id);

      end if;

   end Save_Binary;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads the chart from a binary file
   --===========================================================================
   procedure Load_Binary  is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;

      Stream  : Stream_Access;

   begin

      if Ada.Directories.Exists (Bin_File_Name) then

         Ada.Text_Io.Put_Line ("loading terrain data from binary");

         -- Start writing
         -----------------------------------------------------------------------

         Ada.Streams.Stream_IO.Open (File => File_Id,
                                     Mode => In_File,
                                     Name => Bin_File_Name);

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Natural'Read (Stream, N);
         Natural'Read (Stream, M);

         Ada.Text_Io.Put_Line ("N :" & Natural'Image (N));
         Ada.Text_Io.Put_Line ("M :" & Natural'Image (M));

         if N * M > Terrain'Length then

            Ada.Text_Io.Put_Line (Terrain_Too_Large_Message);

            Clear_Grid;

            return;

         end if;

         Long_Float'Read (Stream, Reference.Lat);
         Long_Float'Read (Stream, Reference.Lon);
         Long_Float'Read (Stream, Middle.Lat);
         Long_Float'Read (Stream, Middle.Lon);
         Float'Read (Stream, Cell_Size);
         Float'Read (Stream, Z_Min_Global);
         Float'Read (Stream, Z_Max_Global);

         for I in 1 .. N * M loop

            Short_Float'Read (Stream, Terrain (I));

         end loop;

         Close (File_Id);

         Loaded := True;

      else

         Clear_Grid;

      end if;

   exception
      when others =>

         if Is_Open (File_Id) then
            Close (File_Id);
            Ada.Text_Io.Put_Line ("error while loading binary");
         end if;

   end Load_Binary;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Lon_Size (Delta_Lon : Float; Latitude : Float) return Float is

      Equator_Length : constant Float := 111.3195;

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

   begin

      return Delta_Lon * Equator_Length * Cos (Latitude * 3.141592 / 180.0);

   end Lon_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Lat_Size (Delta_Lat : Float) return Float is

      Meridian_Length : constant Float := 110.5800;

   begin

      return Delta_Lat * Meridian_Length;

   end Lat_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Saves the chart in binary format
   --===========================================================================
   procedure Save_Binary_Parts is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;
      Stream  : Stream_Access;

      S  : constant Natural := 5; --> number of parts

      Rx : Natural := 2; --> resolution in longitude
      Ry : Natural := 1; --> resolution in latitude

      Nx : Natural;
      Ny : Natural;
      T  : Natural;
      D  : Natural;

      Cx : Float;
      Cy : Float;

      O, K, C, F, P : Natural := 0;

   begin

      -- Read command line arguments
      --------------------------------------------------------------------------
      for I in 1..Ada.Command_Line.Argument_Count loop

         declare
            Argument : String_Buffer (100);
         begin

            Argument.Load (Ada.Command_Line.Argument (I));

            declare
               Key : String := Argument.Read_Next ('=');
               Val : String := Argument.Read_Next ('=');
            begin

               if    Key = "RX" then

                  Rx := Natural'Value (Val);

               elsif Key = "RY" then

                  Ry := Natural'Value (Val);

               end if;

            end;

         end;

      end loop;
      --------------------------------------------------------------------------

      Nx := N / Rx;
      Ny := M / Ry;
      T  := Nx * Ny;
      D  := T / S + S;

      Cx := (Cell_Size * Float (N)) / Float (Nx);
      Cy := (Cell_Size * Float (M)) / Float (Ny);

      -- Note that by specification, the maximum number of nodes in the output
      -- is limited to 10 millon.
      --------------------------------------------------------------------------
      if Nx * Ny <= 10_000_000 then

         Ada.Text_Io.Put_Line ("writing header");

         -- Ada.Text_Io.Put_Line ("Short_Integer  :" & Integer'Image ((Short_Integer'Max_Size_In_Storage_Elements)));
         -- Ada.Text_Io.Put_Line ("Natural        :" & Integer'Image ((Natural'Max_Size_In_Storage_Elements)));
         -- Ada.Text_Io.Put_Line ("Float          :" & Integer'Image ((Float'Max_Size_In_Storage_Elements)));
         -- Ada.Text_Io.Put_Line ("Long_Float     :" & Integer'Image ((Long_Float'Max_Size_In_Storage_Elements)));

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "terrain_1.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         -- Write info header on first file
         -----------------------------------------------------------------------

         Natural'Write (Stream, Nx); --4
         Natural'Write (Stream, Ny); --8

         Long_Float'Write (Stream, Reference.Lat); --16
         Long_Float'Write (Stream, Reference.Lon); --24

         Float'Write (Stream, Cx); --28
         Float'Write (Stream, Cy); --32

         Float'Write (Stream, Z_Min_Global); --36
         Float'Write (Stream, Z_Max_Global); --40

         Ada.Text_Io.Put_Line ("Nx :" & Natural'Image (Nx));
         Ada.Text_Io.Put_Line ("Ny :" & Natural'Image (Ny));

         Ada.Text_Io.Put_Line ("Cx :" & Float'Image (Cx) & "deg ->" & Float'Image (Lon_Size (Cx, Float (Middle.Lat))) & "km");
         Ada.Text_Io.Put_Line ("Cy :" & Float'Image (Cy) & "deg ->" & Float'Image (Lat_Size (Cy))                     & "km");

         Ada.Text_Io.Put_Line ("Zl :" & Integer'Image (Integer (Z_Min_Global)) & "m");
         Ada.Text_Io.Put_Line ("Zu :" & Integer'Image (Integer (Z_Max_Global)) & "m");

         Ada.Text_Io.Put_Line ("buffer size  :" & Natural'Image (T));
         Ada.Text_Io.Put_Line ("buffer usage :" & Natural'Image (Natural (Float (T) / 10_000_000.0 * 100.0)) & "%");
         Ada.Text_Io.Put_Line ("memory size  :" & Natural'Image ((Short_Integer'Size * T) / 8_000_000) & "MB");

         Ada.Text_Io.Put_Line ("SW : " & Image (Reference));
         Ada.Text_Io.Put_Line ("NE : " & Image ((Lon => (Reference.Lon + Long_Float (Nx) * Long_Float (Cx)),
                                                 Lat => (Reference.Lat + Long_Float (Ny) * Long_Float (Cy)))));

         P := 1;
         C := 0;
         F := 0;

         Ada.Text_Io.Put_Line ("writing part 1");

         Natural'Write (Stream, 1);

         for I in 1 .. Ny loop

            K := (I - 1) * Ry * N;

            for J in 1 .. Nx loop

               O := K + (J - 1) * Rx + 1;

               F := F + 1;

               -- Change to next file when reaching the target size
               -----------------------------------------------------------------
               if C >= D then

                  P := P + 1;

                  if Is_Open (File_Id) then
                     Close (File_Id);
                  end if;

                  Ada.Text_Io.Put_Line ("writing part" & Natural'Image (P));

                  Create (File => File_Id,
                          Mode => Out_File,
                          Name => "terrain_" & Utility.Integer_Image (P) & ".bin");

                  Stream := Ada.Streams.Stream_IO.Stream (File_Id);

                  Natural'Write (Stream, F);

                  Ada.Text_Io.Put_Line ("F :" & Natural'Image (F));

                  C := 0;

               end if;

               Short_Integer'Write (Stream, Short_Integer (Terrain (O)));

               C := C + 1;

            end loop;

         end loop;

         Close (File_Id);

      else
         Ada.Text_Io.Put_Line ("ERROR: the output will be too large, consider reducing the resolution or the extension.");
         Ada.Text_Io.Put_Line ("size" & Natural'Image (Nx * Ny) & " is above 10 millon points.");

      end if;

   end Save_Binary_Parts;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compile_Data is

      use Utility;

   begin

      Ada.Text_Io.Put_Line ("reading terrain");

      if Ada.Directories.Exists (Bin_File_Name) then

         Load_Binary;

      elsif Ada.Directories.Exists (Dat_File_Name) then

         Load_Esri_Grid_File;

         Ada.Text_Io.Put_Line ("saving binary terrain format");

         Save_Binary;

      else

         Ada.Text_Io.Put_Line ("no terrain data available");

      end if;

      Ada.Text_Io.Put_Line ("terrain grid loaded");

      Ada.Text_Io.Put_Line ("Lat  :" & Long_Float'Image (Middle.Lat));
      Ada.Text_Io.Put_Line ("Lon  :" & Long_Float'Image (Middle.Lon));

      Ada.Text_Io.Put_Line ("Zmin :" & Float'Image (Z_Min_Global));
      Ada.Text_Io.Put_Line ("Zmax :" & Float'Image (Z_Max_Global));

      Ada.Text_Io.Put_Line ("Size :" & Natural'Image ((Short_Integer'Size * M * N) / 8_000_000) & "MB");

      Ada.Text_Io.Put_Line ("terrain buffer usage: " & Natural'Image ((N * M * 100) / Terrain'Length) & "%");

      Save_Binary_Parts;

   end Compile_Data;
   -----------------------------------------------------------------------------

end Terrain;
--------------------------------------------------------------------------------
