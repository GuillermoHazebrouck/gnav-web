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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
-- Gnav
with Math.Vector2;
use  Math.Vector2;
with Utility;
use  Utility;
with Utility.Maps;
use  Utility.Maps;
with Utility.Streams;
use  Utility.Streams;



--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Layers is


   --===========================================================================
   -- Reads and loads the content of the shape file
   --===========================================================================
   procedure Load_Geometry (Layer        : Layer_Access;
                            Source       : not null access Stream_Buffer_Type;
                            Parts_Count  : Integer;
                            Points_Count : Integer) is

      type Position_Record_Array is array (1..Points_Count) of Position_Record;

      Parts  : array (1..Parts_Count) of Integer;

      Points : Position_Record_Array;

      First  : Positive := Points'First;

      Last   : Positive := First;

      --========================================================================
      procedure Load_Buffer (Sub_Set : Position_Record_Array) is

         use Math.Vector2;

         Part   : Part_Access := null;

         Point  : Vector2_Access;

      begin

         --Ada.Text_IO.Put_Line ("adding part >" & Natural'Image (Sub_Set'Length) & " nodes");

         Layer.Parts.Add_Item (Part);

         if Part = null then

            return;

         end if;

         for K in Sub_Set'Range loop

            Part.Points.Add_Item (Point);

            Point.Set (Sub_Set (K).Lon,
                       Sub_Set (K).Lat);

         end loop;

      end Load_Buffer;
      --------------------------------------------------------------------------

   begin

      for I in 1..Parts_Count loop

         Integer'Read (Source, Parts (I));

         -- Utility.Log.Put_Message ("part at" & Integer'Image (Parts (I)));

      end loop;

      for J in 1..Points_Count loop

         Long_Float'Read (Source, Points (J).Lon);

         Long_Float'Read (Source, Points (J).Lat);

      end loop;

      -- Utility.Log.Put_Message ("loading part");

      -- Utility.Log.Put_Message (Image (Points (Points'First)));

      -- Utility.Log.Put_Message (Image (Points (Points'Last)));

      for I in 1..Parts_Count loop

         First := Parts (I) + 1;

         if I = Parts_Count then

            Last  := Points'Last;

         else

            Last  := Parts (I + 1);

         end if;

         Load_Buffer (Points (First..Last));

      end loop;

   exception

      when E : others =>

         Ada.Text_IO.Put_Line ("error while reading shape part");

   end Load_Geometry;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Reads and loads the content of the shape file
   --===========================================================================
   procedure Process_Shape_File (File : Ada.Directories.Directory_Entry_Type) is

      use Ada.Directories;
      use Ada.Streams.Stream_IO;

      Layer   : Layer_Access := null;

      Path    : String := Full_Name (File);

      Name    : String := Ada.Directories.Base_Name (Path);

      Buffer  : Stream_Element_Array (1..Stream_Element_Offset (Size (File)));

      Stream  : aliased Stream_Buffer_Type;

      Source  : not null access Stream_Buffer_Type := Stream'Access;

      File_Id : File_Type;

      Last    : Stream_Element_Offset;

      Ident   : Integer;

      Version : Integer;

      Shape   : Integer := 0;

      BL, UR  : Position_Record;

      Number  : Integer;

      Length  : Integer;

      Parts   : Integer;

      Points  : Integer;

   begin

      Ada.Text_IO.Put_Line ("reading shape file " & Path);

      Layers.Add_Item (Layer);

      if Layer = null then

         return;

      end if;

      if    Contains (Name, "river") then
         Layer.Kind := Layer_River;
      elsif Contains (Name, "border") then
         Layer.Kind := Layer_Border;
      elsif Contains (Name, "rail") then
         Layer.Kind := Layer_Rail;
      elsif Contains (Name, "road") then
         Layer.Kind := Layer_Road;
      elsif Contains (Name, "lake") then
         Layer.Kind := Layer_Lake;
      else
         Layer.Kind := Layer_Unknown;
      end if;

      Override (Layer.Name, Name);

      Open (File_Id, In_File, Path);

      Read (File_Id, Buffer, Last);

      Stream.Buffer := Buffer'Unrestricted_Access;

      Stream.Cursor := Buffer'First;

      Stream.Endianness_Type := Big_Endian;

      Integer'Read (Source, Ident);

      if Ident /= 9994 then

         Ada.Text_IO.Put_Line ("shape file code not found" & Stream_Element_Offset'Image (Last));

         goto Close_And_Leave;

      end if;

      Stream.Endianness_Type := Little_Endian;

      Stream.Move_Cursor (29);

      Integer'Read (Source, Version);

      if Version /= 1000 then

         Ada.Text_IO.Put_Line ("shape file version not supported");

         goto Close_And_Leave;

      end if;

      Integer'Read (Source, Shape);

      --Ada.Text_IO.Put_Line ("shape type:" & Integer'Image (Shape));

      if Shape = 3 then

         Layer.Form := Form_Polyline;

      elsif Shape = 5 then

         Layer.Form := Form_Polygon;

      else

         Ada.Text_IO.Put_Line ("shaper type not supported");

         goto Close_And_Leave;

      end if;

      Long_Float'Read (Source, BL.Lon);

      Long_Float'Read (Source, BL.Lat);

      Long_Float'Read (Source, UR.Lon);

      Long_Float'Read (Source, UR.Lat);

      --Ada.Text_IO.Put_Line ("Bottom-Left:" & Image (BL));
      --Ada.Text_IO.Put_Line ("Upper-Right:" & Image (UR));

      Stream.Move_Cursor (101);

      while not Stream.End_Off_Buffer loop

         -- Record header
         ------------------------------------

         Stream.Endianness_Type := Big_Endian;

         Integer'Read (Source, Number);

         Integer'Read (Source, Length);

         -- Record content
         ------------------------------------

         Stream.Endianness_Type := Little_Endian;

         Integer'Read (Source, Shape);

         Long_Float'Read (Source, BL.Lon);

         Long_Float'Read (Source, BL.Lat);

         Long_Float'Read (Source, UR.Lon);

         Long_Float'Read (Source, UR.Lat);

         Integer'Read (Source, Parts);

         Integer'Read (Source, Points);

         -- Parts and points
         ------------------------------------

         Load_Geometry (Layer, Source, Parts, Points);

      end loop;

      Ada.Text_IO.Put_Line ("loaded " & Natural'Image (Layer.Parts.Get_Count) & " parts");

      <<Close_And_Leave>>

      Close (File_Id);

   exception
      when E : others =>

         Ada.Text_IO.Put_Line ("error while reading shape file '" & Name & "'");

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Process_Shape_File;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Shape_Files is

      use Ada.Directories;

   begin

      Layers.Clear;

      Search (Directory => "./",
              Pattern   => "*.shp",
              Process   => Process_Shape_File'Access);

   end Load_Shape_Files;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Scans all the data, finds the local bounds and returns the global bounds
   --===========================================================================
   procedure Search_Global_Bounds (South_West, North_East : out Position_Record) is

      use Math.Vector2;

      Layer : Layer_Access;
      Part  : Part_Access;
      Point : Vector2_Access;

      First_Global : Boolean := True;
      First_Local  : Boolean := True;

   begin

      South_West := (0.0, 0.0);
      North_East := (0.0, 0.0);

      Layer := Layers.Get_First_Item;

      while Layer /= null loop

         Part := Layer.Parts.Get_First_Item;

         while Part /= null loop

            First_Local := True;

            Point := Part.Points.Get_First_Item;

            while Point /= null loop

               if First_Local then

                  Part.South_West.Lon := Point.Get_X;
                  Part.South_West.Lat := Point.Get_Y;

                  Part.North_East := Part.South_West;

                  First_Local := False;

               else

                  if Point.Get_X < Part.South_West.Lon then
                     Part.South_West.Lon := Point.Get_X;
                  end if;

                  if Point.Get_Y < Part.South_West.Lat then
                     Part.South_West.Lat := Point.Get_Y;
                  end if;

                  if Point.Get_X > Part.North_East.Lon then
                     Part.North_East.Lon := Point.Get_X;
                  end if;

                  if Point.Get_Y > Part.North_East.Lat then
                     Part.North_East.Lat := Point.Get_Y;
                  end if;

               end if;

               Part.Points.Get_Next_Item (Point);

            end loop;

            if First_Global then

               South_West.Lon := Part.South_West.Lon;
               South_West.Lat := Part.South_West.Lat;

               North_East.Lon := Part.North_East.Lon;
               North_East.Lat := Part.North_East.Lat;

               First_Global := False;

            else

               if Part.South_West.Lon < South_West.Lon then
                  South_West.Lon := Part.South_West.Lon;
               end if;

               if Part.South_West.Lat < South_West.Lat then
                  South_West.Lat := Part.South_West.Lat;
               end if;

               if Part.North_East.Lon > North_East.Lon then
                  North_East.Lon := Part.North_East.Lon;
               end if;

               if Part.North_East.Lat > North_East.Lat then
                  North_East.Lat := Part.North_East.Lat;
               end if;

            end if;

            Layer.Parts.Get_Next_Item (Part);

         end loop;

         Layers.Get_Next_Item (Layer);

      end loop;

   end Search_Global_Bounds;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compile_Data is

      use Ada.Streams.Stream_IO;
      use Math.Vector2;

      File_Id    : File_Type;
      Stream     : Stream_Access;
      Layer      : Layer_Access;
      Part       : Part_Access;
      Point      : Vector2_Access;
      South_West,
      North_East : Position_Record;
      Origin     : Position_Record;
      L, P       : Natural := 0;

   begin

      if Layers.Get_Count > 0 then

         Search_Global_Bounds (South_West, North_East);

         Origin.Lat := 0.5 * (South_West.Lat + North_East.Lat);
         Origin.Lon := 0.5 * (South_West.Lon + North_East.Lon);

         Ada.Text_Io.Put_Line ("compiling layers");
         Ada.Text_Io.Put_Line ("origin: " & Maps.Image (Origin));

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "layers.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Long_Float'Write (Stream, Origin.Lat);    --  8 bytes
         Long_Float'Write (Stream, Origin.Lon);    --  8 bytes

         Natural'Write (Stream, Layers.Get_Count); --  4 bytes

         Layer := Layers.Get_First_Item;

         while Layer /= null loop

            L := L + 1;

            Ada.Text_Io.Put_Line (Trim (Layer.Name) & "->" & Natural'Image (Layer.Parts.Get_Count));

            Character'Write  (Stream, Layer_Keys (Layer.Kind)); -- 1 byte
            Natural'Write    (Stream, Layer.Parts.Get_Count);   -- 4 bytes

            Part := Layer.Parts.Get_First_Item;

            while Part /= null loop

               P := P + 1;

               Float'Write (Stream, Float (Part.South_West.Lat - Origin.Lat)); -- 4 bytes
               Float'Write (Stream, Float (Part.South_West.Lon - Origin.Lon)); -- 4 bytes
               Float'Write (Stream, Float (Part.North_East.Lat - Origin.Lat)); -- 4 bytes
               Float'Write (Stream, Float (Part.North_East.Lon - Origin.Lon)); -- 4 bytes

               Natural'Write (Stream, Part.Points.Get_Count);  -- 4 bytes

               Point := Part.Points.Get_First_Item;
               while Point /= null loop
                  Float'Write (Stream, Float (Point.Get_Y - Origin.Lat)); -- 4 bytes
                  Float'Write (Stream, Float (Point.Get_X - Origin.Lon)); -- 4 bytes
                  Part.Points.Get_Next_Item (Point);
               end loop;

               Layer.Parts.Get_Next_Item (Part);

            end loop;

            Layers.Get_Next_Item (Layer);

         end loop;

         Close (File_Id);

         Ada.Text_Io.Put_Line (Natural'Image (P) & " parts compiled");

      end if;

   end Compile_Data;
   -----------------------------------------------------------------------------

end Layers;
--------------------------------------------------------------------------------
