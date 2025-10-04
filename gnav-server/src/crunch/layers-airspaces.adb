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
-- Gnav
with Math.Tools;
with Math.Vector2;
with Math.Vector2_List;
with Utility;
use  Utility;
with Utility.Maps;
use  Utility.Maps;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Layers.Airspaces is

   --===========================================================================
   -- Specific function for airspace position data (Eg. 50:51:30N 003:11:10E)
   --===========================================================================
   function Sexagecimal_Value (Data : String) return Long_Float is

      Sign   : Long_Float := 1.0;
      Result : Long_Float := 0.0;
      Image  : String := Trim (Data);
      F      : constant Natural := Image'First - 1;

   begin

      if Image'Length > 0 then

         case Image (Image'Last) is

            when 'N' | 'S' =>

               if Image'Length > 8 then

                  Result := Long_Float'Value (Image (F + 1..F + 2)) +
                            Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                            Long_Float'Value (Image (F + 7..F + 8)) / 3600.0;

                  if Image (Image'Last) = 'S' then
                     Sign := -1.0;
                  end if;

                  if not (Result in 0.0..90.0) then
                     Ada.Text_IO.Put_Line ("warning: wrong lat value " & Image);
                  end if;

               else
                  Ada.Text_IO.Put_Line ("warning: wrong lat format " & Image);

               end if;

            when 'E' | 'W' =>

               if Image'Length > 9 then

                  Result := Long_Float'Value (Image (F + 1..F + 3)) +
                            Long_Float'Value (Image (F + 5..F + 6)) / 60.0 +
                            Long_Float'Value (Image (F + 8..F + 9)) / 3600.0;

                  if Image (Image'Last) = 'O' then
                     Sign := -1.0;
                  end if;

                  if not (Result in 0.0..180.0) then
                     Ada.Text_IO.Put_Line ("warning: wrong lat value " & Image);
                  end if;

               else
                  Ada.Text_IO.Put_Line ("warning: wrong lat format " & Image);

               end if;

            when others =>

               Ada.Text_IO.Put_Line ("warning: unexpected cardinal direction found " & Image (Image'Last) & " in " & Image);

         end case;

         return Sign * Result;

      else

         Ada.Text_IO.Put_Line ("warning: invalid lat/long");
         return 0.0;

      end if;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Parse_Open_Air_File (File : Ada.Directories.Directory_Entry_Type) is

      use Ada.Directories;
      use Ada.Text_IO;
      use Math.Vector2;

      subtype Vertical_Limit_String is String (1..10);
      No_Vertical_Limit_String : constant Vertical_Limit_String := (others => ' ');

      File_Id     : File_Type;
      Reader      : String_Buffer (200);
      File_Name   : String := Full_Name (File);

      Sector      : Layer_Access     := null;
      Part        : Part_Access      := null;
      X, P1, P2   : Position_Record  := No_Position_Record;
      R, A1, A2   : Long_Float       := 0.0;
      Angle_Delta : Long_Float       := 8.0;
      D           : Character        := '+';
      Max_Points  : constant Natural := 100;

      NE          : Position_Record := No_Position_Record;
      SW          : Position_Record := No_Position_Record;
      NW          : Position_Record := No_Position_Record;
      SE          : Position_Record := No_Position_Record;

      Out_Point   : Position_Record := No_Position_Record;
      Inn_Point   : Position_Record := No_Position_Record;
      End_Point   : Position_Record := No_Position_Record;

      Name        : Part_Names;
      Class       : Character := ' ';
      Kind        : Character := ' ';
      Upper       : Vertical_Limit_String := No_Vertical_Limit_String;
      Lower       : Vertical_Limit_String := No_Vertical_Limit_String;
      Label_Point : Position_Record       := No_Position_Record;
      Label_Loaded: Boolean               := False;

      Close_Sectors : Boolean := False;
      List_Sectors  : Boolean := False;

      --========================================================================
      --
      --========================================================================
      procedure Assign_Sector_Info is
      begin

         if Sector /= null then

            Override (Sector.Info, Kind & '/' & Class & '/' & Trim (Lower) & '/' & Trim (Upper) & '/' & Image (Label_Point));

            -- The label point is loaded for only one sector
            --------------------------------------------------------------------
            if Label_Point /= No_Position_Record then
               Label_Loaded := True;
               Label_Point  := No_Position_Record;
            end if;

         end if;

      end Assign_Sector_Info;
      --------------------------------------------------------------------------



      --========================================================================
      -- Sets the Kind and Class variables based on the given data
      --========================================================================
      procedure Parse_Kind_And_Class (Data : String) is

         Info : String (1..3);

      begin
         Override (Info, Data);

         if Info = "R  " or Info = "P  " or
            Info = "Q  " or Info = "GP " then

            Kind  := 'R';
            Class := 'A';

         elsif Info = "CTR" then

            Kind  := 'C';
            Class := 'C';

         elsif Info = "GLD" or Info = "W  " then

            Kind  := 'G';
            Class := 'G';

         elsif Info = "TMZ" or else Info = "RMZ" then

            Kind  := 'M';
            Class := 'C';

         elsif Info (1) in 'A'..'F' then

            Kind  := 'T';
            Class := Info (1);

         end if;

         Assign_Sector_Info;

      end Parse_Kind_And_Class;
      --------------------------------------------------------------------------




      --========================================================================
      -- Setups the airspace bounds using the command line options
      --========================================================================
      procedure Setup_Configuration is
      begin

         -- Read command line arguments
         -----------------------------------------------------------------------
         for I in 1..Ada.Command_Line.Argument_Count loop

            declare
               Argument : String_Buffer (100);
            begin

               Argument.Load (Ada.Command_Line.Argument (I));

               declare
                  Key : String := Argument.Read_Next ('=');
                  Val : String := Argument.Read_Next ('=');
               begin

                  if    Key = "N" then

                     NE.Lat := Sexagecimal_Value (Val);

                  elsif Key = "E" then

                     NE.Lon := Sexagecimal_Value (Val);

                  elsif Key = "S" then

                     SW.Lat := Sexagecimal_Value (Val);

                  elsif Key = "W" then

                     SW.Lon := Sexagecimal_Value (Val);

                  elsif Key = "LIST" then

                     List_Sectors := True;

                  elsif Key = "CLOSE" then

                     Close_Sectors := True;

                  end if;

               end;

            end;

         end loop;
         -----------------------------------------------------------------------

         NW := (Lat => NE.Lat, Lon => SW.Lon);
         SE := (Lat => SW.Lat, Lon => NE.Lon);

         Ada.Text_Io.Put_Line ("bounds set to NE = " & Image (NE) & " SW = " & Image (SW));

      end Setup_Configuration;
      --------------------------------------------------------------------------




      --========================================================================
      --
      --========================================================================
      function Long_Float_Value (Image : String) return Long_Float is
      begin
         return Long_Float'Value (Trim (Image));
      end Long_Float_Value;
      --------------------------------------------------------------------------


      --========================================================================
      -- Deflate size by reducing resolution where possible
      --========================================================================
      procedure Deflate_Outline is

         P1, P2, P3 : Vector2_Access;
         L1, L2     : Position_Record;
         S          : Float;
         N          : Natural := 0;

      begin

         if Sector = null or Part = null then

            Ada.Text_Io.Put_Line ("error: layer or part not initialized at outline deflation");

            return;

         end if;

         P1 := Part.Points.Get_First_Item;
         P2 := Part.Points.Get_First_Item;
         Part.Points.Get_Next_Item (P2);

         while P2 /= null loop

            L1.Lat := P1.Get_Y;
            L1.Lon := P1.Get_X;

            L2.Lat := P2.Get_Y;
            L2.Lon := P2.Get_X;

            S := Distance (L1, L2);

            if S < 0.250 then
               P3 := P2;
               Part.Points.Get_Next_Item (P2);
               if P2 = null then
                  exit;
               end if;
               Part.Points.Remove_Item   (P3);
               N := N + 1;
            else
               Part.Points.Get_Next_Item (P1);
               Part.Points.Get_Next_Item (P2);
            end if;

         end loop;

         if Part.Points.Get_Count = 1 then
            Part.Points.Clear;
         elsif N > 0 then
            Ada.Text_Io.Put_Line ("info: at " & Trim (Sector.Name) & " part deflated by" & Natural'Image (N) & " points");
         end if;

      end Deflate_Outline;
      --------------------------------------------------------------------------


      --========================================================================
      -- Splits the airspace in chuncks of no more than Max_Points
      --========================================================================
      procedure Split_Part is
      begin

         if Sector = null or Part = null then

            Ada.Text_Io.Put_Line ("error: layer or part not initialized at part splitting");

            return;

         end if;

         declare

            New_Part  : Part_Access;
            Point,
            New_Point : Vector2_Access;

            N : Natural := 1 + Part.Points.Get_Count / Max_Points;
            M : Natural := Part.Points.Get_Count / N;
            C : Natural := 0;

         begin


            N := 1;

            Sector.Parts.Add_Item (New_Part);

            Point := Part.Points.Get_First_Item;

            while Point /= null loop

               New_Part.Points.Add_Item (New_Point);

               New_Point.Copy_From_Access   (Point);

               C := C + 1;

               if C = M then

                  Sector.Parts.Add_Item (New_Part);

                  N := N + 1;
                  C := 1;

               else
                  Part.Points.Get_Next_Item (Point);

               end if;

            end loop;

            Sector.Parts.Remove_Item (Part);

            Part := New_Part;

            Ada.Text_Io.Put_Line ("warning: airspace " & Trim (Sector.Name) & " split in" & Natural'Image (N) & " parts");

         end;

      end Split_Part;
      --------------------------------------------------------------------------


      --========================================================================
      -- Adds a new point to the current airspace. If the point is out of bounds
      -- it is either neglected or added into a new part.
      --========================================================================
      procedure Add_Point (P : Position_Record) is

         Q     : Position_Record := No_Position_Record;
         Point : Vector2_Access  := null;

      begin

         if Sector = null or Part = null then

            Ada.Text_Io.Put_Line ("error: layer or part not initialized at point insert");

            return;

         end if;

         -- Check if point is outside of the region
         -----------------------------------------------------------------------
         if
           P.Lon < SW.Lon or else
           P.Lon > NE.Lon or else
           P.Lat < SW.Lat or else
           P.Lat > NE.Lat
         then

            if Inn_Point /= No_Position_Record then

               -- Add intersection and create a new part
               -----------------------------------------------------------------
               Q := Intersection (Inn_Point, P, SW, NW);
               if Q = No_Position_Record then
                  Q := Intersection (Inn_Point, P, SW, SE);
                  if Q = No_Position_Record then
                     Q := Intersection (Inn_Point, P, NW, NE);
                     if Q = No_Position_Record then
                        Q := Intersection (Inn_Point, P, NE, SE);
                     end if;
                  end if;
               end if;

               if Q /= No_Position_Record then

                  Part.Points.Add_Item (Point);

                  Point.Set_X (Q.Lon);
                  Point.Set_Y (Q.Lat);

                  -- Finish this part
                  --------------------------------------------------------------

                  Deflate_Outline;

                  if Part.Points.Get_Count = 0 then

                     --Ada.Text_Io.Put_Line ("warning: no points in " & Trim (Name));

                     Sector.Parts.Remove_Item (Part);

                     Part := null;

                  elsif Part.Points.Get_Count > Max_Points then

                     Ada.Text_Io.Put_Line ("warning: too many points in " & Trim (Name) & ":" & Natural'Image (Part.Points.Get_Count));

                     Split_Part;

                  end if;

                  -- Add new one to continue
                  --------------------------------------------------------------

                  Sector.Parts.Add_Item (Part);

               else
                  -- NOTE: this is an error because Out_Point is outside and
                  -- P is inside, which is inconsistent
                  --------------------------------------------------------------
                  Ada.Text_Io.Put_Line ("error: no intersection found between " & Image (Inn_Point) & " and " & Image (P) & " for " & Name);

               end if;

            end if;

            Out_Point := P;
            Inn_Point := No_Position_Record;

         else

            if Out_Point /= No_Position_Record then

               -- First add intersection of previous point with region bounds
               -----------------------------------------------------------------
               Q := Intersection (P, Out_Point, SW, NW);
               if Q = No_Position_Record then
                  Q := Intersection (P, Out_Point, SW, SE);
                  if Q = No_Position_Record then
                     Q := Intersection (P, Out_Point, NW, NE);
                     if Q = No_Position_Record then
                        Q := Intersection (P, Out_Point, NE, SE);
                     end if;
                  end if;
               end if;

               if Q /= No_Position_Record then

                  Part.Points.Add_Item (Point);

                  Point.Set_X (Q.Lon);
                  Point.Set_Y (Q.Lat);

               else
                  -- NOTE: this is an error because Out_Point is outside and
                  -- P is inside, which is inconsistent
                  --------------------------------------------------------------
                  Ada.Text_Io.Put_Line ("error: no intersection found between " & Image (Out_Point) & " and " & Image (P) & " for " & Name);

               end if;

            end if;

            Part.Points.Add_Item (Point);

            Point.Set_X (P.Lon);
            Point.Set_Y (P.Lat);

            Out_Point := No_Position_Record;
            Inn_Point := P;

         end if;

         if P /= No_Position_Record and then End_Point = No_Position_Record then
            End_Point := P;
         end if;

      end Add_Point;
      --------------------------------------------------------------------------


      --========================================================================
      --
      --========================================================================
      function Resolve_Angles return Long_Float is

         A : Long_Float := 0.0;

      begin

         if D = '+' then

            if A1 >= A2 then
               A := A2 - A1;
            else
               A := A2 - A1 - Math.TwoPi;
            end if;

         else

            if A1 <= A2 then
               A := A2 - A1;
            else
               A := A2 - A1 + Math.TwoPi;
            end if;

         end if;

         return A;

      end Resolve_Angles;
      --------------------------------------------------------------------------



      --========================================================================
      -- Arc using center X and angles A1 and A2
      --========================================================================
      procedure Generate_Arc_A is

         V : Vector2_Record;
         P : Position_Record;
         A : Long_Float := 0.0;
         N : Natural    := 0;

      begin

         if X = No_Position_Record or else R = 0.0 or else A1 - A2 = 0.0 then
            Ada.Text_IO.Put_Line ("warning: invalid arc data");
            return;
         end if;

         A1 := (90.0 - A1) * Math.Pi / 180.0;

         A2 := (90.0 - A2) * Math.Pi / 180.0;

         A := Resolve_Angles;

         R := 1.852 * R;

         N := Natural (abs A / (Math.Pi / 180.0 * Angle_Delta));

         A := A / Long_Float (N);

         V.Set_From_Polar (A1, R);

         P := Utility.Maps.Position (X, V);

         Add_Point (P);

         for I in 1..N loop

            V.Rotate (A);

            P := Utility.Maps.Position (X, V);

            Add_Point (P);

         end loop;

         --if Label_Point = No_Position_Record then
         --   Label_Point := X;
         --end if;

         A1 := 0.0;
         A2 := 0.0;
         R  := 0.0;
       --X  := No_Position_Record; --> retain center
         P1 := No_Position_Record;
         P2 := No_Position_Record;

      end Generate_Arc_A;
      --------------------------------------------------------------------------



      --========================================================================
      -- Arc using two points P1 -> P2 and center X
      --========================================================================
      procedure Generate_Arc_B is

         V1 : Vector2_Record;
         V2 : Vector2_Record;
         P  : Position_Record;
         A  : Long_Float := 0.0;
         N  : Natural    := 0;

      begin

         if
           X  = No_Position_Record or else
           P1 = No_Position_Record or else
           P2 = No_Position_Record
         then
            Ada.Text_IO.Put_Line ("warning: invalid arc data");
            return;
         end if;

         V1 := Utility.Maps.Vector (X, P1);

         V2 := Utility.Maps.Vector (X, P2);

         A1 := V1.Bearing;

         A2 := V2.Bearing;

         A := Resolve_Angles;

         N := Natural (abs A / (Math.Pi / 180.0 * Angle_Delta));

         A := A / Long_Float (N);

         Add_Point (P1);

         for I in 1..N loop

            V1.Rotate (A);

            P := Utility.Maps.Position (X, V1);

            Add_Point (P);

         end loop;

         --if Label_Point = No_Position_Record then
         --   Label_Point := X;
         --end if;

         A1 := 0.0;
         A2 := 0.0;
         R  := 0.0;
       --X  := No_Position_Record; --> retain center
         P1 := No_Position_Record;
         P2 := No_Position_Record;

      end Generate_Arc_B;
      --------------------------------------------------------------------------



      --========================================================================
      -- Circle using center X and radius D
      --========================================================================
      procedure Generate_Circle is

         V : Vector2_Record;
         P : Position_Record;
         A : Long_Float := 0.0;
         N : Natural    := Natural (360.0 / Angle_Delta);

      begin

         A := Math.TwoPi / Long_Float (N);

         R := 1.852 * R;

         V.Set_From_Polar (0.0, R);

         for I in 0..N loop

            V.Rotate (A);

            P := Utility.Maps.Position (X, V);

            Add_Point (P);

         end loop;

         --if Label_Point = No_Position_Record then
         --   Label_Point := X;
         --end if;

         R  := 0.0;
         X  := No_Position_Record;

      end Generate_Circle;
      --------------------------------------------------------------------------


      --========================================================================
      -- (See specification file)
      --========================================================================
      procedure Compose_Vertical_Limit (Limit : in out String; S1, S2, S3 : String) is

         -----------------------------------------------------------------------
         function Attach_Unit return String is
         begin

            if (S1 = "FL" or S2 = "FT" or S2 = "M") and S2 /= "AMSL" and S2 /= "AGL" then
               return S1 & S2;
            else
               return S1;
            end if;
         end;
         -----------------------------------------------------------------------

      begin

         -- Reset
         Limit := (others => ' ');
         -----------------------------------------------------------------------

         -- Check 'UNL' and 'GND' flags
         -----------------------------------------------------------------------
         if S1 = "UNL" or S1 = "GND" then
            Override (Limit, S1);
            return;
         elsif S1 = "SFC" then
            Override (Limit, "GND");
            return;
         end if;

         declare
            Altitude    : String := Attach_Unit;
            F : Natural := Altitude'First;
            L : Natural := Altitude'Last;
         begin

            -- Check 'FL' preffix
            -----------------------------------------------------------------------
            if
              Altitude'Length > 3   and then
              Altitude (F)   = 'F' and then
              Altitude (F+1) = 'L'
            then
               for J in F+2..L loop
                  if not (Altitude (J) in '0'..'9') then
                     Ada.Text_IO.Put_Line ("warning: wrong vertical limit " & Altitude & Natural'Image (J));
                     return;
                  end if;
               end loop;

            -- Check 'FT' suffix
            -----------------------------------------------------------------------
            elsif
              Altitude'Length > 2  and then
              Altitude (L-1) = 'F' and then
              Altitude (L  ) = 'T'
            then
               for J in F..L-2 loop
                  if not (Altitude (J) in '0'..'9') then
                     Ada.Text_IO.Put_Line ("warning: wrong vertical limit " & Altitude & Natural'Image (J));
                     return;
                  end if;
               end loop;

            -- Check 'M' suffix
            -----------------------------------------------------------------------
            elsif
              Altitude'Length > 1 and then
              Altitude (L) = 'M'
            then
               for J in F..L-2 loop
                  if not (Altitude (J) in '0'..'9') then
                     Ada.Text_IO.Put_Line ("warning: wrong vertical limit " & Altitude & Natural'Image (J));
                     return;
                  end if;
               end loop;
            else
               Ada.Text_IO.Put_Line ("warning: no altitude unit detected in " & S1 & ' ' & S2 & ' ' & S3);
            end if;

            -- Check reference level
            -----------------------------------------------------------------------
            --if    S2 = "AGL" or else S3 = "AGL" then
            --   Override (Limit, Altitude & 'G' & S2);
            --elsif S2 = "AMSL" or else S3 = "AMSL" then
            --   Override (Limit, Altitude & 'S' & S3);
            --else
            Override (Limit, Altitude);
            --end if;

         end;

      end Compose_Vertical_Limit;
      --------------------------------------------------------------------------



      --========================================================================
      -- Finishes the active sector
      --========================================================================
      procedure Finish_Sector is
      begin

         -- Finish previous one
         -----------------------------------------------------------
         if Sector /= null and Part /= null then

            -- Close if necessary
            --------------------------------------------------------
            if Close_Sectors then

               Add_Point (End_Point);

               End_Point := No_Position_Record;

            end if;

            if Sector.Name = No_Part_Name then
               Ada.Text_IO.Put_Line ("warning: unamed airspace");
            end if;

            -- Place label at geometric center if not declared yet
            --------------------------------------------------------
            if not Label_Loaded then

               declare
                  L : Vector2_Record := Math.Tools.Get_Geometric_Center (Part.Points);
               begin
                  Label_Point.Lat := L.Get_Y;
                  Label_Point.Lon := L.Get_X;
               end;

            end if;

            -- Warn if incomplete data
            --------------------------------------------------------
            if Upper = No_Vertical_Limit_String then
               Ada.Text_IO.Put_Line ("warning: no upper limit in " & Trim (Sector.Name));
            end if;

            if Lower = No_Vertical_Limit_String then
               Ada.Text_IO.Put_Line ("warning: no lower limit in " & Trim (Sector.Name));
            end if;

            Assign_Sector_Info; --?

            Deflate_Outline;

            if Part.Points.Get_Count = 0 then

               Ada.Text_Io.Put_Line ("warning: no points in " & Trim (Sector.Name));
               Sector.Parts.Remove_Item (Part);

            elsif Part.Points.Get_Count > Max_Points then

               Ada.Text_Io.Put_Line ("warning: too many points in part of layer " & Trim (Sector.Name) & ":" & Natural'Image (Part.Points.Get_Count));
               Split_Part;

            end if;

            -- Remove if empty
            --------------------------------------------------------
            declare
               Empty : Boolean := True;
            begin

               Part := Sector.Parts.Get_First_Item;

               while Part /= null loop

                  if Part.Points.Get_Count > 1 then

                     Empty := False;

                     exit;

                  end if;

                  Sector.Parts.Get_Next_Item (Part);

               end loop;

               if Empty then

                  Airspaces.Remove_Item (Sector);

               end if;

            end;

            Part   := null;

            Sector := null;

         end if;

      end Finish_Sector;
      --------------------------------------------------------------------------

   begin

      if Ada.Directories.Exists (File_Name) then

         Ada.Text_Io.Put_Line ("reading OpenAir file " & File_Name);

         Setup_Configuration;

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         while not End_Of_File (File_Id) loop

            Reader.Load (Get_Upper_Case (Trim (Ada.Text_IO.Get_Line (File_Id))));

            declare
               Key : String := Reader.Read_Next (Multiple => True);
            begin

               if Key'Last > 0 and then Key (Key'First) /= '*' then

                  if Key = "AC" then

                     -- Finish last sector
                     -----------------------------------------------------------

                     Finish_Sector;

                     -- Reset variables
                     -----------------------------------------------------------

                     Upper        := No_Vertical_Limit_String;
                     Lower        := No_Vertical_Limit_String;
                     Out_Point    := No_Position_Record;
                     Inn_Point    := No_Position_Record;
                     Label_Point  := No_Position_Record;
                     Label_Loaded := False;
                     D            := '+';

                     -- Add new one
                     -----------------------------------------------------------

                     if Airspaces.Get_Count >= 500 then
                        Ada.Text_IO.Put_Line ("warning: reached the limit of sectors");
                        return;
                     end if;

                     Airspaces.Add_Item (Sector);

                     Sector.Parts.Add_Item (Part);

                     Parse_Kind_And_Class (Reader.Read_Next (Multiple => True));

                  elsif Sector /= null and Part /= null then

                     if
                       Key = "AN"
                     then

                        Override (Name, Reader.Read_Next (Separator => '*'));

                        Sector.Name := Name;

                     elsif Key = "AY" then

                        Parse_Kind_And_Class (Reader.Read_Next (Multiple => True));

                     elsif
                       Key = "AL"
                     then

                        declare
                           Limit_1 : String := Trim (Reader.Read_Next (Multiple => True));
                           Limit_2 : String := Trim (Reader.Read_Next (Multiple => True));
                           Limit_3 : String := Trim (Reader.Read_Next (Multiple => True));
                        begin

                           Compose_Vertical_Limit (Lower, Limit_1, Limit_2, Limit_3);

                        end;

                     elsif
                       Key = "AH"
                     then

                        declare
                           Limit_1 : String := Trim (Reader.Read_Next (Multiple => True));
                           Limit_2 : String := Trim (Reader.Read_Next (Multiple => True));
                           Limit_3 : String := Trim (Reader.Read_Next (Multiple => True));
                        begin

                           Compose_Vertical_Limit (Upper, Limit_1, Limit_2, Limit_3);

                        end;

                     elsif Key = "AT" then

                        Label_Point := (Lat => Sexagecimal_Value (Reader.Read_Next (Multiple => True)),
                                        Lon => Sexagecimal_Value (Reader.Read_Next (Multiple => True)));

                     elsif
                       Key = "DP"
                     then

                        Add_Point ((Lat => Sexagecimal_Value (Reader.Read_Next (Multiple => True)),
                                    Lon => Sexagecimal_Value (Reader.Read_Next (Multiple => True))));

                     elsif
                       Key = "DA"
                     then

                        -- Arc using R, A1, A2
                        --------------------------------------------------------

                        R  := Long_Float_Value (Reader.Read_Next (',', Multiple => True));
                        A1 := Long_Float_Value (Reader.Read_Next (',', Multiple => True));
                        A2 := Long_Float_Value (Reader.Read_Next (',', Multiple => True));

                        if abs (A1 - A2) >= 360.0 then

                           Generate_Circle;

                        else
                           Generate_Arc_A;

                        end if;

                     elsif
                       Key = "DB"
                     then

                        -- Arc using P1, P2, V as center and D as direction
                        --------------------------------------------------------

                        P1.Lat := Sexagecimal_Value (Reader.Read_Next (' ', Multiple => True));
                        P1.Lon := Sexagecimal_Value (Reader.Read_Next (',', Multiple => True));

                        P2.Lat := Sexagecimal_Value (Reader.Read_Next (' ', Multiple => True));
                        P2.Lon := Sexagecimal_Value (Reader.Read_Next (' ', Multiple => True));

                        Generate_Arc_B;

                     elsif
                       Key = "DC"
                     then

                        -- Circle using R
                        --------------------------------------------------------

                        R := Long_Float_Value (Reader.Read_Next (',', Multiple => True));

                        Generate_Circle;

                     elsif
                       Key = "V"
                     then

                        declare
                           Var_Type : String := Trim (Reader.Read_Next ('=', Multiple => True));
                        begin

                           if Var_Type'Length > 0 then

                              if
                                Var_Type = "X"
                              then

                                 X.Lat := Sexagecimal_Value (Reader.Read_Next (' ', Multiple => True));
                                 X.Lon := Sexagecimal_Value (Reader.Read_Next (' ', Multiple => True));

                              elsif
                                Var_Type = "D"
                              then

                                 if Trim (Reader.Read_Next (Multiple => True)) = "-" then
                                    D := '-';
                                 else
                                    D := '+';
                                 end if;

                              end if;

                           else
                              Ada.Text_IO.Put_Line ("warning: no variable type given");

                           end if;

                        end;

                     end if;

                  end if;

               end if;

            exception
               when E : others =>
                  Close (File_Id);
                  Ada.Text_IO.Put_Line ("error: " & Ada.Exceptions.Exception_Message (E));
                  Reader.Dump_Content;
                  return;

            end;

         end loop;

         Close (File_Id);

      else
         Ada.Text_IO.Put_Line ("error: file '" & File_Name & "' not found");

      end if;

      -- Finish last airspace if required
      --------------------------------------------------------------------------
      Finish_Sector;

      -- Organize the stack according to the kind so that the representation is
      -- correctly stacked.
      --------------------------------------------------------------------------

      declare
         Order : constant String (1..5) := "GCRTE";
         Sector_To_Move : Layer_Access := null;
      begin

         for K of reverse Order loop

            Sector := Airspaces.Get_First_Item;

            while Sector /= null loop

               if Sector.Info (1) = K then

                  Sector_To_Move := Sector;

                  Airspaces.Get_Next_Item (Sector);

                  Airspaces.Move_To_First (Sector_To_Move);

               else
                  Airspaces.Get_Next_Item (Sector);

               end if;

            end loop;

         end loop;

      end;

      if List_Sectors then

         Sector := Airspaces.Get_First_Item;

         while Sector /= null loop

            Ada.Text_IO.Put_Line ("{" & Sector.Name & "}{" & Sector.Info & "}" & Natural'Image (Sector.Parts.Get_Count));

            Part := Sector.Parts.Get_First_Item;

            while Part /= null loop

               Ada.Text_IO.Put_Line ("   " & Natural'Image (Part.Points.Get_Count));

               Sector.Parts.Get_Next_Item (Part);

            end loop;

            Airspaces.Get_Next_Item (Sector);

         end loop;

      end if;

   end Parse_Open_Air_File;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Open_Air_Files is

      use Ada.Directories;

   begin

      Airspaces.Clear;

      Search (Directory => "./",
              Pattern   => "*.air",
              Process   => Parse_Open_Air_File'Access);

   end Load_Open_Air_Files;
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
      L, P, N    : Natural := 0;

   begin

      if Airspaces.Get_Count > 0 then

         Ada.Text_Io.Put_Line ("computing bounds");
         Compute_Bounds (Airspaces);
         Compute_Global_Bounds (Airspaces, South_West, North_East);
         Ada.Text_Io.Put_Line ("NE: " & Maps.Image (North_East));
         Ada.Text_Io.Put_Line ("SW: " & Maps.Image (South_West));

         Origin.Lat := 0.5 * (South_West.Lat + North_East.Lat);
         Origin.Lon := 0.5 * (South_West.Lon + North_East.Lon);

         Ada.Text_Io.Put_Line ("compiling airspaces");
         Ada.Text_Io.Put_Line ("origin: " & Maps.Image (Origin));

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "airspaces.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Long_Float'Write (Stream, Origin.Lat);    --  8 bytes
         Long_Float'Write (Stream, Origin.Lon);    --  8 bytes

         Natural'Write (Stream, Airspaces.Get_Count); --  4 bytes

         Layer := Airspaces.Get_First_Item;

         while Layer /= null loop

            --Ada.Text_Io.Put_Line ("AIRSPACES:");

            Part_Names'Write (Stream, Layer.Name); -- 20 bytes
            Part_Infos'Write (Stream, Layer.Info); -- 40 bytes

            Float'Write (Stream, Float (Layer.South_West.Lat - Origin.Lat)); -- 4 bytes
            Float'Write (Stream, Float (Layer.South_West.Lon - Origin.Lon)); -- 4 bytes
            Float'Write (Stream, Float (Layer.North_East.Lat - Origin.Lat)); -- 4 bytes
            Float'Write (Stream, Float (Layer.North_East.Lon - Origin.Lon)); -- 4 bytes

            Natural'Write (Stream, Layer.Parts.Get_Count);  -- 4 bytes

            Part := Layer.Parts.Get_First_Item;

            while Part /= null loop

               --Ada.Text_Io.Put_Line (Part.Name);

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

               N := N + Part.Points.Get_Count;

               Layer.Parts.Get_Next_Item (Part);

            end loop;

            Airspaces.Get_Next_Item (Layer);

         end loop;

         Close (File_Id);

         Ada.Text_Io.Put_Line ("result:");
         Ada.Text_Io.Put_Line (Natural'Image (Airspaces.Get_Count) & " sectors compiled");
         Ada.Text_Io.Put_Line (Natural'Image (P) & " parts compiled");
         Ada.Text_Io.Put_Line (Natural'Image (N) & " nodes in total");

      end if;

   exception
      when E : others =>
         Close (File_Id);
         Ada.Text_IO.Put_Line ("error while compiling the airspaces: " & Ada.Exceptions.Exception_Message (E));

   end Compile_Data;
   -----------------------------------------------------------------------------

end Layers.Airspaces;
--------------------------------------------------------------------------------
