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


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Invalid_Lat_Long : exception;

   --===========================================================================
   -- Specific function for airspace position data (Eg. 505130N 0031110E)
   --===========================================================================
   function Sexagecimal_Value (Image : String) return Long_Float is

      Sign   : Long_Float := 1.0;
      Result : Long_Float := 0.0;
      F      : constant Natural := Image'First - 1;

   begin

      if Image'Length > 0 then

         case Image (Image'Last) is

         when 'N' | 'S' =>

            Result := Long_Float'Value (Image (F + 1..F + 2)) +
                      Long_Float'Value (Image (F + 3..F + 4)) / 60.0 +
                      Long_Float'Value (Image (F + 5..F + 6)) / 3600.0;

            if Image (Image'Last) = 'S' then
               Sign := -1.0;
            end if;

         when 'E' | 'O' =>

            Result := Long_Float'Value (Image (F + 1..F + 3)) +
                      Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                      Long_Float'Value (Image (F + 6..F + 7)) / 3600.0;

            if Image (Image'Last) = 'O' then
               Sign := -1.0;
            end if;

         when others =>

            case Image (Image'First) is

               when 'N' | 'S' =>

                  Result := Long_Float'Value (Image (F + 2..F + 3)) +
                            Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                            Long_Float'Value (Image (F + 6..F + 7)) / 3600.0;

                  if Image (Image'First) = 'S' then
                     Sign := -1.0;
                  end if;

               when 'E' | 'O' =>

                  Result := Long_Float'Value (Image (F + 2..F + 4)) +
                            Long_Float'Value (Image (F + 5..F + 6)) / 60.0 +
                            Long_Float'Value (Image (F + 7..F + 8)) / 3600.0;

                  if Image (Image'First) = 'O' then
                     Sign := -1.0;
                  end if;

               when others =>

                  null;

            end case;

         end case;

         return Sign * Result;

      else

         Ada.Text_IO.Put_Line ("warning: invalid lat/long");

         raise Invalid_Lat_Long;

      end if;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Reads a text file containing AIP namespaces and converts them into
   -- shape files and metadata.
   --===========================================================================
   procedure Generate_Airspaces is

      use Ada.Text_IO;
      use Math.Vector2;

      type Data_Kinds is (Invalid_Data,       -- Line skipper
                          Airspace_Name,      -- 1st line
                          Airspace_Info,      -- 2nd line
                          Airspace_Boundary); -- 3th line and on

      File_Id    : File_Type;

      Reader     : String_Buffer (100);

      Points     : Math.Vector2_List.Stack;

      Data_Kind  : Data_Kinds := Airspace_Name;

      Border     : Layer_Access := null;

      Airspace   : Part_Access  := null;

      File_Name  : String := "files/airspaces.dat";

      Skip_Block : Boolean := False;

      P          : Vector2_Access := null;

      --========================================================================
      -- Announces an error and invalidates the current airspace
      --========================================================================
      procedure Announce_Error (Message : String) is
      begin

         if Airspace /= null then

            Ada.Text_Io.Put_Line ("error: " & Message);

            Ada.Text_Io.Put_Line ("airspace '" &  Trim (Airspace.Name) & "' won't be included");

            Airspaces.Remove_Item (Airspace);

            Data_Kind := Invalid_Data;

         end if;

      end Announce_Error;
      --------------------------------------------------------------------------

      --========================================================================
      -- Indicates if the line can be used to extract data
      -- Note: the '#' character is used for comments
      --========================================================================
      function Valid_Line (Text : String) return Boolean is
      begin

         return Text'Length > 0 and then Text (Text'First) /= '#';

      end Valid_Line;
      --------------------------------------------------------------------------

   begin

      if Ada.Directories.Exists (File_Name) then

         Ada.Text_Io.Put_Line ("generating airspaces");

         Airspaces.Clear;

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         while not End_Of_File (File_Id) loop

            declare
               Line : String := Trim (Ada.Text_IO.Get_Line (File_Id));
            begin

               if Line'Length = 0 then

                  -- Finish the active airspace (if not deleted due to error)
                  --------------------------------------------------------------

                  if Airspace /= null then

                     -- NOTE: we consider the list ends where it started, so
                     -- the last node is removed

                     P := Airspace.Points.Get_First_Item;

                     Airspace.Points.Remove_Item (P);

                     Airspace := null;

                  end if;

                  -- Expect a new airspace on the next line
                  --------------------------------------------------------------

                  Data_Kind := Airspace_Name;

               elsif Valid_Line (Line) then

                  -- Keep building active airspace
                  --------------------------------------------------------------

                  case Data_Kind is

                     when Invalid_Data =>

                        null;

                     when Airspace_Name =>

                        Airspaces.Add_Item (Airspace);

                        Override (Airspace.Name, Line);

                      --Ada.Text_Io.Put_Line ("adding new airspace " & Line);

                        Data_Kind := Airspace_Info;

                     when Airspace_Info =>

                        Override (Airspace.Info, Line);

                        Data_Kind := Airspace_Boundary;

                     when Airspace_Boundary =>

                        Reader.Load (Line);

                        declare
                           Flag : String := Trim (Reader.Read_Next (' '));
                        begin

                           if
                             Flag = "BORDER"
                           then

                              if Airspace.Points.Get_Count = 0 then

                                 Announce_Error ("expected point before border");

                                 goto Next_Line;

                              end if;

                              -- Search for the matching border line
                              --------------------------------------------------

                              declare
                                 Border_Name : String  := Trim (Reader.Read_Next (' '));
                                 F           : Natural := Border_Name'First - 1;
                                 Border_1    : Part_Names := No_Part_Name;
                                 Border_2    : Part_Names := No_Part_Name;
                              begin

                                 if Border_Name'Length = 5 then

                                    Override (Border_1, Border_Name (F + 1..F + 2) & "-" & Border_Name (F + 4..F + 5) & "_border");

                                    Override (Border_2, Border_Name (F + 4..F + 5) & "-" & Border_Name (F + 1..F + 2) & "_border");

                                    Border := Layers.Get_First_Item;

                                    while Border /= null loop

                                       if
                                          Border.Name = Border_1 or
                                          Border.Name = Border_2
                                       then

                                          --Ada.Text_Io.Put_Line ("border " & Trim (Border.Name) & " found");

                                          exit;

                                       end if;

                                       Layers.Get_Next_Item (Border);

                                    end loop;

                                    if Border = null then

                                       Ada.Text_Io.Put_Line ("error: borders " & Trim (Border_1) & " or " & Trim (Border_2) & " not found");

                                    end if;

                                 else

                                    Ada.Text_Io.Put_Line ("invalid border line name");

                                 end if;

                              end;

                           elsif
                             Flag = "ARC"
                           then

                              if Border /= null then

                                 Announce_Error ("expected point after border line");

                                 goto Next_Line;

                              end if;

                              declare
                                 Radius : String := Reader.Read_Next (' ');
                                 Unit   : String := Reader.Read_Next (' ');
                                 PC_Lat : String := Reader.Read_Next (' ');
                                 PC_Lon : String := Reader.Read_Next (' ');
                                 Sense  : String := Reader.Read_Next (' ');
                                 P2_Lat : String := Reader.Read_Next (' ');
                                 P2_Lon : String := Reader.Read_Next (' ');
                                 P1     : Position_Record;
                                 P2     : Position_Record;
                                 PC     : Position_Record;
                                 V1     : Vector2_Record;
                                 V2     : Vector2_Record;
                                 R      : Position_Record;
                                 A1     : Long_Float := 0.0;
                                 A2     : Long_Float := 0.0;
                                 A      : Long_Float := 0.0;
                                 N      : Natural    := 0;
                              begin

                                 P := Airspace.Points.Get_Last_Item;

                                 P1.Lat := P.Get_Y;

                                 P1.Lon := P.Get_X;

                                 P2.Lat := Sexagecimal_Value (P2_Lat);

                                 P2.Lon := Sexagecimal_Value (P2_Lon);

                                 PC.Lat := Sexagecimal_Value (PC_Lat);

                                 PC.Lon := Sexagecimal_Value (PC_Lon);

                                 V1 := Utility.Maps.Vector (PC, P1);

                                 V2 := Utility.Maps.Vector (PC, P2);

                                 A1 := V1.Bearing;

                                 A2 := V2.Bearing;

                                 if Sense = ">" then

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

                                 -- NOTE: Arcs have a resolution of 5 degrees

                                 N := Natural (abs A / (Math.Pi / 180.0 * 5.0));

                                 A := A / Long_Float (N);

                                 for I in 1..N loop

                                    V1.Rotate (A);

                                    R := Utility.Maps.Position (PC, V1);

                                    Airspace.Points.Add_Item (P);

                                    P.Set (R.Lon, R.Lat);

                                 end loop;

                              end;

                           elsif
                             Flag = "CIRCLE"
                           then

                              if Border /= null then

                                 Announce_Error ("expected point after border line");

                                 goto Next_Line;

                              end if;

                              declare
                                 Radius : String := Reader.Read_Next (' ');
                                 Unit   : String := Reader.Read_Next (' ');
                                 C_Lat  : String := Reader.Read_Next (' ');
                                 C_Lon  : String := Reader.Read_Next (' ');
                                 C      : Position_Record;
                                 V      : Vector2_Record;
                                 Q      : Position_Record;
                                 R      : Long_Float := 0.0;
                                 A      : Long_Float := 0.0;
                                 N      : Natural    := 0;
                              begin

                                 C.Lat := Sexagecimal_Value (C_Lat);

                                 C.Lon := Sexagecimal_Value (C_Lon);

                                 R     := Long_Float'Value (Radius);

                                 V.Set (R, 0.0);

                                 N := Natural'Max (72, Natural'Min (8, Natural ((2.0 * Math.Pi * R) / 0.2)));

                                 A := 2.0 * Math.Pi / Long_Float (N);

                                 for I in 1..N loop

                                    V.Rotate (A);

                                    Q := Maps.Position (C, V);

                                    Airspace.Points.Add_Item (P);

                                    P.Set (Q.Lon, Q.Lat);

                                 end loop;

                              end;

                           elsif Line'Length = 16 then

                              if Border /= null then

                                 -- Complete the pending border line
                                 -----------------------------------------------

                                 declare
                                    P1 : Vector2_Record := No_Vector2_Record;
                                    P2 : Vector2_Record := No_Vector2_Record;
                                    C1 : Vector2_Record := No_Vector2_Record;
                                    C2 : Vector2_Record := No_Vector2_Record;
                                    Bd : Part_Access    := null;
                                    S1 : Natural := 0;
                                    S2 : Natural := 0;
                                 begin

                                    P := Airspace.Points.Get_Last_Item;

                                    P1.Copy_From_Access (P);

                                    Airspace.Points.Remove_Item (P);

                                    P2.Set (Sexagecimal_Value (Line (9..16)),
                                            Sexagecimal_Value (Line (1.. 7)));

                                    declare
                                       Bl : Part_Access    := null;
                                       R1 : Vector2_Record := No_Vector2_Record;
                                       R2 : Vector2_Record := No_Vector2_Record;
                                       O1 : Long_Float     := Long_Float'Last;
                                       O2 : Long_Float     := Long_Float'Last;
                                       A1 : Natural        := 0;
                                       A2 : Natural        := 0;
                                       D1 : Long_Float     := Long_Float'Last;
                                       D2 : Long_Float     := Long_Float'Last;
                                    begin

                                       Bl := Border.Parts.Get_First_Item;

                                       while Bl /= null loop

                                          if Bl.Points.Get_Count > 0 then

                                             Math.Tools.Get_Closest_Point (Bl.Points, P1, False, R1, O1, A1);
                                             Math.Tools.Get_Closest_Point (Bl.Points, P2, False, R2, O2, A2);

                                             if O1 < D1 and O2 < D2 then

                                                Bd := Bl;
                                                D1 := O1;
                                                D2 := O2;
                                                S1 := A1;
                                                S2 := A2;
                                                C1 := R1;
                                                C2 := R2;

                                             end if;

                                          else

                                             Ada.Text_Io.Put_Line ("warning: no points loaded for border line");

                                          end if;

                                          Border.Parts.Get_Next_Item (Bl);

                                       end loop;

                                    end;

                                    if Bd /= null then

                                       Airspace.Points.Add_Item (P);

                                       P.Copy_From_Record (C1);

                                       if S1 < S2 then

                                          for I in S1+1..S2 loop

                                             Airspace.Points.Add_Item (P);

                                             P.Copy_From_Access (Bd.Points.Get_Item (I));

                                          end loop;

                                       elsif S2 < S1 then

                                          for I in reverse S2+1..S1 loop

                                             Airspace.Points.Add_Item (P);

                                             P.Copy_From_Access (Bd.Points.Get_Item (I));

                                          end loop;

                                       end if;

                                       Airspace.Points.Add_Item (P);

                                       P.Copy_From_Record (C2);

                                    else

                                       Announce_Error ("the border line could not be completed");

                                       goto Next_Line;

                                    end if;

                                 end;

                                 Border := null;

                              else

                                 -- Load the point
                                 -----------------------------------------------

                                 Airspace.Points.Add_Item (P);

                                 P.Set (Sexagecimal_Value (Line (9..16)),
                                        Sexagecimal_Value (Line (1.. 7)));

                              end if;

                           else

                              Ada.Text_Io.Put_Line ("unrecognized line '" & Line & "'");

                           end if;

                        end;

                  end case;

               end if;

            exception
               when E : others =>
                  Ada.Text_Io.Put_Line ("error while decoding line """ & Line & """");

            end;

            <<Next_Line>>

         end loop;

      end if;

   exception
      when E : others =>

         Ada.Text_Io.Put_Line ("while generating airspace");

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

   end Generate_Airspaces;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- Scans all the data, finds the local bounds and returns the global bounds
   --===========================================================================
   procedure Search_Global_Bounds (South_West, North_East : out Position_Record) is

      use Math.Vector2;

      Part  : Part_Access;
      Point : Vector2_Access;

      First_Global : Boolean := True;
      First_Local  : Boolean := True;

   begin

      South_West := (0.0, 0.0);
      North_East := (0.0, 0.0);

      Part := Airspaces.Get_First_Item;

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

         Airspaces.Get_Next_Item (Part);

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
      Part       : Part_Access;
      Point      : Vector2_Access;
      South_West,
      North_East : Position_Record;
      Origin     : Position_Record;
      L, P       : Natural := 0;

   begin

      if Airspaces.Get_Count > 0 then

         Search_Global_Bounds (South_West, North_East);

         Origin.Lat := 0.5 * (South_West.Lat + North_East.Lat);
         Origin.Lon := 0.5 * (South_West.Lon + North_East.Lon);

         Ada.Text_Io.Put_Line ("compiling airspaces");
         Ada.Text_Io.Put_Line ("origin: " & Maps.Image (Origin));

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "files/airspaces.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Long_Float'Write (Stream, Origin.Lat);    --  8 bytes
         Long_Float'Write (Stream, Origin.Lon);    --  8 bytes

         Natural'Write (Stream, Airspaces.Get_Count); --  4 bytes

         Part := Airspaces.Get_First_Item;

         while Part /= null loop

            P := P + 1;

            Part_Names'Write (Stream, Part.Name); -- 20 bytes
            Part_Infos'Write (Stream, Part.Info); -- 40 bytes

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

            Airspaces.Get_Next_Item (Part);

         end loop;

         Close (File_Id);

         Ada.Text_Io.Put_Line (Natural'Image (P) & " parts compiled");

      end if;

   end Compile_Data;
   -----------------------------------------------------------------------------

end Layers.Airspaces;
--------------------------------------------------------------------------------
