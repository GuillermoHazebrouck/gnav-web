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
with Ada.Streams;
use  Ada.Streams;
with Interfaces;
-- Gnav
with Glex.Fonts;
with Glex.Symbols;
with Utility.Atmosphere;
use  Utility.Atmosphere;
with Utility.Log;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;
with Utility.Storage;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Airspaces is

   --===========================================================================
   --
   --===========================================================================
   procedure Load_Configuration;

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
         Utility.Log.Put_Message ("warning: invalid lat/long");
         return 0.0;

      end if;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Recomputes the altitude string based on the current units and QNH
   --===========================================================================
   procedure Recompute (This : in out Airspace_Record) is
   begin

      This.Label.Lower := To_Altitude (This.Limits.Lower);
      This.Label.Upper := To_Altitude (This.Limits.Upper);

      This.Lower_Limit := To_Altitude (This.Limits.Lower);
      This.Upper_Limit := To_Altitude (This.Limits.Upper);

   end Recompute;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Parse_Info (This : in out Airspace_Record; Info : String) is

      use Utility.Strings;

      Reader : String_Buffer (40);

   begin

      Reader.Load (Info);

      declare
         Kind : String := Reader.Read_Next ('/');
      begin
            if Kind = "C" then This.Kind := Airspace_Ctr;
         elsif Kind = "T" then This.Kind := Airspace_Tma;
         elsif Kind = "M" then This.Kind := Airspace_Tmz;
         elsif Kind = "E" then This.Kind := Airspace_Cta;
         elsif Kind = "R" then This.Kind := Airspace_Res;
         elsif Kind = "G" then This.Kind := Airspace_Gld;
         else                  This.Kind := Airspace_Unk;
         end if;
      end;

      Override (This.Class,        Reader.Read_Next ('/'));
      Override (This.Limits.Lower, Reader.Read_Next ('/'));
      Override (This.Limits.Upper, Reader.Read_Next ('/'));
    --This.Point := Maps.Value (Reader.Read_Next ('-'));

      This.Recompute;

   end Parse_Info;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Updates the vertical sector limits based on the current QNH and unit
   --===========================================================================
   procedure Recompute_Vertical_Limits is
   begin

      for A of Airspaces loop

         if A.Loaded then
            A.Recompute;
         end if;

      end loop;

   end Recompute_Vertical_Limits;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads the reference through a server request
   --===========================================================================
   procedure Load_Airspaces (S : in out Stream_Reader_Type) is

      P : Natural := 0; -- parts
      N : Natural := 0; -- nodes
      J : Natural := 0; -- part counter
      R : Natural := 0; -- node counter

      X, Y   : Float;
      Origin : Position_Record;

   begin

      if S.Is_Empty then
         return;
      end if;

      -- Data origin
         ----------------------------------------------
      Origin.Lat := S.Read_Long_Float;
      Origin.Lon := S.Read_Long_Float;

      -- Number of airspaces
         ----------------------------------------------
      Number_Of_Airspaces := S.Read_Natural;

      Utility.Log.Put_Message ("loading " & Integer_Image (Number_Of_Airspaces) & " airspaces");
      Utility.Log.Put_Message ("stream size = " & Integer_Image (S.Get_Size));

      for I in 1 .. Number_Of_Airspaces loop

         if I > Airspaces'Last then
            Utility.Log.Put_Message ("warning: airspace resources full");
            return;
         end if;

         -- Airspace name
         -- TODO: split in name, callsign and frequency
         ----------------------------------------------
         Override (Airspaces (I).Name, S.Read_String (20));

         -- Airspace info
         ----------------------------------------------
         Airspaces (I).Parse_Info (S.Read_String (40));

         -- Airspace bounds
         ----------------------------------------------
         Airspaces (I).South_West.Lat := Long_Float (S.Read_Float) + Origin.Lat;
         Airspaces (I).South_West.Lon := Long_Float (S.Read_Float) + Origin.Lon;
         Airspaces (I).North_East.Lat := Long_Float (S.Read_Float) + Origin.Lat;
         Airspaces (I).North_East.Lon := Long_Float (S.Read_Float) + Origin.Lon;

         Airspaces (I).Center.Lat := 0.5 * (Airspaces (I).North_East.Lat + Airspaces (I).South_West.Lat);
         Airspaces (I).Center.Lon := 0.5 * (Airspaces (I).North_East.Lon + Airspaces (I).South_West.Lon);

         -- Number of parts
         ----------------------------------------------
         P := S.Read_Natural;

         for K in 1 .. P loop

            J := J + 1;

            if J > Parts'Last then
               Utility.Log.Put_Message ("warning: airspaces part resources full");
               return;
            end if;

            if K = 1 then
               Airspaces (I).First := J;
               Airspaces (I).Last  := J;
            else
               Airspaces (I).Last  := J;
            end if;

            -- Part bounds
            ----------------------------------------------
            Parts (J).South_West.Lat := Long_Float (S.Read_Float) + Origin.Lat;
            Parts (J).South_West.Lon := Long_Float (S.Read_Float) + Origin.Lon;
            Parts (J).North_East.Lat := Long_Float (S.Read_Float) + Origin.Lat;
            Parts (J).North_East.Lon := Long_Float (S.Read_Float) + Origin.Lon;

            -- Airspace points
            ----------------------------------------------
            N := S.Read_Natural;

            if N > Lines_Limit then
               Utility.Log.Put_Message ("warning: airspace line too long");
            end if;

            Parts (J).First := R + 1;

            Lines_Buffer.Reset;

            for C in 1 .. N loop

               Y := S.Read_Float;
               X := S.Read_Float;

               R := R + 1; --> TODO: even if checked by Crunch, protect buffer range

               Nodes_Buffer (R).Y := Y + Float (Origin.Lat - Airspaces (I).Center.Lat);
               Nodes_Buffer (R).X := X + Float (Origin.Lon - Airspaces (I).Center.Lon);

               Y := Y + Float (Origin.Lat - Center.Lat);
               X := X + Float (Origin.Lon - Center.Lon);

               Lines_Buffer.Line_To (X * Maps.Shrink, Y);

            end loop;

            Lines_Buffer.Finish;

            Parts (J).Resource.Load (Lines_Buffer);

            Parts (J).Last := R;

         end loop;

         Airspaces (I).Loaded := P > 0;

      end loop;

      Load_Configuration;

   end Load_Airspaces;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      Allow_Notifications := Utility.Storage.Get_Item ("ALERTS") = "Y";

      Utility.Resources.Request_Binary_Resource ("airspaces.bin", Load_Airspaces'Access);

      Utility.Atmosphere.On_Qnh_Changed.Connect (Recompute_Vertical_Limits'Access);

      Utility.Atmosphere.On_Altitude_Unit_Changed.Connect (Recompute_Vertical_Limits'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (View : Map_View_Record) is

      F : Natural renames Focused_Airspace;

   begin

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      if F in Airspaces'Range then

         for I in 1 .. F-1 loop

            if Airspaces (I).Loaded then

               for J in Airspaces (I).First .. Airspaces (I).Last loop

                  if View.On_Clip (Parts (J).North_East,
                                   Parts (J).South_West)
                  then
                     Parts (J).Resource.Draw (Color_Gray_8,
                                              Thickness (Airspaces (I).Kind));
                  end if;

               end loop;

            end if;

         end loop;

         for I in F+1 ..Number_Of_Airspaces loop

            if Airspaces (I).Loaded then

               for J in Airspaces (I).First .. Airspaces (I).Last loop

                  if View.On_Clip (Parts (J).North_East,
                                   Parts (J).South_West)
                  then
                     Parts (J).Resource.Draw (Color_Gray_8,
                                              Thickness (Airspaces (I).Kind));
                  end if;

               end loop;

            end if;

         end loop;

         for J in Airspaces (F).First .. Airspaces (F).Last loop

            if View.On_Clip (Parts (J).North_East,
                             Parts (J).South_West)
            then
               Parts (J).Resource.Draw (Colors    (Airspaces (F).Kind),
                                        Thickness (Airspaces (F).Kind));
            end if;

         end loop;

      else

         for I in 1 .. Number_Of_Airspaces loop

            if
              Airspaces (I).Loaded and then not
              Airspaces (I).Active
            then

               for J in Airspaces (I).First .. Airspaces (I).Last loop

                  if View.On_Clip (Parts (J).North_East,
                                   Parts (J).South_West)
                  then
                     Parts (J).Resource.Draw (Color_Gray_8,
                                              Thickness (Airspaces (I).Kind));
                  end if;

               end loop;

            end if;

         end loop;

         for I in 1 .. Number_Of_Airspaces loop

            if
              Airspaces (I).Loaded and then
              Airspaces (I).Active
            then

               for J in Airspaces (I).First .. Airspaces (I).Last loop

                  if View.On_Clip (Parts (J).North_East,
                                   Parts (J).South_West)
                  then
                     Parts (J).Resource.Draw (Colors    (Airspaces (I).Kind),
                                              Thickness (Airspaces (I).Kind));
                  end if;

               end loop;

            end if;

         end loop;

      end if;

      Glex.Get_Transform.Load_Unit;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Count_Right_Crossing (X, Y    : Float;
                                   A, B    : access Node_Record;
                                   Add     : in out Boolean;
                                   Counter : in out Natural) is
   begin

      -- Check if the segment is fully at the left of the point
      ----------------------------------------------------------
      if A.X < X and then B.X < X then

         return;

      end if;

      -- Segment is at the right, check if the ray crosses it
      ----------------------------------------------------------
      declare
         Y_A, Y_B : Float;
      begin

         Y_A := A.Y - Y;
         Y_B := B.Y - Y;

         if
           (Y_A >= 0.0 and Y_B <  0.0) or else
           (Y_A <  0.0 and Y_B >= 0.0)
         then

            if A.X > X and then B.X > X then

               if Add then
                  Counter := Counter + 1;
                  Add := False;
               else
                  Counter := Counter - 1;
                  Add := True;
               end if;

            else

               declare
                  X_A, D_X, D_Y, X_C : Float;
               begin

                  X_A := A.X - X;
                  D_X := B.X - A.X;
                  D_Y := Y_B - Y_A;
                  X_C := X_A + D_X * abs (Y_A / D_Y);

                  if X_C > 0.0 then -- (right crossings only)

                     if Add then
                        Counter := Counter + 1;
                        Add := False;
                     else
                        Counter := Counter - 1;
                        Add := True;
                     end if;

                  end if;

               end;

            end if;

         end if;

      end;

   end Count_Right_Crossing;
   pragma Inline (Count_Right_Crossing);
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Contains (This : Airspace_Record; Position : Position_Record) return Boolean is
   begin

      -- Check if the point is within the airspace bounds
      --------------------------------------------------------------------------
      if
        This.Loaded and then
        This.North_East.Lon > Position.Lon and then
        This.North_East.Lat > Position.Lat and then
        This.South_West.Lon < Position.Lon and then
        This.South_West.Lat < Position.Lat
      then

         declare
            X       : Float := Float (Position.Lon - This.Center.Lon);
            Y       : Float := Float (Position.Lat - This.Center.Lat);
            A, B    : access Node_Record;
            Add     : Boolean := True;
            Counter : Natural := 0;
            F       : Natural;
            First   : Boolean := True;
         begin

            for P in This.First..This.Last loop

               if
                 Parts (P).North_East.Lat >  Position.Lat and then
                 Parts (P).South_West.Lat <  Position.Lat and then
                 Parts (P).North_East.Lon >= Position.Lon --> at least a portion at the right
               then

                  -- Include part in the horizontal ray scan
                  -- TODO: handle parts that discontinuate at the corners of the
                  -- map coverage area.
                  --------------------------------------------------------------

                  if First then
                     A := Nodes_Buffer (Parts (P).First)'Access;
                     F := Parts (P).First + 1;
                     First := False;
                  else
                     F := Parts (P).First;
                  end if;

                  for N in F .. Parts (P).Last loop

                     B := Nodes_Buffer (N)'Access;

                     Count_Right_Crossing (X, Y, A, B, Add, Counter);

                     A := B;

                  end loop;

               end if;

            end loop;

            return Counter /= 0;

         end;

      else
         return False;

      end if;


   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function In_Frame (This     : Airspace_Record;
                      Position : Position_Record;
                      Offset   : Float := 1.0) return Boolean is

      Lat_Offset : constant Long_Float := Long_Float (0.00833333 * Offset);
      Lon_Offset : constant Long_Float := Lat_Offset / Long_Float (Shrink);

   begin

      return
        This.North_East.Lon + Lon_Offset > Position.Lon and then
        This.North_East.Lat + Lat_Offset > Position.Lat and then
        This.South_West.Lon - Lon_Offset < Position.Lon and then
        This.South_West.Lat - Lat_Offset < Position.Lat;

   end In_Frame;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Clear_Focus is
   begin

      Focused_Airspace := 0;

   end Clear_Focus;
   -----------------------------------------------------------------------------


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Notify_Active_Keys   : constant array (Notify_Kinds) of Character :=
     (Notify_In     => '0',
      Notify_Out    => '1',
      Notify_In_Out => '2',
      Notify_None   => '3');

   Notify_Inactive_Keys : constant array (Notify_Kinds) of Character :=
     (Notify_In     => '4',
      Notify_Out    => '5',
      Notify_In_Out => '6',
      Notify_None   => '7');
   --===========================================================================
   --
   --===========================================================================
   procedure Save_Configuration is

      Status : String (1..Number_Of_Airspaces);

   begin

      Utility.Log.Put_Message ("saving airspace status");

      for I in 1..Number_Of_Airspaces loop

         if Airspaces (I).Active then

            Status (I) := Notify_Active_Keys   (Airspaces (I).Notify);

         else
            Status (I) := Notify_Inactive_Keys (Airspaces (I).Notify);

         end if;

      end loop;

      Utility.Storage.Set_Item ("SECTORS", Status);

   end Save_Configuration;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Load_Configuration is

      Status : String  := Utility.Storage.Get_Item ("SECTORS");

   begin

      if
        Status'Length > 0 and then
        Status'First = 1  and then
        Status'Last  = Number_Of_Airspaces
      then

         for I in 1..Number_Of_Airspaces loop

            if Status (I) in '0'..'3' then

               Airspaces (I).Active := True;

               case Status (I) is
                  when '0' => Airspaces (I).Notify := Notify_In;
                  when '1' => Airspaces (I).Notify := Notify_Out;
                  when '2' => Airspaces (I).Notify := Notify_In_Out;
                  when '3' => Airspaces (I).Notify := Notify_None;
                  when others => null;
               end case;

            elsif Status (I) in '4'..'7' then

               Airspaces (I).Active := False;

               case Status (I) is
                  when '4' => Airspaces (I).Notify := Notify_In;
                  when '5' => Airspaces (I).Notify := Notify_Out;
                  when '6' => Airspaces (I).Notify := Notify_In_Out;
                  when '7' => Airspaces (I).Notify := Notify_None;
                  when others => null;
               end case;

            end if;

         end loop;

         Utility.Log.Put_Message ("airpace status loaded");

      else
         Utility.Log.Put_Message ("airpace status not loaded");

      end if;

   end Load_Configuration;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Save_Master_Alerts_Switch is
   begin

      if Allow_Notifications then
         Utility.Storage.Set_Item ("ALERTS", "Y");
      else
         Utility.Storage.Set_Item ("ALERTS", "N");
      end if;

   end Save_Master_Alerts_Switch;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Save_Configuration_If_Changed is
   begin

      if Status_Changed then
         Status_Changed := False;
         Save_Configuration;
      end if;

   end Save_Configuration_If_Changed;
   -----------------------------------------------------------------------------

end Maps.Airspaces;
--------------------------------------------------------------------------------
