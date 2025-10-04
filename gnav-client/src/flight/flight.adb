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
with Utility.Log;
with Utility.Notifications;
with Maps.Terrain;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Lapses is
   begin

      if This.Ages (Field) = No_Time then

         return Invalid_Lapse;

      else

         return Cached_Time - This.Ages (Field);

      end if;

   end Age;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Relative_Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Lapses is
   begin

      if This.Ages (Field) = No_Time then

         return Invalid_Lapse;

      else

         return This.Timestamp - This.Ages (Field);

      end if;

   end Relative_Age;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Update (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean is
   begin

      return This.Origin (Field) /= Update_None;

   end Is_Update;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Recent (This : Flight_Data_Record; Field : Data_Field_Kind; Lapse : Float := 2.0) return Boolean is
   begin

      return This.Ages (Field) /= No_Time and then Cached_Time - This.Ages (Field) < Lapse_Of (Lapse);

   end Is_Recent;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Valid (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean is
   begin

      return This.Ages (Field) /= No_Time;

   end Is_Valid;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns an IGC image of the data
   --===========================================================================
   function Get_Igc_Image (This : Flight_Data_Record) return String is

      use Utility.Calendar;
      use Utility.Strings;

      C : Day_Lapse := Get_Clock (This.Timestamp);

      Timestamp : String := Hour_Image (C) & Minute_Image (C) & Second_Image (C);
      Position  : String := Maps.Compact_Image (This.Position);
      Altitude  : String := "00000";
      Course    : String := "000";
      Speed     : String := "000";

   begin

      Override (Altitude, Float_Image (This.Altitude,    0), '0', True);
      Override (Course,   Float_Image (This.Speed * 3.6, 0), '0', True);
      Override (Speed,    Float_Image (This.Course,      0), '0', True);

      return 'B' & Timestamp & Position & 'A' & Altitude & Altitude & Course & Speed;

   end Get_Igc_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Get_Previous_Index (Index : in out History_Range) is
   begin

      if Index > History_Range'First then

         Index := Index - 1;

      else

         Index := History_Range'Last;

      end if;

   end Get_Previous_Index;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the next index in the circular history buffer
   --===========================================================================
   procedure Get_Next_Index (Index : in out History_Range) is
   begin

      if Index < History_Range'Last then

         Index := Index + 1;

      else

         Index := History_Range'First;

      end if;

   end Get_Next_Index;
   -----------------------------------------------------------------------------



   Below_100m : Boolean := False;
   --===========================================================================
   -- Compute elevation from position and altitude and generate notification
   -- when above 100m.
   -- NOTE: we do not report negative values, which are clearly wrong.
   --===========================================================================
   procedure Compute_Elevation is
   begin

      if
        Data.Is_Update (Field_Altitude) and then
        Data.Is_Recent (Field_Position)
      then

         declare
            Terrain_Elevation : Float := Maps.Terrain.Get_Elevation (Data.Position);
         begin

            if Data.Altitude > Terrain_Elevation then

               Data.Elevation := Data.Altitude - Terrain_Elevation;

               Data.Ages   (Field_Elevation) := Data.Timestamp;

               Data.Origin (Field_Elevation) := Update_Internal;

               -- 100m notification
               -------------------------------------------------
               if Below_100m and Data.Elevation > 100.0 then

                  Utility.Notifications.Add_Notification (Utility.Notifications.Notify_100m);

                  Below_100m := False;

               elsif Data.Elevation < 90.0 then

                  Below_100m := True;

               end if;

            end if;

         end;

      end if;


   end Compute_Elevation;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Course is
   begin

      null;

   end Compute_Course;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Level is

      H        : History_Range := Current;
      Span     : Float := 0.0;
      Time     : Float := 0.0;
      Step     : Float := 0.0;
      Altitude : Float := Data.Altitude;
      Level    : Float := 0.0;

   begin

      if Data.Is_Update (Field_Altitude) then

         for K in History_Range loop

            if History (H).Is_Update (Field_Altitude) then

               Time := Seconds (Data.Ages (Field_Altitude) - History (H).Ages (Field_Altitude));

               exit when Time > Level_Max_Lapse;

               Step := Time - Span;
               Span := Time;

               Level    := Level + 0.5 * (Altitude + History (H).Altitude) * Step;
               Altitude := History (H).Altitude;

            end if;

            Get_Previous_Index (H);

         end loop;

         if Span > 0.0 then

            Data.Level := Level / Span;

            Data.Ages   (Field_Level) := Data.Timestamp - Lapse_Of (0.5 * Span);

            Data.Origin (Field_Level) := Update_Internal;

         end if;

      end if;

   end Compute_Level;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Turn_Rate is

      H       : History_Range := Current;
      Course  : Float := Data.Course;
      Step    : Float := 0.0;
      Time    : Float := 0.0;
      Span    : Float := 0.0;
      Turn    : Float := 0.0;

   begin

      -- The turn is computed as the mean course variation during the last 5s
      -- NOTE: this is done only if the field is not already provided
      --------------------------------------------------------------------------

      if Data.Is_Update (Field_Course) and not Data.Is_Update (Field_Turn) then

         for K in History_Range loop

            if History (H).Is_Update (Field_Course) then

               Time := Seconds (Data.Ages (Field_Course) - History (H).Ages (Field_Course));

               exit when Time > Turn_Rate_Max_Lapse;

               Span := Time;

               if Course >= History (H).Course then
                  Step := Course - History (H).Course;
                  if Step > 180.0 then
                     Step := Step - 360.0;
                  end if;
               else
                  Step := History (H).Course - Course;
                  if Step > 180.0 then
                     Step := Step - 360.0;
                  end if;
                  Step := -Step;
               end if;

               Turn   := Turn + Step;

               Course := History (H).Course;

            end if;

            Get_Previous_Index (H);

         end loop;

         if Span > 0.0 then

            Turn := Turn / Span;

            if Turn in -60.0..60.0 then

               Data.Turn := Turn;

               Data.Ages   (Field_Turn) := Data.Timestamp - Lapse_Of (0.5 * Span);

               Data.Origin (Field_Turn) := Update_Internal;

             --Utility.Log.Put_Message ("turn " & Float'Image (Turn));

            end if;

         end if;

      end if;

   end Compute_Turn_Rate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Flight_Mode is

      H      : History_Range := Current;
      Course : Float := Data.Course;
      Step   : Float := 0.0;
      Turn   : Float := 0.0;
      Sense_1,
      Sense_2 : Boolean := False;

   begin

      if Data.Is_Update (Field_Course) and Data.Is_Update (Field_Turn) then

         -- Check if we are going straight
         -------------------------------------------------
         if Data.Mode = Mode_Straight and then abs Data.Turn > Max_Turn_Straight then

            Data.Mode := Mode_Manouver;

         elsif abs Data.Turn < Min_Turn_Straight then

            Data.Mode := Mode_Straight;

         end if;

         -- Check if we are circling
         -------------------------------------------------
         if Data.Mode = Mode_Manouver and then History (H).Is_Recent (Field_Turn, Turn_Rate_Max_Lapse) then

            Sense_1 := Data.Turn        > 0.0;
            Sense_2 := History (H).Turn > 0.0;

            if Sense_1 = Sense_2 and then History (H).Mode = Mode_Circling then

               -- Once detected, check only that we keep turning in the same direction

               Data.Mode := Mode_Circling;

            else

               for K in History_Range loop

                  Sense_2 := History (H).Turn > 0.0;

                  exit when History (H).Mode = Mode_Straight or else abs Turn > 360.0 or else Sense_1 /= Sense_2;

                  if History (H).Is_Update (Field_Course) then

                     if Course >= History (H).Course then
                        Step := Course - History (H).Course;
                        if Step > 180.0 then
                           Step := Step - 360.0;
                        end if;
                     else
                        Step := History (H).Course - Course;
                        if Step > 180.0 then
                           Step := Step - 360.0;
                        end if;
                        Step := -Step;
                     end if;

                     Turn    := Turn + Step;
                     Course  := History (H).Course;
                     Sense_1 := Sense_2;

                  end if;

                  Get_Previous_Index (H);

               end loop;

             --Utility.Log.Put_Message ("turn " & Float'Image (Turn));

               if abs Turn > 360.0 then

                  Data.Mode := Mode_Circling;

               end if;

            end if;

         end if;

       --Utility.Log.Put_Message ("mode " & Flight_Mode_Kinds'Image (Data.Mode));

      end if;

   end Compute_Flight_Mode;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- The vario is computed as altitude variation registered in the last 5s
   -- NOTE: this is done only if the field is not already provided
   --===========================================================================
   procedure Compute_Vario is

      H     : History_Range := Current;
      Time  : Float := 0.0;
      Span  : Float := 0.0;
      Step  : Float := 0.0;
      Vario : Float := 0.0;

   begin

      if Data.Is_Update (Field_Level) then

         for K in History_Range loop

            if History (H).Is_Update (Field_Level) then

               Time := Seconds (Data.Ages (Field_Level) - History (H).Ages (Field_Level));

               exit when Time > Vario_Max_Lapse;

               Span := Time;
               Step := Data.Level - History (H).Level;

            end if;

            Get_Previous_Index (H);

         end loop;

         if Span > 0.0 then

            Vario := Step / Span;

            -- NOTE: outside a given range we have either a big error or a
            -- transit, which in either case we rather do not register
            --------------------------------------------------------------------
            if Vario in -8.0..8.0 then

               Data.Vario := Vario;

               Data.Ages   (Field_Vario) := Data.Timestamp - Lapse_Of (0.5 * Span);

               Data.Origin (Field_Vario) := Update_Internal;

            end if;

         end if;

      end if;

   end Compute_Vario;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Complete_Data is
   begin

      -- NOTE: the order is relevant
      ------------------------------

      Compute_Elevation;

      Compute_Level;

      Compute_Vario;

      Compute_Turn_Rate;

      Compute_Flight_Mode;

      -- TODO:
      -- Flight.Wind.Compute_Wind;

   end Complete_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Cache_Data is
   begin

      Previous := History (Current);

      Get_Next_Index (Current);

      History (Current) := Data;

      On_Data_Cached.Trigger;

   end Cache_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Clear_History is
   begin

      Current  := History_Range'First;

      History  := (others => No_Flight_Data);

      Data     := No_Flight_Data;

      Previous := No_Flight_Data;

      On_Data_Cleared.Trigger;

   end Clear_History;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Previous return Flight_Data_Record is
   begin

      return Previous;

   end Get_Previous;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Variation (Field : Data_Field_Kind) return Float is
      I0, I1, I2 : History_Range;
      D1, D2 : Float := 0.0;
   begin

      I0 := Current;
      I1 := I0;
      Get_Previous_Index (I1);
      I2 := I1;
      Get_Previous_Index (I2);

      case Field is

         when Field_Position =>

            if
              History (I0).Is_Valid (Field) and
              History (I1).Is_Valid (Field) and
              History (I2).Is_Valid (Field)
            then
               D1 := Float (Distance (History (I0).Position, History (I1).Position));
               D2 := Float (Distance (History (I1).Position, History (I2).Position));
            end if;

         when others =>

            return 0.0;

      end case;

      if D1 > 0.0 then

         return abs (D1 - D2) / D1;

      else
         return 1.0;

      end if;

   end Variation;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Step (Field : Data_Field_Kind) return Float is
      I0, I1 : History_Range;
   begin

      I0 := Current;
      I1 := Current;
      Get_Previous_Index (I1);

      case Field is

         when Field_Position =>

            if
              History (I0).Is_Update (Field) and
              History (I1).Is_Update (Field)
            then
               return Float (Distance (History (I0).Position, History (I1).Position));
            end if;

         when others =>

            return 0.0;

      end case;

      return 0.0;

   end Step;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Local_Time return Times is
   begin

      return Cached_Time + Time_Offset + Date_Offset + Utc_Offset;

   end Get_Local_Time;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Utc_Time return Times is
   begin

      return Cached_Time + Time_Offset + Date_Offset;

   end Get_Utc_Time;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Time_Synchronized return Boolean is
   begin

      return (Cached_Time - Last_Time_Sync) < One_Minute;

   end Time_Synchronized;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Date_Synchronized return Boolean is
   begin

      return (Cached_Time - Last_Time_Sync) < One_Minute;

   end Date_Synchronized;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Time_Zone (Zone : Integer) is
   begin

      if Zone in -12..12 then

         Utc_Offset := Lapse_Of (Float (3600 * Zone));

      end if;

   end Set_Time_Zone;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Time_Zone return Integer is
   begin

      return Integer (Hours (Utc_Offset));

   end Get_Time_Zone;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Resync_Data_And_Time is
   begin

      Last_Time_Sync := Cached_Time - One_Hour;

      Last_Date_Sync := Cached_Time - One_Hour;

   end Resync_Data_And_Time;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_History (H : History_Range) return Flight_Data_Record is
      O : Natural := Natural (Current) + Natural (H);
      L : Natural := Natural (History_Range'Last);
   begin

      if O > L then
         O := O - L;
      end if;

      return History (History_Range (O));

   end Get_History;
   -----------------------------------------------------------------------------


end Flight;
--------------------------------------------------------------------------------
