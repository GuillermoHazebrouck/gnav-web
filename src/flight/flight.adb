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

      return This.Origin (Field) /= Origin_None;

   end Is_Update;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Recent (This : Flight_Data_Record; Field : Data_Field_Kind; Lapse : Float := 2.0) return Boolean is
   begin

      return  This.Ages (Field) /= No_Time and then Cached_Time - This.Ages (Field) < Lapse_Of (Lapse);

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
   -- (See specification file)
   --===========================================================================
   procedure Cache_Timestamp (This : in out Flight_Data_Record; Field : Data_Field_Kind) is
   begin

      This.Ages (Field) := This.Timestamp;

   end Cache_Timestamp;
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

      Current := History_Range'First;

      History := (others => No_Flight_Data);

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
   procedure Compute_Course is
   begin

      null;

   end Compute_Course;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Turn_Rate is

      H      : History_Range := Current;
      Angle  : Float  := 0.0;
      Span   : Lapses := No_Lapse;
      V1, V2 : Vector2_Record;

   begin

      -- The turn is computed as the mean course variation during the last 5s
      --------------------------------------------------------------------------

      if Data.Is_Update (Field_Course) and not Data.Is_Update (Field_Turn) then

         V1.Set_From_Polar (Long_Float (Data.Course) / 180.0 * Math.Pi, 1.0);

         for K in History_Range loop

            Get_Previous_Index (H);

            if History (H).Is_Update (Field_Course) then

               exit when Cached_Time - History (H).Timestamp > Lapse_Of (5.0);

               V2.Set_From_Polar (Long_Float (History (H).Course) / 180.0 * Math.Pi, 1.0);

               Angle := Angle + Float (V1.Angle (V2) * 180.0 / Math.Pi);

               V1 := V2;

               Span := Data.Timestamp - History (H).Timestamp;

            end if;

         end loop;

         if Span > No_Lapse then

            Data.Turn := Angle / Seconds (Span);

            Data.Ages   (Field_Turn) := Cached_Time;

            Data.Origin (Field_Turn) := Origin_Internal;

         end if;

      end if;

   end Compute_Turn_Rate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Straight_Flight return Boolean is
   begin

      return Data.Is_Update (Field_Turn) and abs Data.Turn < 2.0;

   end Straight_Flight;
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
              History (I0).Is_Update (Field) and
              History (I1).Is_Update (Field) and
              History (I2).Is_Update (Field)
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

         Utc_Offset := One_Hour + Lapse_Of (Float (3600 * Zone));

      end if;

   end Set_Time_Zone;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Time_Zone return Integer is
   begin

      return Integer (Seconds (Utc_Offset) / 3600.0);

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

end Flight;
--------------------------------------------------------------------------------
