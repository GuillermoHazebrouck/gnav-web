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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Calendar is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last register of time
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Time : Times := No_Time;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Cached_Time return Times is
   begin

      return Last_Time;

   end Cached_Time;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Cache_Time (Time : Times) is
   begin

      Last_Time := Time;

   end Cache_Time;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Zero_Hour (Time : Times) return Times is
   begin

      return (Time.Days, 0.0);

   end Zero_Hour;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Is_Leap
   --===========================================================================
   function Is_Leap (Year : Years) return Boolean is
   begin
      --  Leap centennial years

      if Year mod 400 = 0 then
         return True;

      --  Non-leap centennial years

      elsif Year mod 100 = 0 then
         return False;

      --  Regular years

      else
         return Year mod 4 = 0;
      end if;
   end Is_Leap;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Day (Date : Times) return Days is
      D : Days;
      Y : Years;
      M : Months;
      S : Day_Lapse;
   begin
      Split (Date, Y, M, D, S);
      return D;
   end Day;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Month (Date : Times) return Months is
      Y : Years;
      M : Months;
      D : Days;
      S : Day_Lapse;
   begin
      Split (Date, Y, M, D, S);
      return M;
   end Month;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Year (Date : Times) return Years is
      Y : Years;
      M : Months;
      D : Days;
      S : Day_Lapse;
   begin
      Split (Date, Y, M, D, S);
      return Y;
   end Year;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Seconds (Date : Times) return Day_Lapse is
      Y : Years;
      M : Months;
      D : Days;
      S : Day_Lapse;
   begin
      Split (Date, Y, M, D, S);
      return S;
   end Seconds;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Clock (Date : Times) return Day_Lapse is
   begin

      return Date.Seconds;

   end Get_Clock;
   -----------------------------------------------------------------------------




   Norm_Year : constant array (Months) of Days := (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   Leap_Year : constant array (Months) of Days := (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Month_Days (Y : Years; M : Months) return Days is
   begin
      if Is_Leap (Y) then
         return Leap_Year (M);
      else
         return Norm_Year (M);
      end if;
   end Month_Days;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Split (Date    : Times;
                    Year    : out Years;
                    Month   : out Months;
                    Day     : out Days;
                    Seconds : out Day_Lapse)
   is
      Count : Integer := 0;
      Plus  : Integer := 0;
   begin

      -- TODO: change algorithm to include larger range of years

      if Date.Days >= 0 then

         -- Day 0 = 01/01/2020
         for Y in 2020 .. Years'Last loop
            -- TODO: do a check with the number of days in the year
            for M in Months'Range loop
               Plus := Month_Days (Y, M);
               if Count + Plus > Date.Days then
                  Year    := Y;
                  Month   := M;
                  Day     := Date.Days - Count + 1;
                  Seconds := Date.Seconds;
                  return;
               end if;
               Count := Count + Plus;
            end loop;
         end loop;

      else

         -- Day -1 = 31/12/2019
         for Y in reverse Years'First .. 2019 loop
            -- TODO: do a check with the number of days in the year
            for M in reverse Months'Range loop
               Plus := Month_Days (Y, M);
               if Count - Plus <= Date.Days then
                  Year    := Y;
                  Month   := M;
                  Day     := Date.Days - Count + Plus + 1;
                  Seconds := Date.Seconds;
                  return;
               end if;
               Count := Count - Plus;
            end loop;

         end loop;

      end if;

      Year    := Years'First;
      Month   := Months'First;
      Day     := Days'First;
      Seconds := Day_Lapse'First;

      return;

   end Split;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Time_Of (Year    : Years;
                     Month   : Months;
                     Day     : Days;
                     Seconds : Day_Lapse := 0.0) return Times
   is

      Result        : Times            := (0, 0.0);
      One_Norm_Year : constant Integer := 365;
      One_Leap_Year : constant Integer := 366;

   begin

      -- Check that the given number of days for the months is correct
      --------------------------------------------------------------------------
      if Is_Leap (Year) then
         if Day > Leap_Year (Month) then
            return No_Time;
         end if;
      else
         if Day > Norm_Year (Month) then
            return No_Time;
         end if;
      end if;

      if Year >= 2020 then

         -- Count forwards form epoc day
         -----------------------------------------------------------------------
         for Y in 2020 .. Years'Last loop
            if Y = Year then
               for M in Months'First..Month loop
                  if M = Month then
                     Result.Days    := Result.Days + Day - 1;
                     Result.Seconds := Seconds;
                     return Result;
                  else
                     Result.Days    := Result.Days + Month_Days (Y, M);
                  end if;
               end loop;
            else
               if Is_Leap (Y) then
                  Result.Days := Result.Days + One_Leap_Year;
               else
                  Result.Days := Result.Days + One_Norm_Year;
               end if;
            end if;
         end loop;

      else

         -- Count backwards form epoc day
         -----------------------------------------------------------------------
         for Y in reverse Year .. 2019 loop
            if Y = Year then
               for M in reverse Month..Months'Last loop
                  if M = Month then
                     Result.Days    := Result.Days - (Month_Days (Y, M) - Day) - 1;
                     Result.Seconds := Seconds;
                     return Result;
                  else
                     Result.Days    := Result.Days - Month_Days  (Y, M);
                  end if;
               end loop;
            else
               if Is_Leap (Y) then
                  Result.Days := Result.Days - One_Leap_Year;
               else
                  Result.Days := Result.Days - One_Norm_Year;
               end if;
            end if;
         end loop;

      end if;

      return Result;

   end Time_Of;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- A lapse in seconds
   --===========================================================================
   function Lapse_Of (Seconds : Float) return Lapses is

      Result : Lapses;

   begin

      if Seconds in Day_Lapse'Range then
         Result.Days    := 0;
         Result.Seconds := Day_Lapse (Seconds);
      else
         Result.Days    := Integer ((Float'Floor (Seconds / Day_Lapse'Last))); -- TODO: fix problem here
         Result.Seconds := Seconds - Float (Result.Days)  * Day_Lapse'Last;
      end if;

      return Result;

   end Lapse_Of;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- A lapse in seconds
   --===========================================================================
   function Long_Lapse_Of (Seconds : Long_Float) return Lapses is

      Day    : constant Long_Float := 86_400.0;
      Result : Lapses;

   begin

      Result.Days    := Integer ((Long_Float'Floor (Seconds / Day)));
      Result.Seconds := Float (Seconds - Long_Float (Result.Days) * Day);

      return Result;

   end Long_Lapse_Of;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Seconds (Lapse : Lapses) return Float is
   begin

      return Day_Lapse'Last * Float (Lapse.Days) + Lapse.Seconds;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function "-" (Time_1, Time_2 : Times) return Lapses is

      Result : Lapses;

   begin

      if Time_1 = No_Time or else Time_2 = No_Time then
         return No_Lapse;
      end if;

      Result.Days := Time_1.Days - Time_2.Days;

      if Time_1.Seconds > Time_2.Seconds then
         Result.Seconds := Time_1.Seconds - Time_2.Seconds;
      else
         Result.Days    := -1;
         Result.Seconds := Day_Lapse'Last - (Time_2.Seconds - Time_1.Seconds);
      end if;

      return Result;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function "+" (Time : Times; Lapse : Lapses) return Times is

      Result  : Times;
      Seconds : Float;

   begin

      if Time = No_Time then
         return No_Time;
      end if;

      Result.Days := Time.Days + Lapse.Days;
      Seconds     := Time.Seconds + Lapse.Seconds;

      if Seconds >= Day_Lapse'Last then
         Result.Days    := Result.Days + 1;
         Result.Seconds := Seconds - Day_Lapse'Last;
      else
         Result.Seconds := Seconds;
      end if;

      return Result;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function "-" (Time : Times; Lapse : Lapses) return Times is

      Result : Times;

   begin

      if Time = No_Time then
         return No_Time;
      end if;

      Result.Days := Time.Days - Lapse.Days;

      if Time.Seconds > Lapse.Seconds then
         Result.Seconds := Time.Seconds - Lapse.Seconds;
      else
         Result.Days    := -1;
         Result.Seconds := Day_Lapse'Last - (Lapse.Seconds - Time.Seconds);
      end if;

      return Result;

   end;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function "+" (Lapse_1, Lapse_2 : Lapses) return Lapses is

      Result  : Lapses;
      Seconds : Float := Lapse_1.Seconds + Lapse_2.Seconds;

   begin

      Result.Days := Lapse_2.Days + Lapse_2.Days;

      if Seconds >= Day_Lapse'Last then
         Result.Days    := Result.Days + 1;
         Result.Seconds := Seconds - Day_Lapse'Last;
      else
         Result.Seconds := Seconds;
      end if;

      return Result;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function "-" (Lapse_1, Lapse_2 : Lapses) return Lapses is

      Result : Lapses;

   begin

      Result.Days := Lapse_1.Days - Lapse_2.Days;

      if Lapse_1.Seconds > Lapse_2.Seconds then
         Result.Seconds := Lapse_1.Seconds - Lapse_2.Seconds;
      else
         Result.Days    := -1;
         Result.Seconds := Day_Lapse'Last - (Lapse_2.Seconds - Lapse_1.Seconds);
      end if;

      return Result;

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function ">" (Lapse_1 : Lapses; Lapse_2 : Lapses) return Boolean is
   begin

      return Seconds (Lapse_1) > Seconds (Lapse_2);

   end;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function "<" (Lapse_1 : Lapses; Lapse_2 : Lapses) return Boolean is
   begin

      return Seconds (Lapse_1) < Seconds (Lapse_2);

   end;
   -----------------------------------------------------------------------------


end Utility.Calendar;
--------------------------------------------------------------------------------
