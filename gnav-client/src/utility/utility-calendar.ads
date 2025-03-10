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
package Utility.Calendar is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents an amount of time (in seconds) within one day
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Day_Lapse is Float range 0.0..86_400.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Valid period ranges
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Years  is Natural range 1900 .. 2100;
   subtype Months is Natural range    1 ..   12;
   subtype Days   is Natural range    1 ..   31;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a given instant
   -- (referenced on 1/1/2020 00:00 UTC)
   -- TODO: replace by the record on the private part
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Times is private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Ephoc time: 01/01/2020 00:00-UTC
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Time : constant Times;

   --===========================================================================
   -- Return the
   --===========================================================================
   function Year    (Date : Times) return Years;
   function Month   (Date : Times) return Months;
   function Day     (Date : Times) return Days;
   function Seconds (Date : Times) return Day_Lapse;

   --===========================================================================
   -- Splits the date in usual calendar components
   --===========================================================================
   procedure Split (Date    : Times;
                    Year    : out Years;
                    Month   : out Months;
                    Day     : out Days;
                    Seconds : out Day_Lapse);

   --===========================================================================
   -- Returns the seconds component
   --===========================================================================
   function Get_Clock (Date : Times) return Day_Lapse;

   --===========================================================================
   -- Sets the clock for the day
   --===========================================================================
   procedure Set_Clock (Date : in out Times; Clock : Day_Lapse);

   --===========================================================================
   --
   --===========================================================================
   function Time_Of
     (Year    : Years;
      Month   : Months;
      Day     : Days;
      Seconds : Day_Lapse := 0.0) return Times;

   --===========================================================================
   -- Cache the current time
   --===========================================================================
   procedure Cache_Time (Time : Times);

   --===========================================================================
   -- Returns the zero hour of the given date
   --===========================================================================
   function Zero_Hour (Time : Times) return Times;

   --===========================================================================
   -- The cached time
   --===========================================================================
   function Cached_Time return Times;

   --===========================================================================
   -- Represents amounts of time (high precision)
   --===========================================================================
   type Lapses is private;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Constant lapses
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Lapse        : constant Lapses;
   One_Millisecond : constant Lapses;
   One_Second      : constant Lapses;
   One_Minute      : constant Lapses;
   One_Hour        : constant Lapses;
   Invalid_Lapse   : constant Lapses;

   --===========================================================================
   -- A lapse in seconds
   --===========================================================================
   function Lapse_Of (Seconds : Float) return Lapses;

   --===========================================================================
   -- A lapse in seconds
   --===========================================================================
   function Long_Lapse_Of (Seconds : Long_Float) return Lapses;

   --===========================================================================
   -- The total lapse in seconds (precision is lost for high lapses)
   --===========================================================================
   function Seconds (Lapse : Lapses) return Float;

   --===========================================================================
   -- Returns the amount of time between two moments
   --===========================================================================
   function "-" (Time_1 : Times; Time_2 : Times) return Lapses;

   --===========================================================================
   -- Returns the time after increaseing a lapse
   --===========================================================================
   function "+" (Time : Times; Lapse : Lapses) return Times;

   --===========================================================================
   -- Returns the time after increaseing a lapse
   --===========================================================================
   function "-" (Time : Times; Lapse : Lapses) return Times;

   --===========================================================================
   -- Returns the amount of time between two moments
   --===========================================================================
   function "+" (Lapse_1 : Lapses; Lapse_2 : Lapses) return Lapses;

   --===========================================================================
   -- Returns the amount of time between two moments
   --===========================================================================
   function "-" (Lapse_1 : Lapses; Lapse_2 : Lapses) return Lapses;

   --===========================================================================
   -- Indicates if the interval is grater
   --===========================================================================
   function ">" (Lapse_1 : Lapses; Lapse_2 : Lapses) return Boolean;

   --===========================================================================
   -- Indicates if the interval is smaller
   --===========================================================================
   function "<" (Lapse_1 : Lapses; Lapse_2 : Lapses) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   Time_Error : exception;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Times is record

      Days    : Integer;

      Seconds : Day_Lapse;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Arbitraty time out of calendar range
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Time : constant Times := (-100000, 0.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Lapses is record

      Days    : Integer;

      Seconds : Day_Lapse;

   end record;
   -----------------------------------------------------------------------------

   No_Lapse        : constant Lapses := (0, 0.0);
   One_Millisecond : constant Lapses := (0, 0.001);
   One_Second      : constant Lapses := (0, 1.0);
   Two_Second      : constant Lapses := (0, 2.0);
   One_Minute      : constant Lapses := (0, 60.0);
   One_Hour        : constant Lapses := (0, 3600.0);
   One_Day         : constant Lapses := (1, 0.0);
   Invalid_Lapse   : constant Lapses := (9_999_999,0.0);

   --===========================================================================
   --  Determine whether a given year is leap
   --===========================================================================
   function Is_Leap (Year : Years) return Boolean;

end Utility.Calendar;
