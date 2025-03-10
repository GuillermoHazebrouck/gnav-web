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
-- Gnav
with Utility.Calendar;
use  Utility.Calendar;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Flight.Register is
   
   --===========================================================================
   -- Completes the flight register based on the current stage
   --===========================================================================
   procedure Update;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Register_Range is new Positive range 1..20;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Registers the flight parameters during a continuous flight mode
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Register_Record is record
      
      Start  : Times; -- Most recent input
      
      Stop   : Times; -- Oldest input
      
      Sink   : Float; -- Average sink
      
      Lift   : Float; -- Average lift
      
      Vario  : Float; -- Average vertical speed
      
      Speed  : Float; -- Average ground speed
      
      Span   : Float; -- Total time
      
      Course : Float; -- Last measured course
      
      Ratio  : Float; -- Average sink ratio (Sink / Ground_Speed)
      
      Mode   : Flight_Mode_Kinds;
      
      Active : Boolean;
      
   end record;
   
   --===========================================================================
   -- Indicates if the given sink underestimates the meassured gliding slope
   --===========================================================================
   function Sink_Warning_Active return Boolean;
   
   --===========================================================================
   -- Gets the given register (organized in chronological order)
   --===========================================================================
   function Get_Register (Index : Register_Range) return not null access Register_Record;
   
private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Register_Record : constant Register_Record := (Start  => No_Time,
                                                     Stop   => No_Time,
                                                     Sink   => 0.0,
                                                     Lift   => 0.0,
                                                     Span   => 0.0,
                                                     Speed  => 0.0,
                                                     Vario  => 0.0,
                                                     Course => 0.0,
                                                     Ratio  => 0.0,
                                                     Mode   => Mode_Straight,
                                                     Active => False);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Chronological register of the flight divided in different modes.
   -- Each part records no less than 10s of a sustained mode.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Register : array (Register_Range) of aliased Register_Record := (others => No_Register_Record);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Current_Register : Register_Range := Register_Range'First;
 
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Meassured_Gliding_Ratio : Float := 0.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Meassured_Gliding_Course : Float := 0.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the provided sink is underestimating the measured slope
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Sink_Warning : Boolean := False;
   
end Flight.Register;
--------------------------------------------------------------------------------
