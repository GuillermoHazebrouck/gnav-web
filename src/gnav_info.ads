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
with Utility.Calendar;
use  Utility.Calendar;
with Maps;
use  Maps;

--******************************************************************************
--
--******************************************************************************
package Gnav_Info is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- G-NAV system data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Core_Version    : String (1..4)  := "B001";
   Service_Version : String (1..4)  := (others => ' ');
   Service_Name    : String (1..12) := (others => ' ');
   Startup_Time    : Times := No_Time;
   Home_Position   : Position_Record := No_Position_Record;
   Home_Name       : String (1..4)  := "HOME";
   Utc_Offst       : Lapses  := No_Lapse;
   Request_Metar   : Boolean := True;
   Request_Traffic : Boolean := False;

end Gnav_Info;
--------------------------------------------------------------------------------
