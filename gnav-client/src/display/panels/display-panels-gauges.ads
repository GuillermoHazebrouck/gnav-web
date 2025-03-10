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
with Utility.Calendar;
use  Utility.Calendar;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Display.Panels.Gauges is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The shift to the UTC time
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Utc_Shift : Lapses := Lapse_of (3600.0);

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   --
   --===========================================================================
   procedure Draw;

   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float);

   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys);

   --===========================================================================
   -- Displays a moving altimeter bar at the given position
   --===========================================================================
   procedure Draw_Altimeter (X, Y, H : Float);

   --===========================================================================
   -- Indicates if the altitude panel is toggled
   --===========================================================================
   function Altitude_Pressed (X, Y : Float) return Boolean;

end Display.Panels.Gauges;
--------------------------------------------------------------------------------
