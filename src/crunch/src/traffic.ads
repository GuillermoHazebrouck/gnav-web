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
with Utility.Maps;
use  Utility.Maps;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides an UDP listener to parse OGN tracks
--//////////////////////////////////////////////////////////////////////////////
package Traffic is
   
   Reference   : Position_Record := (51.0, 4.0);
   
   Limits      : Position_Record := ( 2.0, 4.0);
   
   --===========================================================================
   -- Reads the aircraft data from the data file 'data/aircraft.dat'
   --===========================================================================
   procedure Start_Tracker;
   
end Traffic;
--------------------------------------------------------------------------------
