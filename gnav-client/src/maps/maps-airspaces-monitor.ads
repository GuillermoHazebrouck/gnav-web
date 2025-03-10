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
-- This packages monitors the given position of the track, and generates
-- notifications when entering or leaving airspaces with a configured
-- type of notification.
--//////////////////////////////////////////////////////////////////////////////
package Maps.Airspaces.Monitor is

   --===========================================================================
   -- Draws the active notifications
   --===========================================================================
   procedure Draw_Notifications;

   --===========================================================================
   -- Process the position to verify if we came inside/outside a sector
   --===========================================================================
   procedure Process_Location (Location : Position_Record; Altitude : Float);

end Maps.Airspaces.Monitor;
--------------------------------------------------------------------------------
