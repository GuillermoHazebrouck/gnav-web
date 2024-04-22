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
with Interfaces;

--******************************************************************************
--
--******************************************************************************
package Gnav is

   pragma Elaborate_Body;

   --===========================================================================
   -- Passes the GNSS data to G-NAV
   --===========================================================================
   procedure Gnav_Set_Gnss_Data (Lat : Interfaces.IEEE_Float_64;
                                 Lon : Interfaces.IEEE_Float_64;
                                 Alt : Interfaces.IEEE_Float_64;
                                 Spd : Interfaces.IEEE_Float_64;
                                 Crs : Interfaces.IEEE_Float_64)
     with Export      => True,
          Convention  => C,
          Link_Name   => "Gnav_Set_Gnss_Data";

   --===========================================================================
   -- Refreshes the G-NAV screen
   --===========================================================================
   procedure Gnav_Refresh_Screen
     with Export      => True,
          Convention  => C,
          Link_Name   => "Gnav_Refresh_Screen";

   --===========================================================================
   -- Passes a Click or Touch event to G-NAV
   --===========================================================================
   procedure Gnav_Touch_Screen (X : Interfaces.IEEE_Float_64;
                                Y : Interfaces.IEEE_Float_64)
     with Export      => True,
          Convention  => C,
          Link_Name   => "Gnav_Touch_Screen";

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Cache_Time (M : Interfaces.IEEE_Float_64)
     with Export      => True,
          Convention  => C,
          Link_Name   => "Gnav_Cache_Time";

   --===========================================================================
   -- Runs the timed events
   --===========================================================================
   procedure Gnav_Process_Timer
     with Export      => True,
          Convention  => C,
          Link_Name   => "Gnav_Process_Timer";

   --===========================================================================
   -- Updates the size of the screen
   --===========================================================================
   procedure Gnav_Update_Size (W : Interfaces.IEEE_Float_64;
                               H : Interfaces.IEEE_Float_64)
     with Export      => True,
          Convention  => C,
          Link_Name   => "Gnav_Update_Size";

end Gnav;
--------------------------------------------------------------------------------
