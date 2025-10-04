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
with Interfaces;

--//////////////////////////////////////////////////////////////////////////////
-- This unit generates a simulated flight using the current turn rate as imput 
-- variable and the a simulated atmosphere with dynamic thermals
--//////////////////////////////////////////////////////////////////////////////
package Flight.Simulation is
            
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The selected turn rate
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Selected_Turn_Rate : Float := 0.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The selected airspeed (m/s)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Selected_Airspeed : Float := 30.0;
   
   --===========================================================================
   -- Simulates the next trajectory point
   --===========================================================================
   procedure Next_Simulation_Step (Millis : Interfaces.IEEE_Float_64);
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Clouds (View : Map_View_Record);
   
private

end Flight.Simulation;
--------------------------------------------------------------------------------
