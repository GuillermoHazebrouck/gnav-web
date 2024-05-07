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
package Layers.Airspaces is

   --===========================================================================
   -- Reads a text file containing airspace sectors.
   --===========================================================================
   procedure Generate_Airspaces;

   --===========================================================================
   -- Generates the native G-NAV file
   --===========================================================================
   procedure Compile_Data;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Airspace_Kinds is (Airspace_Ctr,
                           Airspace_Tma,
                           Airspace_Cta,
                           Airspace_Tsa,
                           Airspace_Tmz,
                           Airspace_Rmz);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of layers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Airspaces : Part_List.Stack;

end Layers.Airspaces;
--------------------------------------------------------------------------------
