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
with Ada.Streams;
use  Ada.Streams;
with Utility.Maps;
use  Utility.Maps;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides OGN tracks in the traffic stack
--//////////////////////////////////////////////////////////////////////////////
package Traffic.Ogn is

   --===========================================================================
   -- Starts consuming OGN tracks within the configured region
   --===========================================================================
   procedure Start_Updating;

private
   
   --===========================================================================
   -- Consumes OGN tracks within the configured region until there is a problem
   -- or disconnection.
   --===========================================================================
   procedure Update_Tracks;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   task Maintain_Tracks is
      
      entry Start;
   
   end Maintain_Tracks;
   
end Traffic.Ogn;
--------------------------------------------------------------------------------
