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

--//////////////////////////////////////////////////////////////////////////////
package Display.Main is

   --===========================================================================
   -- Initializes the package by loading all resources
   --===========================================================================
   procedure Initialize;
      
   --===========================================================================
   -- Draws the menu and the current page
   --===========================================================================
   procedure Draw;

   --===========================================================================
   -- Handles a screen (or mouse) press event
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float);

   --===========================================================================
   -- Handles a screen move event.
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean);
   
end Display.Main;
--------------------------------------------------------------------------------
