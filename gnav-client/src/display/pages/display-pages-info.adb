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
with Display.Panels.Baro;
with Display.Panels.Radio;
with Display.Panels.Metar;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Info is
        
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      Display.Panels.Baro.Initialize  (0.01, 0.28);
      
      Display.Panels.Radio.Initialize (0.30, 0.01, 0.37, 0.850);
      
      Display.Panels.Metar.Initialize (0.68, 0.01, 0.31, 0.850);
            
   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
   begin
     
      Display.Panels.Baro.Draw;
      
      Display.Panels.Radio.Draw;
      
      Display.Panels.Metar.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
    
      Display.Panels.Baro.Screen_Pressed  (X, Y);
      
      Display.Panels.Radio.Screen_Pressed (X, Y);
      
      Display.Panels.Metar.Screen_Pressed (X, Y);
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean) is
   begin
      
      Display.Panels.Radio.Screen_Move (X, Y, Dx, Dy, First);
      
      Display.Panels.Baro.Screen_Move (X, Y, Dx, Dy, First);
      
   end Screen_Move;
   -----------------------------------------------------------------------------          
     
     
end Display.Pages.Info;
--------------------------------------------------------------------------------
