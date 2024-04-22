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
with Flight;
with Flight.Aircraft;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Maps;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Check is
        
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.016,
                                             Height    => 0.050,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   Btn_Registration : Button_Record;
   
   Btn_Location     : Button_Record;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
      
      Allocation : Allocation_Record;
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.10;
      W : constant Dimension_Float := 0.15;
      
   begin
      
      Allocation.X := 0.16 + M;

      Allocation.Y := 1.0 - (H + M);

      Allocation.W := 0.16;

      Allocation.H := H;

      Btn_Registration.Set_Allocation (Allocation);

      Btn_Registration.Set_Style (Button_Alive);

      Btn_Registration.Set_Font_Size (0.4, 0.35, 0.6);
  
      --
      
      Allocation.X := Allocation.X + Allocation.W + M;

      Allocation.W := 1.0 - (Allocation.X + M);

      Allocation.H := H;
      
      Btn_Location.Set_Allocation (Allocation);
      
      Btn_Location.Set_Style (Button_Disabled);

      Btn_Location.Set_Font_Size (0.4, 0.35, 0.6);
  
      -- Subpanels
      --------------------------------------------------------------------------
      
      Display.Panels.Baro.Initialize  (0.010, 0.320);
      
      Display.Panels.Radio.Initialize (0.340, 0.34, 0.650, 0.52);
      
      Display.Panels.Metar.Initialize (0.340, 0.01, 0.650, 0.30);
            
   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Glex.Fonts;
      use Utility.Strings;
      
   begin
     
      -- Aircraft registration
      
      Btn_Registration.Set_Label (Trim (Flight.Aircraft.This_Aircraft.Registration));
      
      Btn_Registration.Draw;

      -- Current position registration
      
      if Flight.Data.Is_Recent (Flight.Field_Position, 4.0) then
         
         Btn_Location.Set_Label (Maps.Image (Flight.Data.Position));
         
         Btn_Location.Set_Style (Button_Alive);

      elsif Flight.Data.Is_Valid (Flight.Field_Position) then
         
         Btn_Location.Set_Label (Maps.Image (Flight.Data.Position));
         
         Btn_Location.Set_Style (Button_Disabled);
         
      else
         
         Btn_Location.Set_Label ("LOCATION UNKNOWN");
         
         Btn_Location.Set_Style (Button_Disabled);

         Btn_Location.Set_Label_Color (Glex.Colors.Line_Red);

      end if;
      
      Btn_Location.Draw;
      
      -- Panels
            
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
      
      Display.Panels.Baro.Screen_Pressed (X, Y);
      
      Display.Panels.Radio.Screen_Pressed (X, Y);
      
      Display.Panels.Metar.Screen_Pressed (X, Y);
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      Display.Panels.Baro.Key_Changed (Key);
      
      Display.Panels.Radio.Key_Changed (Key);
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.Check;
--------------------------------------------------------------------------------
