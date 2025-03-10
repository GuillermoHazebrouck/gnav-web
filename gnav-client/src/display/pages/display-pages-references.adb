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
with Display.Pages.Navigation;
with Display.Panels.Gauges;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Flight;
with Utility.Atmosphere;
with Utility.Strings;
with Utility.Log;
with Maps.Airspaces.Viewer;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.References is
   
   package Sector_List_Viewer is new Maps.Airspaces.Viewer (Number_Of_Rows    => 3,
                                                            Number_Of_Columns => 3,
                                                            Number_Of_Pages   => 5);
   
   --///////////////////////////////////////////////////////////////////////////
   -- Actions
   --///////////////////////////////////////////////////////////////////////////
   
   Btn_Name    : Button_Record;
   
   Btn_Up      : Button_Record;
   
   Btn_Down    : Button_Record;
  
   Sector_List : Sector_List_Viewer.List_Record;
   
   Btn_Toggle  : Button_Record;
  
   Btn_Qnh     : Button_Record;
  
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
      
      use Utility.Strings;
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.06;
      W : constant Dimension_Float := 0.22;
      
      Allocation : Allocation_Record;
      
   begin

      Allocation.X := 0.16;      
      Allocation.Y := 0.00;      
      Allocation.W := 0.62;      
      Allocation.H := 1.00;
          
      Sector_List.Set_Allocation (Allocation);
      
      Allocation.X := 0.010;      
      Allocation.Y := 0.020;      
      Allocation.W := 0.140;      
      Allocation.H := 0.100;
      
      Btn_Toggle.Set_Allocation (Allocation);
      Btn_Toggle.Set_Style (Button_Normal);
      Btn_Toggle.Set_Label ("TOGGLE");
      Btn_Toggle.Set_Font_Size (0.35, 0.3);
           
      Allocation.Y := Allocation.Y + Allocation.H + 0.02;      
      
      Btn_Qnh.Set_Allocation (Allocation);
      Btn_Qnh.Set_Style (Button_Normal);
      Btn_Qnh.Set_Label ("QNH");
      Btn_Qnh.Set_Font_Size (0.35, 0.3);
      
   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Refresh_List is
   begin
      
      Sector_List.Build_List (Display.Pages.Navigation.Get_Map_View,
                              Flight.Data.Position,
                              Inside_Only => False);
      
   end Refresh_List;
   -----------------------------------------------------------------------------
   
        
     
   
   --===========================================================================
   -- Draw
   --===========================================================================
   procedure Draw is
      
      use Utility.Strings;
      
   begin

      if Sector_List.Focused then  
       
         Btn_Toggle.Set_Style (Button_Action);

      else
         Btn_Toggle.Set_Style (Button_Disabled);
         
      end if;
            
      Btn_Qnh.Set_Label ("Q" & Float_Image (Utility.Atmosphere.Get_Qnh, 0));
                     
      if Utility.Atmosphere.Get_Qnh_Valid then
            
         Btn_Qnh.Set_Style (Button_Enabled);
            
      else
         Btn_Qnh.Set_Style (Button_Disabled);
         
      end if;

      Btn_Qnh.Draw;
      
      Btn_Toggle.Draw;
      
      Sector_List.Draw_List (Flight.Data.Position,
                             True,
                             Display.Blink);
 
      Display.Panels.Gauges.Draw;
       
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
      
      if Sector_List.Contains (X, Y) then
         
         Sector_List.Focus_Item (X, Y);
         
         return;
         
      elsif Sector_List.Focused and then Btn_Toggle.Contains (X, Y) then
         
         --Sector_List.Toggle_Item;
            
         return;
            
      elsif Btn_Qnh.Contains (X, Y) then
            
         Utility.Atmosphere.Set_Qnh (Utility.Atmosphere.Get_Qnh, not Utility.Atmosphere.Get_Qnh_Valid);
            
         return;
         
      end if;
      
      Display.Panels.Gauges.Screen_Pressed (X, Y);
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------
      
   
   
   Update_Page : Boolean := False;
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float) is
      
      use Utility.Strings;
      
      Step : constant Float := 0.12;
      
   begin
      
      if Sector_List.Contains (X, Y) then
         
         if abs Dx < Step then
           
           Update_Page := True;
           
         elsif Update_Page then
      
            if Dx < 0.0 then
            
               Sector_List.Next_Page;
         
               Refresh := True;
         
            elsif Dx > 0.0 then
               
               Sector_List.Previous_Page;
                     
               Refresh := True;
               
            end if;
            
            Update_Page := False;
            
         end if;
         
      end if;
      
   end Screen_Move;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Handles a panel key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
    
      null;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
   
end Display.Pages.References;
--------------------------------------------------------------------------------
