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
with Display.Pages.System;
with Display.Pages.Glider;
with Display.Pages.Info;
with Display.Pages.Wind;
with Flight.Aircraft;
with Widgets.Widget;
with Widgets.Button;
use  Widgets.Button;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Menu is
           
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The five front panel keys
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Page_Types is (Page_Glider,
                       Page_Wind,
                       Page_Aprs,
                       Page_Info);

   Btn_Glider   : Button_Record;
   
   Btn_Info     : Button_Record;
   
   Btn_Aprs     : Button_Record;
   
   Btn_Wind     : Button_Record;
   
   Btn_Log      : Button_Record;
   
   Active_Page  : Page_Types := Page_Info;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
      
      use Widgets.Widget;
      
      Allocation : Allocation_Record;
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.10;
      W : constant Dimension_Float := 0.15;
      
   begin
      
      Display.Pages.Glider.Initialize;
      
      Display.Pages.Info.Initialize;
            
      Display.Pages.System.Initialize;
      
      Display.Pages.Wind.Initialize;
        
      Allocation.X := 0.155 + M;
      Allocation.Y := 1.00 - (H + M);
      Allocation.W := 0.15;
      Allocation.H := H;

      -- AIS tab (QNH/METAR/RADIO)
      -------------------------------------------------
      
      Btn_Info.Set_Allocation (Allocation);
      
      Btn_Info.Set_Style (Button_Enabled);
         
      Btn_Info.Set_Font_Size (0.4, 0.3, 0.6);
      
      Btn_Info.Set_Label ("INFO");
       
      -- Aircraft tab
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;

      Btn_Glider.Set_Allocation (Allocation);

      Btn_Glider.Set_Style (Button_Normal);
   
      Btn_Glider.Set_Font_Size (0.4, 0.3, 0.6);

      -- WIND tab
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Btn_Wind.Set_Allocation (Allocation);
      
      Btn_Wind.Set_Style (Button_Normal);

      Btn_Wind.Set_Font_Size (0.4, 0.3, 0.6);
  
      Btn_Wind.Set_Label ("WIND");
       
      -- SYSTEM tab
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;

      Btn_Aprs.Set_Allocation (Allocation);

      Btn_Aprs.Set_Style (Button_Normal);
     
      Btn_Aprs.Set_Font_Size (0.4, 0.3, 0.6);
  
      Btn_Aprs.Set_Label ("SETUP");
       
   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Utility.Strings;
      
   begin
      
      case Active_Page is
         
         when Page_Info =>
            
            Pages.Info.Draw;
                     
         when Page_Glider =>
            
            Pages.Glider.Draw;
            
         when Page_Aprs =>
                        
            Pages.System.Draw;
                     
         when Page_Wind =>
            
            Pages.Wind.Draw;
                 
      end case;
      
      Btn_Glider.Set_Label (Trim (Flight.Aircraft.This_Aircraft.Model));
      
      Btn_Glider.Draw;

      Btn_Aprs.Draw;
     
      Btn_Wind.Draw;
      
      Btn_Info.Draw;
      
      Btn_Log.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
      
   
   --===========================================================================
   --
   --===========================================================================
   procedure Reset_Button_Styles is
   begin
           
      Btn_Log.Set_Style    (Button_Normal);
          
      Btn_Aprs.Set_Style   (Button_Normal);
     
      Btn_Wind.Set_Style   (Button_Normal);
      
      Btn_Info.Set_Style  (Button_Normal);
      
      Btn_Glider.Set_Style (Button_Normal);
      
   end Reset_Button_Styles;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      if Btn_Aprs.Contains (X, Y) then

         Active_Page := Page_Aprs;
                  
         Reset_Button_Styles;
         
         Btn_Aprs.Set_Style (Button_Enabled);
                      
         Refresh := True;
         
      elsif Btn_Info.Contains (X, Y) then

         Active_Page := Page_Info;
                           
         Reset_Button_Styles;
         
         Btn_Info.Set_Style (Button_Enabled);
         
         Refresh := True;
         
      elsif Btn_Wind.Contains (X, Y) then

         Active_Page := Page_Wind;
                          
         Reset_Button_Styles;
         
         Btn_Wind.Set_Style (Button_Enabled);
           
         Refresh := True;
         
      elsif Btn_Glider.Contains (X, Y) then

         Active_Page := Page_Glider;
                         
         Reset_Button_Styles;
         
         Btn_Glider.Set_Style (Button_Enabled);
             
         Refresh := True;
         
      else
               
         case Active_Page is
         
            when Page_Info =>
            
               Pages.Info.Screen_Pressed (X, Y);
                     
            when Page_Glider =>
            
               Pages.Glider.Screen_Pressed (X, Y);
                     
            when Page_Aprs =>
            
               Pages.System.Screen_Pressed (X, Y);
                     
            when Page_Wind =>
            
               Pages.Wind.Screen_Pressed (X, Y);
                
         end case;
      
      end if;
         
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean) is
   begin
       
      case Active_Page is
         
         when Page_Info =>
            
            Pages.Info.Screen_Move (X, Y, Dx, Dy, First);
                 
         when Page_Glider =>
            
            Pages.Glider.Screen_Move (X, Y, Dx, Dy, First);
                  
         when others => 
            
            null;
            
      end case;
      
   end Screen_Move;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      null;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.Menu;
--------------------------------------------------------------------------------
