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
-- Standard
-- Gnav
with Display.Pages.Navigation;
with Display.Pages.System;
with Display.Pages.Strips;
with Flight.Aircraft;
with Flight.Plan;
with Glex.Colors;
use  Glex.Colors;
with Widgets.Widget;
use  Widgets.Widget;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Dialog;
with Widgets.Keyboard;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Menu is
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The five front panel keys
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Page_Types is (Page_Navigation,
                       Page_Strips,
                       Page_System);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Page : Page_Types := Page_Navigation;
      
   -- Main menu buttons
   ---------------------------------

   Btn_Strips  : Button_Record;
   
   Btn_Check   : Button_Record;
   
   Btn_Exit    : Button_Record;
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Initialize is
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := (1.0 - 9.0 * M) / 8.0;
      W : constant Dimension_Float := 0.15;

      Allocation : Allocation_Record;
      
   begin

      -- General confirmation dialog and keyboard
      -----------------------------------------------------
      Widgets.Dialog.Initialize;
      
      Widgets.Keyboard.Initialize;
                        
      -- Flight plan page button
      ------------------------------------------------------      
      Allocation.X := 0.005;
      
      Allocation.H := H;
      
      Allocation.Y := 1.0 - H - M;
      
      Allocation.W := W;

      Btn_Strips.Set_Label ("ROUTE");
      
      Btn_Strips.Set_Allocation (Allocation);
      
      Btn_Strips.Set_Background_Color (Color_Gray_5);
      
      Btn_Strips.Set_Label_Color (Fore => Color_White,
                                      Glow => Color_Black);
      
      Btn_Strips.Set_Font_Size (0.3, 0.28);
       
      -- Checklist page button
      ------------------------------------------------------
      Btn_Check.Set_Label ("CHECK");
      
      Allocation.X := 0.005;
      
      Allocation.Y := Allocation.Y - H - M;
      
      Allocation.W := W;
      
      Btn_Check.Set_Allocation (Allocation);
            
      Btn_Check.Set_Background_Color (Color_Gray_5);
      
      Btn_Check.Set_Label_Color (Fore => Color_White,
                                 Glow => Color_Black);
      
      Btn_Check.Set_Font_Size (0.3, 0.3);

      -- Checklist page button
      ------------------------------------------------------      
      Allocation.X := 0.005;
      
      Allocation.H := 0.100;
      
      Allocation.Y := 0.890;
      
      Allocation.W := 0.150;
        
      Btn_Exit.Set_Label ("< NAV");
      
      Btn_Exit.Set_Allocation (Allocation);
            
      Btn_Exit.Set_Style (Button_Cancel);
        
      Btn_Exit.Set_Font_Size (0.4, 0.3);
      
      -- Sub pages
      ------------------------------------------------------
      Display.Pages.Navigation.Initialize;
      
      Display.Pages.Strips.Initialize;
      
      Display.Pages.System.Initialize;
      
   end Initialize;
   -----------------------------------------------------------------------------
      
   
   
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Draw is      
   begin
      
      -- Draw the active page under the menu
      --------------------------------------
      
      case Active_Page is
            
         when Page_Navigation =>
                 
            Display.Pages.Navigation.Draw;
            
            Btn_Strips.Draw;
            
            Btn_Check.Draw;
         
         when Page_Strips =>

            Display.Pages.Strips.Draw;
               
            Btn_Exit.Draw;
             
         when Page_System =>
            
            Display.Pages.System.Draw;
            
            Btn_Exit.Draw;
               
      end case;
      
      -- Dialog (this must always be the last one)
      -----------------------------------------------
      Widgets.Dialog.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   



   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      --------------------------------------------------------------------------	
      -- Handle pending action on dialog
      --------------------------------------------------------------------------
      
      if Widgets.Dialog.Is_Open then
         
         Widgets.Dialog.Handle_Action (X, Y);
         
         return;
         
      end if;
         
      --------------------------------------------------------------------------	
      -- Check main menu page widgets first
      --------------------------------------------------------------------------
            
      if Active_Page = Page_Navigation then
         
         if Btn_Strips.Contains (X, Y) then
         
            Active_Page := Page_Strips;
            
            Display.Refresh := True;
         
            return;
             
         elsif Btn_Check.Contains (X, Y) then
         
            Active_Page := Page_System;
      
            Display.Refresh := True;
         
            return;
        
         end if;
          
      elsif Btn_Exit.Contains (X, Y) then
      
         if Active_Page = Page_System then
            
            Flight.Aircraft.Save_Configuration;
            
            Flight.Aircraft.Calculate_Gliding_States;
            
         elsif Active_Page = Page_Strips then
            
            if Display.Pages.Strips.Back_From_Edition_Mode then
               
               Flight.Plan.Save_Configuration;
        
            else               
               Active_Page := Page_Navigation;
        
            end if;
               
            Flight.Plan.Jump_In_Proximity := True;
      
            Display.Refresh := True;
         
            return;
            
         end if;
         
         Active_Page := Page_Navigation;
        
         Flight.Plan.Jump_In_Proximity := True;
      
         Display.Refresh := True;
         
         return;
        
      end if;
      
      --------------------------------------------------------------------------	
      -- Redirect event to other pages
      --------------------------------------------------------------------------
      
      case Active_Page is
         
         when Page_Navigation =>
            
            Display.Pages.Navigation.Screen_Pressed (X, Y);
        
         when Page_Strips =>

            Display.Pages.Strips.Screen_Pressed (X, Y);
       
         when Page_System =>
            
            Display.Pages.System.Screen_Pressed (X, Y);
            
         when others =>
            
            null;
            
      end case;
                  
   end Screen_Pressed;
   -----------------------------------------------------------------------------

   
   
   
   --===========================================================================
   -- (See specifications file)
   -- IMPORTANT: the call rate can be high, each implementation must limit the
   -- refresh.
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean) is
   begin
      
      case Active_Page is

         when Page_System =>
            
            Display.Pages.System.Screen_Move (X, Y, Dx, Dy, First);
            
         when Page_Navigation =>
            
            Display.Pages.Navigation.Screen_Move (X, Y, Dx, Dy, First);
        
         when others =>
            null;
            
      end case;
      
   end Screen_Move;
   -----------------------------------------------------------------------------
   
end Display.Menu;
--------------------------------------------------------------------------------
