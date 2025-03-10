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
with Flight;
with Flight.Traffic;
with Gnav_Info;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Maps;
with Maps.Airspaces;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Dialog;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Strings;
with Utility.Storage;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Aprs is
        
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.010,
                                             Height    => 0.035,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Regular);

   Pnl_Aprs        : Panel_Record;

   Btn_Traffic     : Button_Record;
   
   Btn_Follow      : Button_Record;
   
   Pnl_Misc        : Panel_Record;

   Btn_Simu        : Button_Record;
   
   Btn_Alerts      : Button_Record;
   
   Btn_Utc         : Button_Record;
   
   Btn_Utc_Plus    : Button_Record;
   
   Btn_Utc_Min     : Button_Record;
      
   Btn_Margin      : Button_Record;
   
   Btn_Margin_Plus : Button_Record;
   
   Btn_Margin_Min  : Button_Record;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
      
      Allocation : Allocation_Record;
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.10;
      
   begin
     
      -- Aprs panel
      ----------------------------------------------------
      
      Allocation.X := 0.01;
      Allocation.H := 0.28;
      Allocation.Y := 0.86 - Allocation.H;
      Allocation.W := 0.98;

      Pnl_Aprs.Set_Allocation (Allocation);

      Pnl_Aprs.Set_Background_Color (Color_Gray_1);

      Pnl_Aprs.Set_Font_Size (0.03, 0.25);

      Pnl_Aprs.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Aprs.Set_Label ("APRS");

      -- Master switch
      ----------------------------------------------------
      
      Allocation.X := Pnl_Aprs.Get_Allocation.X + M;
      Allocation.Y := Pnl_Aprs.Get_Allocation.Y + 2.0 * M;
      Allocation.W := 0.15;
      Allocation.H := 0.10;

      Btn_Follow.Set_Allocation (Allocation);
      
      Btn_Follow.Set_Style (Button_Disabled);

      Btn_Follow.Set_Font_Size (0.35, 0.3, 0.6);
 
      Btn_Follow.Set_Label ("FOLLOW");
      
      -- Downlinked info
      ----------------------------------------------------
      
      Allocation.Y := Allocation.Y + Allocation.H + M;
      
      Btn_Traffic.Set_Allocation (Allocation);

      Btn_Traffic.Set_Style (Button_Alive);

      Btn_Traffic.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Traffic.Set_Label ("TRAFFIC");
           
      -- Simulation panel
      ----------------------------------------------------
      
      Allocation.X := 0.01;
      Allocation.H := 0.54;
      Allocation.Y := Pnl_Aprs.Get_Allocation.Y - Allocation.H - 3.0 * M;
      Allocation.W := 0.98;

      Pnl_Misc.Set_Allocation (Allocation);

      Pnl_Misc.Set_Background_Color (Color_Gray_1);

      Pnl_Misc.Set_Font_Size (0.03, 0.25);

      Pnl_Misc.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Misc.Set_Label ("MISC");

      -- Control simulation
      ----------------------------------------------------
      
      Allocation.X := Pnl_Misc.Get_Allocation.X + M;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Pnl_Misc.Get_Allocation.Y + Pnl_Misc.Get_Allocation.H - 5.0 * M - Allocation.H;
                  
      Btn_Simu.Set_Allocation (Allocation);

      Btn_Simu.Set_Style (Button_Disabled);

      Btn_Simu.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Simu.Set_Label ("SIMULATE");
      
      -- Control sector alerts
      ----------------------------------------------------
      
      Allocation.X := Btn_Simu.Get_Allocation.X;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Btn_Simu.Get_Allocation.Y - M - Allocation.H;
                  
      Btn_Alerts.Set_Allocation (Allocation);

      Btn_Alerts.Set_Style (Button_Disabled);

      Btn_Alerts.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Alerts.Set_Label ("ALERTS");
      
   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Glex.Fonts;
      use Utility.Strings;
      
      Color : Line_Color_Record;
      
   begin
      
      Pnl_Aprs.Draw;
      
      -- APRS Squawk
      
      Glex.Fonts.Draw ("SQUAWK: " & Gnav_Info.User_Id,
                       Btn_Traffic.Get_Allocation.X + Btn_Traffic.Get_Allocation.W + 0.02, 
                       Btn_Traffic.Get_Allocation.Y + 0.03,
                       Font_1,
                       Line_Green);
      
      -- APRS status
      
      if Flight.Traffic.Enabled then

         Btn_Traffic.Set_Style (Button_Enabled);
               
         if not Flight.Traffic.Follow then
            Color := Line_Green;
         elsif Flight.Data.Is_Recent (Flight.Field_Position, 4.0) then
            Color := Line_Cyan;
         elsif Flight.Data.Is_Valid  (Flight.Field_Position) then         
            Color := Line_Gray;        
         else         
            Color := Line_Red;
         end if;
         
         Glex.Fonts.Draw (Flight.Traffic.Get_Aprs_Data,
                          Btn_Follow.Get_Allocation.X + Btn_Traffic.Get_Allocation.W + 0.02, 
                          Btn_Follow.Get_Allocation.Y + 0.03,
                          Font_1,
                          Color);
         
         if Flight.Traffic.Follow then            
            Btn_Follow.Set_Style (Button_Enabled);            
         else            
            Btn_Follow.Set_Style (Button_Disabled);            
         end if;
                           
      else

         Btn_Traffic.Set_Style (Button_Disabled);
         Btn_Follow.Set_Style (Button_Disabled); 
         
      end if;
      
      Btn_Traffic.Draw;
      Btn_Follow.Draw;
      
      -- Simulation
      ---------------------------------------
      
      Pnl_Misc.Draw;
      
      if Gnav_Info.Simulation_Mode then
         Btn_Simu.Set_Style (Button_Enabled);
      else
         Btn_Simu.Set_Style (Button_Disabled);
      end if;
      
      Btn_Simu.Draw;
      
      -- Alerts
      ---------------------------------------
      
      if Maps.Airspaces.Allow_Notifications then
         Btn_Alerts.Set_Style (Button_Enabled);
      else
         Btn_Alerts.Set_Style (Button_Disabled);
      end if;
      
      Btn_Alerts.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
     
   
   --===========================================================================
   --
   --===========================================================================
   procedure Confirm_Simulation (Result : Widgets.Dialog.Dialog_Result_Kind) is
      use Widgets.Dialog;
   begin
      
      if Result = Dialog_Ok then
         Gnav_Info.Simulation_Mode  := True;
         Gnav_Info.Simulation_Reset := True;
         Flight.Traffic.Enabled     := False;
      end if;
      
   end Confirm_Simulation;
   -----------------------------------------------------------------------------
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
      
      if Btn_Traffic.Contains (X, Y) then
         
         Flight.Traffic.Enabled := not Flight.Traffic.Enabled;
         
         if Gnav_Info.Simulation_Mode then
            Flight.Traffic.Enabled := False;
         end if;
         
         Flight.Traffic.Save_Configuration;
         
         Refresh := True;
     
      elsif Flight.Traffic.Enabled and then Btn_Follow.Contains (X, Y) then
         
         Flight.Traffic.Follow := not Flight.Traffic.Follow;
         
         Flight.Traffic.Save_Configuration;
         
         Refresh := True;
     
      elsif Btn_Alerts.Contains (X, Y) then
         
         Maps.Airspaces.Allow_Notifications := not Maps.Airspaces.Allow_Notifications;
         
         Maps.Airspaces.Save_Master_Alerts_Switch;
         
         Refresh := True;
     
      elsif Btn_Simu.Contains (X, Y) then
         
         if Gnav_Info.Simulation_Mode then
            Gnav_Info.Simulation_Mode  := False;
         else
            Widgets.Dialog.Confirm ("ACTIVATE SIMULATION MODE", Confirm_Simulation'Access);
         end if;
         
      end if;
         
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      null;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.Aprs;
--------------------------------------------------------------------------------
