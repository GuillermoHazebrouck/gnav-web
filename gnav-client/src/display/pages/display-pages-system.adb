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
package body Display.Pages.System is
        
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.010,
                                             Height    => 0.035,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Regular);

   Pnl_Aprs          : Panel_Record;

   Btn_Traffic       : Button_Record;
   
   Btn_Follow        : Button_Record;
   
   Pnl_Misc          : Panel_Record;

   Btn_Simu          : Button_Record;
   
   Btn_Utc           : Button_Record;
   
   Btn_Utc_Plus      : Button_Record;
   
   Btn_Utc_Min       : Button_Record;
      
   Pnl_Alerts        : Panel_Record;

   Btn_Alerts        : Button_Record;
   
   Btn_Margin        : Button_Record;
   
   Btn_Margin_Plus   : Button_Record;
   
   Btn_Margin_Min    : Button_Record;
   
   Pnl_Versions      : Panel_Record;

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
      Allocation.W := 0.30;

      Pnl_Misc.Set_Allocation (Allocation);

      Pnl_Misc.Set_Background_Color (Color_Gray_1);

      Pnl_Misc.Set_Font_Size (0.03, 0.25);

      Pnl_Misc.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Misc.Set_Label ("MISC");

      Allocation.X := Pnl_Misc.Get_Allocation.X + M;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Pnl_Misc.Get_Allocation.Y + Pnl_Misc.Get_Allocation.H - 5.0 * M - Allocation.H;
                  
      Btn_Simu.Set_Allocation (Allocation);

      Btn_Simu.Set_Style (Button_Disabled);

      Btn_Simu.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Simu.Set_Label ("SIMULATE");
      
      -- Alerts panel
      ----------------------------------------------------
        
      Allocation.X := Pnl_Misc.Get_Allocation.X + Pnl_Misc.Get_Allocation.W + M;
      Allocation.H := 0.54;
      Allocation.Y := Pnl_Misc.Get_Allocation.Y;
      Allocation.W := 0.38;

      Pnl_Alerts.Set_Allocation (Allocation);

      Pnl_Alerts.Set_Background_Color (Color_Gray_1);

      Pnl_Alerts.Set_Font_Size (0.03, 0.25);

      Pnl_Alerts.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Alerts.Set_Label ("SETUP");

      Allocation.X := Pnl_Alerts.Get_Allocation.X + M;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Pnl_Alerts.Get_Allocation.Y + Pnl_Alerts.Get_Allocation.H - 5.0 * M - Allocation.H;
                  
      Btn_Alerts.Set_Allocation (Allocation);

      Btn_Alerts.Set_Style (Button_Disabled);

      Btn_Alerts.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Alerts.Set_Label ("ALERTS");
      
      Allocation.X := Btn_Alerts.Get_Allocation.X;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Btn_Alerts.Get_Allocation.Y - M - Allocation.H - 0.1;
                  
      Btn_Margin.Set_Allocation (Allocation);

      Btn_Margin.Set_Style (Button_Alive);

      Btn_Margin.Set_Font_Size (0.55, 0.3, 0.6);
  
      Btn_Margin.Set_Label (Utility.Strings.Float_Image (Flight.Safety_Height, 0));
            
      Allocation.X := Allocation.X + Allocation.W + 0.01;
      Allocation.W := 0.075;
      
      Btn_Margin_Min.Set_Allocation (Allocation);

      Btn_Margin_Min.Set_Style (Button_Action);
      
      Btn_Margin_Min.Set_Label ("-");
      
      Allocation.X := Allocation.X + Allocation.W + 0.01;
                  
      Btn_Margin_Plus.Set_Allocation (Allocation);

      Btn_Margin_Plus.Set_Style (Button_Action);
      
      Btn_Margin_Plus.Set_Label ("+");
      
      -- Versions panel
      ----------------------------------------------------
        
      Allocation.X := Pnl_Alerts.Get_Allocation.X + Pnl_Alerts.Get_Allocation.W + M;
      Allocation.H := 0.54;
      Allocation.Y := Pnl_Misc.Get_Allocation.Y;
      Allocation.W := 1.0 - M - Allocation.X;

      Pnl_Versions.Set_Allocation (Allocation);

      Pnl_Versions.Set_Background_Color (Color_Gray_1);

      Pnl_Versions.Set_Font_Size (0.03, 0.25);

      Pnl_Versions.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Versions.Set_Label ("VERSION");

   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Glex.Fonts;
      use Utility.Strings;
      
      Color : Line_Color_Record;
      X, Y  : Float := 0.0; 
      
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
      
      Glex.Fonts.Draw (Integer_Image (Flight.Traffic.Number_Of_Tracks) & " TRACKS",
                       Pnl_Aprs.Get_Allocation.X + Pnl_Aprs.Get_Allocation.W - 0.02, 
                       Pnl_Aprs.Get_Allocation.Y + Pnl_Aprs.Get_Allocation.H - 0.05,
                       Font_1,
                       Line_Gray,
                       Alignment_TR);
      
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
      
      Pnl_Alerts.Draw;
      
      if Maps.Airspaces.Allow_Notifications then
         Btn_Alerts.Set_Style (Button_Enabled);
      else
         Btn_Alerts.Set_Style (Button_Disabled);
      end if;
      
      Btn_Alerts.Draw;
      
      -- Safety height
      ---------------------------------------
      
      Glex.Fonts.Draw ("SAFETY BUFFER:", 
                       Btn_Margin.Get_Allocation.X, Btn_Margin.Get_Allocation.Y + Btn_Margin.Get_Allocation.H + 0.03,
                       Font_1,
                       Line_Gray,
                       Alignment_LL);
            
      Btn_Margin.Draw;
      
      Btn_Margin_Min.Draw;
      
      Btn_Margin_Plus.Draw;
      
      -- Versions
      ---------------------------------------
      
      Pnl_Versions.Draw;
      
      X := Pnl_Versions.Get_Allocation.X + 0.02;
      Y := Pnl_Versions.Get_Allocation.Y + Pnl_Versions.Get_Allocation.H - 0.05;
      
      Glex.Fonts.Draw (Gnav_Info.Service_Name,
                       X, Y,
                       Font_1,
                       Line_Gray,
                       Alignment_TL);
      
      Y := Y - 0.07;
      
      Glex.Fonts.Draw ("WASM: " & Gnav_Info.Core_Version, 
                       X, Y,
                       Font_1,
                       Line_Gray,
                       Alignment_TL);
            
      Y := Y - 0.07;
      
      Glex.Fonts.Draw ("HTML: " & Gnav_Info.Html_Version, 
                       X, Y,
                       Font_1,
                       Line_Gray,
                       Alignment_TL);
      
      
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
         
      elsif Btn_Margin_Plus.Contains (X, Y) and Flight.Safety_Height <= 350.0 then
         
         Flight.Safety_Height := Flight.Safety_Height + 50.0;
         
         Btn_Margin.Set_Label (Utility.Strings.Float_Image (Flight.Safety_Height, 0));
      
      elsif Btn_Margin_Min.Contains (X, Y) and Flight.Safety_Height >= 200.0 then
         
         Flight.Safety_Height := Flight.Safety_Height - 50.0;
         
         Btn_Margin.Set_Label (Utility.Strings.Float_Image (Flight.Safety_Height, 0));
      
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
          
     
     
end Display.Pages.System;
--------------------------------------------------------------------------------
