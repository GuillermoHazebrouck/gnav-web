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
with Glex.Symbols;
with Maps;
with Maps.Airspaces;
with Maps.Terrain;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Dialog;
with Widgets.Keyboard;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Calendar;
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

   Font_2 : Glex.Fonts.Font_Style_Record := (Width     => 0.007,
                                             Height    => 0.020,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Regular);
     
   Font_3 : Glex.Fonts.Font_Style_Record := (Width     => 0.030,
                                             Height    => 0.100,
                                             Space     => 0.015,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Regular);

   Font_4 : Glex.Fonts.Font_Style_Record := (Width     => 0.020,
                                             Height    => 0.070,
                                             Space     => 0.010,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Regular);

   Pnl_Aprs          : Panel_Record;

   Btn_Traffic       : Button_Record;
   
   Btn_Follow        : Button_Record;
   
   Btn_Squawk        : Button_Record;
   
   Btn_Tailmark      : Button_Record;
   
   Btn_Ok            : Button_Record;
   
   Btn_Cancel        : Button_Record;
   
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
   
   Btn_Bias          : Button_Record;
   
   Btn_Bias_Plus     : Button_Record;
   
   Btn_Bias_Min      : Button_Record;
   
   Pnl_Versions      : Panel_Record;

   Btn_Resolution    : Button_Record;
   
   New_Altitude_Bias : Float := 0.0;
   
   Squawk_Edit_Mode  : Boolean := False;
   
   Mark_Edit_Mode    : Boolean := False;
   
   New_Squawk        : Gnav_Info.Id_Type := Gnav_Info.No_Id;
   
   New_Mark          : Gnav_Info.Tm_Type := Gnav_Info.No_Tm;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
      
      Allocation : Allocation_Record;
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.10;
      
   begin

      -- Misc panel
      ----------------------------------------------------
      
      Allocation.X := 0.01;
      Allocation.H := 0.54;
      Allocation.Y := 0.86 - Allocation.H;
      Allocation.W := 0.35;

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
    
      -- UTC offset
      
      Allocation.X := Btn_Simu.Get_Allocation.X;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Btn_Simu.Get_Allocation.Y - M - Allocation.H - 0.07;
                  
      Btn_Utc.Set_Allocation (Allocation);

      Btn_Utc.Set_Style (Button_Alive);

      Btn_Utc.Set_Font_Size (0.55, 0.3, 0.6);
  
      Btn_Utc.Set_Label (Utility.Strings.Integer_Image (Flight.Get_Time_Zone));
      
      Allocation.X := Allocation.X + Allocation.W + 0.01;
      Allocation.W := 0.075;
      
      Btn_Utc_Min.Set_Allocation (Allocation);

      Btn_Utc_Min.Set_Style (Button_Action);
      
      Btn_Utc_Min.Set_Symbol (Glex.Symbols.Triangle_Down);
      
      Allocation.X := Allocation.X + Allocation.W + 0.01;
                  
      Btn_Utc_Plus.Set_Allocation (Allocation);

      Btn_Utc_Plus.Set_Style (Button_Action);
      
      Btn_Utc_Plus.Set_Symbol (Glex.Symbols.Triangle_Up);
             
      -- Terrain resolution
                
      Allocation.X := Btn_Simu.Get_Allocation.X;
      Allocation.W := 0.15;
      Allocation.Y := Btn_Utc.Get_Allocation.Y - M - Allocation.H - 0.07;
      
      Btn_Resolution.Set_Allocation (Allocation);

      Btn_Resolution.Set_Style (Button_Alive);

      Btn_Resolution.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Resolution.Set_Label ("LR");
            
      -- Setup panel
      ----------------------------------------------------
        
      Allocation.X := Pnl_Misc.Get_Allocation.X + Pnl_Misc.Get_Allocation.W + M;
      Allocation.H := 0.54;
      Allocation.Y := Pnl_Misc.Get_Allocation.Y;
      Allocation.W := 0.35;

      Pnl_Alerts.Set_Allocation (Allocation);

      Pnl_Alerts.Set_Background_Color (Color_Gray_1);

      Pnl_Alerts.Set_Font_Size (0.03, 0.25);

      Pnl_Alerts.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Alerts.Set_Label ("SYSTEM");

      Allocation.X := Pnl_Alerts.Get_Allocation.X + M;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Pnl_Alerts.Get_Allocation.Y + Pnl_Alerts.Get_Allocation.H - 5.0 * M - Allocation.H;
                  
      Btn_Alerts.Set_Allocation (Allocation);

      Btn_Alerts.Set_Style (Button_Disabled);

      Btn_Alerts.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Alerts.Set_Label ("ALERTS");
      
      -- Safety margin
      
      Allocation.X := Btn_Alerts.Get_Allocation.X;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Btn_Alerts.Get_Allocation.Y - M - Allocation.H - 0.07;
                  
      Btn_Margin.Set_Allocation (Allocation);

      Btn_Margin.Set_Style (Button_Alive);

      Btn_Margin.Set_Font_Size (0.55, 0.3, 0.6);
  
      Btn_Margin.Set_Label (Utility.Strings.Float_Image (Flight.Safety_Height, 0));
            
      Allocation.X := Allocation.X + Allocation.W + 0.01;
      Allocation.W := 0.075;
      
      Btn_Margin_Min.Set_Allocation (Allocation);

      Btn_Margin_Min.Set_Style (Button_Action);
      
      Btn_Margin_Min.Set_Symbol (Glex.Symbols.Triangle_Down);
      
      Allocation.X := Allocation.X + Allocation.W + 0.01;
                  
      Btn_Margin_Plus.Set_Allocation (Allocation);

      Btn_Margin_Plus.Set_Style (Button_Action);
      
      Btn_Margin_Plus.Set_Symbol (Glex.Symbols.Triangle_Up);
                  
      -- Altitude correction
      
      New_Altitude_Bias := Gnav_Info.Altitude_Bias;
      
      Allocation.X := Btn_Margin.Get_Allocation.X;
      Allocation.W := 0.15;
      Allocation.H := 0.10;
      Allocation.Y := Btn_Margin.Get_Allocation.Y - M - Allocation.H - 0.07;
                  
      Btn_Bias.Set_Allocation (Allocation);

      Btn_Bias.Set_Style (Button_Alive);

      Btn_Bias.Set_Font_Size (0.55, 0.3, 0.6);
  
      Btn_Bias.Set_Label (Utility.Strings.Float_Image (New_Altitude_Bias, 0));
            
      Allocation.X := Allocation.X + Allocation.W + 0.01;
      Allocation.W := 0.075;
      
      Btn_Bias_Min.Set_Allocation (Allocation);

      Btn_Bias_Min.Set_Style (Button_Action);
      
      Btn_Bias_Min.Set_Symbol (Glex.Symbols.Triangle_Down);
      
      Allocation.X := Allocation.X + Allocation.W + 0.01;
                  
      Btn_Bias_Plus.Set_Allocation (Allocation);

      Btn_Bias_Plus.Set_Style (Button_Action);
      
      Btn_Bias_Plus.Set_Symbol (Glex.Symbols.Triangle_Up);
      
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
     
      -- Aprs panel
      ----------------------------------------------------
      
      Allocation.X := 0.01;
      Allocation.H := 0.28;
      Allocation.Y := Pnl_Misc.Get_Allocation.Y - Allocation.H - 3.0 * M;
      Allocation.W := 0.98;

      Pnl_Aprs.Set_Allocation (Allocation);

      Pnl_Aprs.Set_Background_Color (Color_Gray_1);

      Pnl_Aprs.Set_Font_Size (0.03, 0.25);

      Pnl_Aprs.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Aprs.Set_Label ("APRS");

      -- Follow traffic switch
      
      Allocation.X := Pnl_Aprs.Get_Allocation.X + M;
      Allocation.Y := Pnl_Aprs.Get_Allocation.Y + 2.0 * M;
      Allocation.W := 0.15;
      Allocation.H := 0.10;

      Btn_Follow.Set_Allocation (Allocation);
      
      Btn_Follow.Set_Style (Button_Disabled);

      Btn_Follow.Set_Font_Size (0.35, 0.3, 0.6);
 
      Btn_Follow.Set_Label ("FOLLOW");
      
      -- Master APRS switch
      
      Allocation.Y := Allocation.Y + Allocation.H + M;
      
      Btn_Traffic.Set_Allocation (Allocation);

      Btn_Traffic.Set_Style (Button_Alive);

      Btn_Traffic.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Traffic.Set_Label ("TRAFFIC");
           
      -- Id and callsign edition buttons
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Btn_Squawk.Set_Allocation (Allocation);

      Btn_Squawk.Set_Style (Button_Normal);

      Btn_Squawk.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Squawk.Set_Label ("SQUAWK");
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Btn_Tailmark.Set_Allocation (Allocation);

      Btn_Tailmark.Set_Style (Button_Normal);

      Btn_Tailmark.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Tailmark.Set_Label ("PILOT");
      
      Allocation.X := (1.0 - 2.0 * Allocation.W - 0.03) * 0.5;
      Allocation.Y := 0.35;

      Btn_Ok.Set_Allocation (Allocation);
   
      Btn_Ok.Set_Style (Button_Ok);

      Btn_Ok.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Ok.Set_Label ("CHANGE");
           
      Allocation.X := Allocation.X + Allocation.W + 0.03;
      
      Btn_Cancel.Set_Allocation (Allocation);
      
      Btn_Cancel.Set_Style (Button_Cancel);
      
      Btn_Cancel.Set_Font_Size (0.35, 0.3, 0.6);
  
      Btn_Cancel.Set_Label ("CANCEL");
           
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
      
      if Squawk_Edit_Mode or else Mark_Edit_Mode then
         
         -- APRS Squawk
      
         if Squawk_Edit_Mode then
            
            Glex.Fonts.Draw (New_Squawk,
                             0.5, 
                             Btn_Ok.Get_Allocation.Y + Btn_Ok.Get_Allocation.H + 0.1,
                             Font_3,
                             Line_White, Alignment_LC);
            
         elsif Mark_Edit_Mode then
            
            Glex.Fonts.Draw (New_Mark,
                             0.5, 
                             Btn_Ok.Get_Allocation.Y + Btn_Ok.Get_Allocation.H + 0.1,
                             Font_3,
                             Line_White, Alignment_LC);
            
         end if;
                     
         Widgets.Keyboard.Set_Allocation ((0.01, 0.01, 0.98, 0.30));

         Widgets.Keyboard.Draw;
         
         Btn_Ok.Draw;
         
         Btn_Cancel.Draw;
         
      else
         
         Pnl_Aprs.Draw;
      
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
         Btn_Squawk.Draw;
         Btn_Tailmark.Draw;
      
         Glex.Fonts.Draw (Gnav_Info.User_Tm,
                          Btn_Tailmark.Get_Allocation.X + Btn_Tailmark.Get_Allocation.W + 0.02, 
                          Btn_Tailmark.Get_Allocation.Y + Btn_Tailmark.Get_Allocation.H * 0.50,
                          Font_4,
                          Line_White,
                          Alignment_CL);
      
         Glex.Fonts.Draw (Integer_Image (Flight.Traffic.Number_Of_Tracks) & " TRACKS",
                          Pnl_Aprs.Get_Allocation.X + Pnl_Aprs.Get_Allocation.W - 0.02, 
                          Pnl_Aprs.Get_Allocation.Y + Pnl_Aprs.Get_Allocation.H - 0.05,
                          Font_1,
                          Line_Gray,
                          Alignment_TR);
      
         -- Miscellaneus
         ---------------------------------------
      
         Pnl_Misc.Draw;
      
         if Gnav_Info.Simulation_Mode then
            Btn_Simu.Set_Style (Button_Enabled);
         else
            Btn_Simu.Set_Style (Button_Disabled);
         end if;
      
         Btn_Simu.Draw;

         -- UTC offset
      
         Glex.Fonts.Draw ("TIME ZONE:", 
                          Btn_Utc.Get_Allocation.X, Btn_Utc.Get_Allocation.Y + Btn_Utc.Get_Allocation.H + 0.02,
                          Font_2,
                          Line_Gray,
                          Alignment_LL);
            
         Btn_Utc.Draw;
      
         Btn_Utc_Min.Draw;
      
         Btn_Utc_Plus.Draw;
      
         -- Terrain resolution
               
         Glex.Fonts.Draw ("TERRAIN RESOLUTION:", 
                          Btn_Resolution.Get_Allocation.X, Btn_Resolution.Get_Allocation.Y + Btn_Resolution.Get_Allocation.H + 0.02,
                          Font_2,
                          Line_Gray,
                          Alignment_LL);
            
         if Maps.Terrain.Get_High_Resolution then
            Btn_Resolution.Set_Label ("HIGH");
         else
            Btn_Resolution.Set_Label ("LOW");
         end if;
      
         Btn_Resolution.Draw;
               
         -- Setup
         ---------------------------------------
      
         Pnl_Alerts.Draw;
      
         if Maps.Airspaces.Allow_Notifications then
            Btn_Alerts.Set_Style (Button_Enabled);
         else
            Btn_Alerts.Set_Style (Button_Disabled);
         end if;
      
         Btn_Alerts.Draw;
      
         -- Safety height
      
         Glex.Fonts.Draw ("ALTITUDE BUFFER:", 
                          Btn_Margin.Get_Allocation.X, Btn_Margin.Get_Allocation.Y + Btn_Margin.Get_Allocation.H + 0.02,
                          Font_2,
                          Line_Gray,
                          Alignment_LL);
            
         Btn_Margin.Draw;
      
         Btn_Margin_Min.Draw;
      
         Btn_Margin_Plus.Draw;
      
         -- Altitude correction
      
         Glex.Fonts.Draw ("ALTITUDE BIAS:", 
                          Btn_Bias.Get_Allocation.X, Btn_Bias.Get_Allocation.Y + Btn_Bias.Get_Allocation.H + 0.02,
                          Font_2,
                          Line_Gray,
                          Alignment_LL);
            
         Btn_Bias.Draw;
      
         Btn_Bias_Min.Draw;
      
         Btn_Bias_Plus.Draw;
      
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
      
      end if;
      
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
   function Is_Alphanumerics (Text : String) return Boolean is
   begin
      
      for C of Text loop
         if not (C in 'A'..'Z') and then not (C in '0'..'9') then
            return False;
         end if;
      end loop;
      return True;
      
   end Is_Alphanumerics;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
      
      use Utility.Calendar;
      use Utility.Strings;
      
      Time_Zone  : Integer := Flight.Get_Time_Zone;
      Valid_Code : Boolean := False;
      
   begin
      
      if Squawk_Edit_Mode or else Mark_Edit_Mode then
         
         if Widgets.Keyboard.Key_Pressed (X, Y) then
         
            if Squawk_Edit_Mode then
               
               Utility.Strings.Override (New_Squawk, Widgets.Keyboard.Get_Text);
            
               Valid_Code := Is_Alphanumerics (New_Squawk);
               
            elsif Mark_Edit_Mode then
               
               Utility.Strings.Override (New_Mark, Widgets.Keyboard.Get_Text);
            
               Valid_Code := Is_Alphanumerics (New_Mark);
               
            end if;
               
            if Valid_Code then
                  
               Btn_Ok.Set_Style (Button_Ok);  
                  
            else                  
               Btn_Ok.Set_Style (Button_Disabled);  
                  
            end if;
            
            Refresh := True;
     
         elsif Btn_Cancel.Contains (X, Y) then
            
            Squawk_Edit_Mode := False;
            
            Mark_Edit_Mode   := False;
            
            Refresh := True;
     
         elsif Btn_Ok.Contains (X, Y) then
            
            if Squawk_Edit_Mode and then Is_Alphanumerics (New_Squawk) then
               
               Squawk_Edit_Mode := False;
            
               Gnav_Info.User_Id := New_Squawk;
               
               Utility.Storage.Set_Item ("ID", Gnav_Info.User_Id);
            
            elsif Mark_Edit_Mode and then Is_Alphanumerics (New_Mark) then
               
               Mark_Edit_Mode := False;
            
               Gnav_Info.User_Tm := New_Mark;
               
               Utility.Storage.Set_Item ("TM", Gnav_Info.User_Tm);
            
            end if;
            
            Refresh := True;
     
         end if;
         
      else
         
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
     
         elsif Btn_Squawk.Contains (X, Y) then
         
            Squawk_Edit_Mode := True;
            
            Btn_Ok.Set_Style (Button_Disabled);  
                  
            New_Squawk := Gnav_Info.User_Id;
         
         elsif Btn_Tailmark.Contains (X, Y) then
         
            Mark_Edit_Mode := True;
            
            Btn_Ok.Set_Style (Button_Disabled);  
                  
            New_Mark := Gnav_Info.User_Tm;
         
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
         
         elsif Btn_Resolution.Contains (X, Y) then
         
            Maps.Terrain.Set_High_Resolution (not Maps.Terrain.Get_High_Resolution);
         
         elsif Btn_Margin_Plus.Contains (X, Y) and Flight.Safety_Height <= 350.0 then
         
            Flight.Safety_Height := Flight.Safety_Height + 50.0;
         
            Btn_Margin.Set_Label (Float_Image (Flight.Safety_Height, 0));
      
         elsif Btn_Margin_Min.Contains (X, Y) and Flight.Safety_Height >= 200.0 then
         
            Flight.Safety_Height := Flight.Safety_Height - 50.0;
         
            Btn_Margin.Set_Label (Float_Image (Flight.Safety_Height, 0));
      
         elsif Btn_Utc_Plus.Contains (X, Y) and Time_Zone <  12 then

            Flight.Set_Time_Zone (Time_Zone + 1);

            Btn_Utc.Set_Label (Integer_Image (Flight.Get_Time_Zone));
         
            Utility.Storage.Set_Item ("UTC_OFFSET", Integer_Image (Flight.Get_Time_Zone));
         
         elsif Btn_Utc_Min.Contains (X, Y)  and Time_Zone > -12 then
         
            Flight.Set_Time_Zone (Time_Zone - 1);

            Btn_Utc.Set_Label (Integer_Image (Flight.Get_Time_Zone));
      
            Utility.Storage.Set_Item ("UTC_OFFSET", Integer_Image (Flight.Get_Time_Zone));
         
         elsif Btn_Bias_Plus.Contains (X, Y) and Gnav_Info.Altitude_Bias <  60.0 then

            New_Altitude_Bias := New_Altitude_Bias + 5.0;

            Btn_Bias.Set_Label (Float_Image (New_Altitude_Bias, 0));
         
            Utility.Storage.Set_Item ("BIAS", Float_Image (New_Altitude_Bias, 0));
         
         elsif Btn_Bias_Min.Contains (X, Y)  and Gnav_Info.Altitude_Bias > -60.0 then
         
            New_Altitude_Bias := New_Altitude_Bias - 5.0;

            Btn_Bias.Set_Label (Float_Image (New_Altitude_Bias, 0));
         
            Utility.Storage.Set_Item ("BIAS", Float_Image (New_Altitude_Bias, 0));
         
         end if;
         
         if New_Altitude_Bias /= Gnav_Info.Altitude_Bias then
            Btn_Bias.Set_Style (Button_Cancel);
         else
            Btn_Bias.Set_Style (Button_Alive);
         end if;
         
      end if;
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------        
     
     
end Display.Pages.System;
--------------------------------------------------------------------------------
