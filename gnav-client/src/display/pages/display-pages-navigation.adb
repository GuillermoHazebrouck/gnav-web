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
with Ada.Numerics.Elementary_Functions;
-- Gnav
with Display.Compass;
with Display.Panels.Gauges;
with Display.Panels.Vario;
with Flight;
with Flight.Aircraft;
with Flight.Plan;
with Flight.Register;
with Flight.Representation;
with Flight.Traffic;
with Glex;
with Glex.Basic;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Gnav_Info;
with Math.Vector2;
with Maps;
use  Maps;
with Maps.Terrain;
with Maps.Layers;
with Maps.Airspaces;
with Maps.Airspaces.Viewer;
with Maps.Airspaces.Monitor;
with Maps.Reference;
with Timing.Events;
with Utility.Strings;
with Utility.Units;
use  Utility.Units;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Navigation is
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The map view
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   View : Maps.Map_View_Record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The local list of sectors
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Sector_List_Viewer is new Maps.Airspaces.Viewer (Number_Of_Rows    => 1,
                                                            Number_Of_Columns => 4,
                                                            Number_Of_Pages   => 3);
   
   Sector_List : Sector_List_Viewer.List_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The local list of sectors
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Selected_Reference : Maps.Reference.Reference_Info;
   
   -- Main function buttons
   ---------------------------------
   
   Frm_Functions : Widget_Record;
   
   Btn_Zoom_In   : Button_Record;
     
   Btn_Zoom_Out  : Button_Record;
  
   Btn_Center    : Button_Record;
    
   -- Range function buttons
   ---------------------------------
   
   Btn_Range      : Button_Record;
      
   Btn_Range_Mode : Button_Record;
      
   Btn_Cone_Mode  : Button_Record;
        
   -- Mac Cready adjustment buttons
   ---------------------------------
   
   Pnl_Wind        : Panel_Record;
   
   Pnl_Mc_Cready   : Panel_Record;
   
   Btn_Sink_Plus   : Button_Record; -- Inter thermal sink (+)
     
   Btn_Sink_Min    : Button_Record; -- Inter thermal sink (-)
    
   -- Status variables
   ---------------------------------
   
   Auto_Center   : Boolean := True;
   
   Frame_SW      : Position_Record := No_Position_Record;
      
   Frame_NE      : Position_Record := No_Position_Record;
   
   Altitude_Unit : Altitude_Units  := Unit_Meter;
   
   Distance_Unit : Distance_Units  := Unit_Kilometer;
   
   Velocity_Unit : Velocity_Units  := Unit_Kilometer_Hour;
   
   -- Function flags
   ---------------------------------
   
   Show_Sector_List : Boolean := False;
   
   Show_Vario_Panel : Boolean := False;
   
   -- Fonts
   ---------------------------------
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.012, 
                                             Height    => 0.036, 
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);
      
   -- Fonts
   ---------------------------------
   Font_2 : Glex.Fonts.Font_Style_Record := (Width     => 0.010, 
                                             Height    => 0.033, 
                                             Space     => 0.004,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);
         
   -- Fonts
   ---------------------------------
   Font_3 : Glex.Fonts.Font_Style_Record := (Width     => 0.012, 
                                             Height    => 0.035, 
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);
            
   -- Fonts
   ---------------------------------
   Font_4 : Glex.Fonts.Font_Style_Record := (Width     => 0.020,
                                             Height    => 0.035, 
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);
                
   -- Fonts
   ---------------------------------
   Font_5 : Glex.Fonts.Font_Style_Record := (Width     => 0.010, 
                                             Height    => 0.032, 
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);
            
   -- Wind rosetta
   ---------------------------------
   Wind_Arrow : Glex.Basic.Resource_Type;
     
   --===========================================================================
   -- Loads the wind rosetta buffer
   --===========================================================================
   procedure Load_Wind_Arrow is
      
      Arrow_Buffer  : Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (4);

   begin

      Arrow_Buffer.Load_Node ( 0.14, 0.00);
      Arrow_Buffer.Load_Node (-0.14, 0.10);
      Arrow_Buffer.Load_Node (-0.06, 0.00);
      Arrow_Buffer.Load_Node (-0.14,-0.10);

      Wind_Arrow.Load (Arrow_Buffer);

   end Load_Wind_Arrow;
   -----------------------------------------------------------------------------
        



   --===========================================================================
   -- Draw wind
   --===========================================================================
   procedure Draw_Wind is

      use Glex.Basic;
      use Ada.Numerics.Elementary_Functions;
      use Glex.Fonts;
         
      X      : Float := Pnl_Wind.Get_Allocation.X + 0.50 * Pnl_Wind.Get_Allocation.W;      
      Y      : Float := Pnl_Wind.Get_Allocation.Y + 0.50 * Pnl_Wind.Get_Allocation.H;      
      Size   : Float := 0.18;
      
      Aspect : Float := Width / Height;
      
      Matrix : Glex.Transform_Record := Glex.Get_Transform.all;

      Angle  : Float;

      Wind   : Float := Convert (Float (Flight.Data.Wind.Norm2), 
                                 Unit_Meter_Second,
                                 Unit_Kilometer_Hour);
      
   begin

      if Flight.Data.Is_Valid (Flight.Field_Wind) and then Wind > 0.0 then
         
         Angle := Float (Flight.Data.Wind.Orientation * 180.0 / Math.Pi);

         if Angle < 0.0 then

            Angle := 360.0 + Angle;

         end if;

         Angle := Angle * Float (Math.Pi / 180.0);
         
         X := Pnl_Wind.Get_Allocation.X + 0.66 * Pnl_Wind.Get_Allocation.W;
         
         Glex.Get_Transform.Translate (X, Y);

         Glex.Get_Transform.Scale     (Size, Size * Aspect);

         Glex.Get_Transform.Rotate    (Angle);

         Wind_Arrow.Draw (Color_Sky, Triangle_Fan);
         
         Glex.Get_Transform.Copy (Matrix);
         
         X := Pnl_Wind.Get_Allocation.X + 0.23 * Pnl_Wind.Get_Allocation.W;
         Y := Pnl_Wind.Get_Allocation.Y + 0.40 * Pnl_Wind.Get_Allocation.H;
         
         Glex.Fonts.Draw (Text      => Utility.Strings.Float_Image (Wind, 0),
                          X         => X, -- - 0.04 * Cos (Angle),
                          Y         => Y, -- - 0.04 * Sin (Angle) * Aspect,
                          Style     => Font_1,
                          Color     => Line_Cyan,
                          Alignment => Glex.Fonts.Alignment_CC);
         
      else
         
         Glex.Fonts.Draw (Text      => "NO WIND",
                          X         => X,
                          Y         => Y,
                          Style     => Font_5,
                          Color     => Line_Cyan,
                          Alignment => Glex.Fonts.Alignment_CC);
                  
      end if;  
           
   end Draw_Wind;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Center_Frame is
   begin

      Frame_SW := View.Screen_To_Position (View.X + 0.25 * View.W,
                                           View.Y + 0.25 * View.H);
      
      Frame_NE := View.Screen_To_Position (View.X + 0.75 * View.W,
                                           View.Y + 0.75 * View.H);
      
   end Center_Frame;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Update_View is
      
      use Math.Vector2;
      
   begin
      
      -- Update center of screen
      -------------------------------------------------
      
      if Auto_Center then
         
         if 
           Flight.Data.Position.Lat < Frame_SW.Lat or else
           Flight.Data.Position.Lon < Frame_SW.Lon or else
           Flight.Data.Position.Lat > Frame_NE.Lat or else
           Flight.Data.Position.Lon > Frame_NE.Lon
         then
            
            View.Center := Flight.Data.Position;
            
            Center_Frame;
            
         end if;
                  
      end if;

   end Update_View;
   -----------------------------------------------------------------------------	
              
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Update_Range_Buttons is
      
      use Flight.Aircraft;
      
   begin
      
      if View.Show_Terrain then
         
         if View.Cone_Active then
            
            Btn_Range.Set_Label ("R");
            
            Btn_Range.Set_Background_Color (Color_Pink);
            
            Btn_Range.Set_Label_Color      (Line_Red);
         
            if Get_Range_Mode = Range_Straight then
            
               Btn_Range_Mode.Set_Label ("AHEAD");
            
            else
            
               Btn_Range_Mode.Set_Label ("LOCAL");
            
               if Get_Cone_Mode = Cone_10_To_1 then
               
                  Btn_Cone_Mode.Set_Label ("10:1");
            
               else               
                  Btn_Cone_Mode.Set_Label ("OPTIMAL");
            
               end if;
            
            end if;
         
         else            
            Btn_Range.Set_Label ("T");
            
            Btn_Range.Set_Background_Color (Color_Green);
            
            Btn_Range.Set_Label_Color      (Line_Grass);
         
         end if;
         
      else
         Btn_Range.Set_Label ("S");
            
         Btn_Range.Set_Background_Color (Color_Cyan);
            
         Btn_Range.Set_Label_Color      (Line_Cyan);
         
      end if;
      
      Btn_Range_Mode.Set_Visible (View.Cone_Active);
      
      Btn_Cone_Mode.Set_Visible  (View.Cone_Active and then Get_Range_Mode = Range_Local);
      
   end Update_Range_Buttons; 
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
        
      use Utility.Strings;
      
      Allocation : Allocation_Record;
      
   begin

      -- Functions panel
      ------------------------------------------------------
      
      Frm_Functions.Set_Allocation ((0.0,0.0,0.16,1.0));
        
      Frm_Functions.Set_Background_Color (Color_Gray_2);
      
      Frm_Functions.Set_Border_Color (Color_Black);
            
      -- On-demand local sector list
      ------------------------------------------------------  
                 
      Allocation.X := 0.170;      
      Allocation.Y := 0.020;      
      Allocation.H := 0.250;          
      Allocation.W := 0.600;
           
      Sector_List.Set_Allocation (Allocation);
      
      -- Zoom out button
      ------------------------------------------------------   
      
      Allocation.X := 0.005;      
      Allocation.Y := 0.010;      
      Allocation.H := 0.110;          
      Allocation.W := 0.072;
           
      Btn_Zoom_Out.Set_Allocation (Allocation);
      
      Btn_Zoom_Out.Set_Label ("-");
      
      Btn_Zoom_Out.Set_Style (Button_Normal);
      
      Btn_Zoom_Out.Set_Font_Size (0.4, 0.5);
                       
      -- Zoom in button
      ------------------------------------------------------
      
      Allocation.Y := 2.0 * Allocation.Y + Allocation.H;  
      
      Btn_Zoom_In.Set_Allocation (Allocation);
      
      Btn_Zoom_In.Set_Label ("+");
      
      Btn_Zoom_In.Set_Style (Button_Normal);
      
      Btn_Zoom_In.Set_Font_Size (0.4, 0.5);
                        
      -- Center/follow view button
      ------------------------------------------------------
      
      Allocation := Btn_Zoom_In.Get_Allocation;
      
      Allocation.X := Allocation.X + Allocation.W + 0.005;
      
      Btn_Center.Set_Allocation (Allocation);
      
      Btn_Center.Set_Label (">X<");
      
      Btn_Center.Set_Style (Button_Normal);
      
      Btn_Center.Set_Font_Size (0.3, 0.3, 0.5);
                     
      -- Range button
      ------------------------------------------------------  
           
      Allocation := Btn_Zoom_Out.Get_Allocation;
      
      Allocation.X := Btn_Center.Get_Allocation.X;
      
      Btn_Range.Set_Allocation (Allocation);
      
      Btn_Range.Set_Label ("-");
      
      Btn_Range.Set_Style (Button_Normal);
      
      Btn_Range.Set_Font_Size (0.3, 0.3, 0.5);
                
      -- Range mode button
      ------------------------------------------------------  
     
      Allocation.X := Frm_Functions.Get_Allocation.X + Frm_Functions.Get_Allocation.W + 0.005;      
      Allocation.H := 0.7 * Btn_Range.Get_Allocation.H;
      Allocation.Y := Btn_Range.Get_Allocation.Y;      
      Allocation.W := 0.100;
           
      Btn_Range_Mode.Set_Allocation (Allocation);
      
      Btn_Range_Mode.Set_Label ("---");
      
      Btn_Range_Mode.Set_Label_Color (Color_Red);
      
      Btn_Range_Mode.Set_Font_Size (0.36, 0.3, 0.5);
                             
      Btn_Range_Mode.Set_Background_Color (Color_Pink);
              
      --
      
      Allocation.X := Allocation.X + Allocation.W + 0.005;
           
      Btn_Cone_Mode.Set_Allocation (Allocation);
      
      Btn_Cone_Mode.Set_Label ("---");
      
      Btn_Cone_Mode.Set_Label_Color (Color_Red);
      
      Btn_Cone_Mode.Set_Font_Size (0.36, 0.3, 0.5);
                             
      Btn_Cone_Mode.Set_Background_Color (Color_Pink);
        
      -- Mac Cready setup buttons
      ------------------------------------------------------  

      Allocation.X := Btn_Zoom_In.Get_Allocation.X;
      Allocation.Y := Btn_Zoom_In.Get_Allocation.Y + Btn_Zoom_In.Get_Allocation.H + 0.008;
      Allocation.W := Btn_Center.Get_Allocation.X + Btn_Center.Get_Allocation.W - Allocation.X;
      Allocation.H := 0.32;

      Pnl_Mc_Cready.Set_Allocation (Allocation);

      Pnl_Mc_Cready.Set_Background_Color (Color_Gray_7);

      Pnl_Mc_Cready.Set_Show_Border (True);
      
      Pnl_Mc_Cready.Set_Label ("M/C", Label_Left);

      Pnl_Mc_Cready.Set_Font_Size (0.03, 0.25);

      Pnl_Mc_Cready.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));
      
      --
      
      Allocation := Btn_Zoom_In.Get_Allocation;
      
      Allocation.X := Pnl_Mc_Cready.Get_Allocation.X + 0.005;
      Allocation.Y := Pnl_Mc_Cready.Get_Allocation.Y + Pnl_Mc_Cready.Get_Allocation.H - Allocation.H - 0.08;
      Allocation.W :=(Pnl_Mc_Cready.Get_Allocation.W - 0.015) * 0.5;
      
      Btn_Sink_Min.Set_Label ("{");
      
      Btn_Sink_Min.Set_Allocation (Allocation);
      
      Btn_Sink_Min.Set_Font_Size (0.4, 0.5);
      
      Btn_Sink_Min.Set_Style (Button_Normal);
                        
      Allocation.X := Allocation.X + Allocation.W + 0.005;      
           
      Btn_Sink_Plus.Set_Label ("}");
      
      Btn_Sink_Plus.Set_Allocation (Allocation);
      
      Btn_Sink_Plus.Set_Style (Button_Normal);
                 
      Btn_Sink_Plus.Set_Font_Size (0.4, 0.5);
                    
      -- Wind speed and direction panel
      ------------------------------------------------------
      
      Allocation.X := 0.005;
      Allocation.Y := Pnl_Mc_Cready.Get_Allocation.Y + Pnl_Mc_Cready.Get_Allocation.H + 0.028;
      Allocation.W := Frm_Functions.Get_Allocation.W - 0.010;
      Allocation.H := 0.13;

      Pnl_Wind.Set_Allocation (Allocation);

      Pnl_Wind.Set_Background_Color (Color_Gray_7);

      Pnl_Wind.Set_Show_Border (True);
      
      Pnl_Wind.Set_Label ("WND", Label_Left);

      Pnl_Wind.Set_Font_Size (0.03, 0.25);

      Pnl_Wind.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));
      
      -- Flight data panel
      ------------------------------------------------------  
      
      Display.Panels.Gauges.Initialize;
            
      -- Vario panel
      ------------------------------------------------------  
      
      Allocation.X := 0.16;
      Allocation.H := 0.35;
      Allocation.Y := 0.00;
      Allocation.W := 0.65;

      Display.Panels.Vario.Initialize (Allocation);
             
      -- Setup the view based on the loaded terrain
      --------------------------------------------------------------------------
      
      View.Center := Flight.Data.Position;
      
      View.Zoom   := Lower_Zoom;
      
      View.X := 0.160;
      
      View.Y := 0.000;
      
      View.W := 0.655;
      
      View.H := 1.000;
      
      View.Show_Terrain := True;
      
      Center_Frame;
      
      -- Route and trajectory
      --------------------------------------------------------------------------
      
      Flight.Representation.Initialize;
      
      -- View update
      --------------------------------------------------------------------------
      
      Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                    Callback => Update_View'Access);
                      
      -- Wind arrow
      --------------------------------------------------------------------------
      
      Load_Wind_Arrow;
      
      -- Sync range buttons
      --------------------------------------------------------------------------
      
      Update_Range_Buttons;
            
   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
      
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Utility.Strings;
            
      X     : Float := 0.0;
      Y     : Float := 0.0;
      Color : Line_Color_Record;

   begin
      
      -- Map
      --------------------------------------------------------------------------
      
      Maps.Terrain.Draw (View);
      
      Maps.Layers.Draw (View);
      
      Maps.Airspaces.Draw (View);
      
      Maps.Reference.Draw (View);
      
      Maps.Airspaces.Monitor.Draw_Notifications;
      
      -- Flight representation
      --------------------------------------------------------------------------
      
      Flight.Representation.Draw (View);
      
      -- Flight data
      --------------------------------------------------------------------------
      
      if Show_Vario_Panel then
         
         Display.Panels.Vario.Draw;
      
      end if;
         
      Display.Panels.Gauges.Draw_Altimeter (X => 0.766,
                                            Y => 0.500,
                                            H => 0.950);
      
      Display.Panels.Gauges.Draw;
      
      Frm_Functions.Draw;
      
      Btn_Range.Draw;
      
      Btn_Range_Mode.Draw;
  
      Btn_Cone_Mode.Draw;
         
      -- Map moving buttons
      --------------------------------------------------------------------------
      
      Btn_Center.Draw;
      
      Btn_Zoom_In.Draw;
      
      Btn_Zoom_Out.Draw;
      
      -- MacCready setup panel
      --------------------------------------------------------------------------
      
      Pnl_Mc_Cready.Draw;
      
      Btn_Sink_Plus.Draw;
      
      if Flight.Register.Sink_Warning_Active then         
         Btn_Sink_Min.Set_Style (Button_Enabled);
      else
         Btn_Sink_Min.Set_Style (Button_Normal);
      end if;
      
      Btn_Sink_Min.Draw;
      
      if Flight.Aircraft.Get_Sink > 0.0 then         
         Color := Line_Grass;
      else         
         Color := Line_Red;         
      end if;
      
      X := Pnl_Mc_Cready.Get_Allocation.X + 0.66 * Pnl_Mc_Cready.Get_Allocation.W;
      Y := Btn_Sink_Plus.Get_Allocation.Y + Btn_Sink_Plus.Get_Allocation.H + 0.022;
        
      Glex.Fonts.Draw (Text      => Utility.Strings.Float_Image (Flight.Aircraft.Get_Sink, 1),
                       X         => X,
                       Y         => Y,
                       Style     => Font_1,
                       Color     => Color,
                       Alignment => Glex.Fonts.Alignment_LC);

      -- Necessary altitude to reach waypoint
      --------------------------------------------------------------------------
      
      X := Btn_Zoom_In.Get_Allocation.X + Btn_Zoom_In.Get_Allocation.W * 0.150;
      Y := Btn_Zoom_In.Get_Allocation.Y + Btn_Zoom_In.Get_Allocation.H + 0.022;
      
      -- TODO: manage units to synchronize with gauges panel (m <-> FT)
 
      Glex.Fonts.Draw (Text      => "\",
                       X         => X,
                       Y         => Y,
                       Style     => Font_4,
                       Color     => Line_Magenta,
                       Alignment => Glex.Fonts.Alignment_LL);
      
      X := X + Font_4.Width + 3.0 * Font_4.Space;
      
      if Flight.Data.Is_Recent (Flight.Field_Position) then
         
         Glex.Fonts.Draw (Text      => Flight.Plan.Next_Waypoint.Get_Required_Altitude,
                          X         => X,
                          Y         => Y,
                          Style     => Font_3,
                          Color     => Line_Magenta,
                          Alignment => Glex.Fonts.Alignment_LL);
                
      else
         
         Glex.Fonts.Draw (Text      => "---",
                          X         => X,
                          Y         => Y,
                          Style     => Font_1, 
                          Color     => Line_Red,
                          Alignment => Glex.Fonts.Alignment_LL);         
         
      end if;
         
      -- Optimal airspeed to fly in current direction or in the route direction
      --------------------------------------------------------------------------

      X := Btn_Zoom_In.Get_Allocation.X + Btn_Zoom_In.Get_Allocation.W * 0.150;
      Y := Y + 0.060;
      
      Glex.Fonts.Draw (Text      => ">",
                       X         => X,
                       Y         => Y,
                       Style     => Font_4,
                       Color     => Line_White,
                       Alignment => Glex.Fonts.Alignment_LL);
       
      X := X + Font_4.Width + 3.0 * Font_4.Space;
      
      if Flight.Data.Is_Recent (Flight.Field_Course) then
         
         declare
            Speed : Float := Flight.Aircraft.Get_Optimal_Speed;
         begin
         
            Glex.Fonts.Draw (Text      => Float_Image (3.6 * Speed, 0),
                             X         => X,
                             Y         => Y,
                             Style     => Font_3,
                             Color     => Line_White, 
                             Alignment => Glex.Fonts.Alignment_LL);
            
         end;
         
      elsif Flight.Data.Is_Recent (Flight.Field_Position) then
                  
         Glex.Fonts.Draw (Text      => Flight.Plan.Next_Waypoint.Get_Optimal_Speed,
                          X         => X,
                          Y         => Y,
                          Style     => Font_3,
                          Color     => Line_Magenta, 
                          Alignment => Glex.Fonts.Alignment_LL);
         
      else
         
         Glex.Fonts.Draw (Text      => "---",
                          X         => X,
                          Y         => Y,
                          Style     => Font_1, 
                          Color     => Line_Red,
                          Alignment => Glex.Fonts.Alignment_LL);         
         
      end if;
         
      -- Wind speed and direction
      --------------------------------------------------------------------------

      Pnl_Wind.Draw;
      
      Draw_Wind;
      
      X := Frm_Functions.Get_Allocation.X + Frm_Functions.Get_Allocation.W + 0.008;
      Y := 0.94;
      
      -- Show automatic position reporting system (APRS) warning
      --------------------------------------------------------------------------

      declare
         use Flight.Traffic;
      begin
                  
         if Get_Status /= Aprs_Disabled then
         
            case Get_Status is         
               when Aprs_Not_Receiving => Color := Line_Red;
               when Aprs_Not_Reporting => Color := Line_Yellow;
               when Aprs_Nominal       => Color := Line_Grass;
               when others             => null;
            end case;
         
            Glex.Fonts.Draw (Text      => "APRS",
                             X         => X,
                             Y         => Y,
                             Style     => Font_5,
                             Color     => Color,
                             Alignment => Glex.Fonts.Alignment_LL);
               
            Y := Y - 1.8 * Font_5.Height;
            
         end if;
         
      end;
      
      -- Show simulation mode
      --------------------------------------------------------------------------

      if Gnav_Info.Simulation_Mode then
             
         Glex.Fonts.Draw (Text      => "SIMU",
                          X         => X,
                          Y         => Y,
                          Style     => Font_5,
                          Color     => Line_Yellow,
                          Alignment => Glex.Fonts.Alignment_LL);
            
         Y := Y - 1.8 * Font_5.Height;
            
      end if;
      
      -- Show blinking GNSS flag when no fix
      --------------------------------------------------------------------------

      if not Flight.Data.Is_Recent (Flight.Field_Position, Lapse => 4.0) then
             
         Glex.Fonts.Draw (Text      => "GNSS",
                          X         => X,
                          Y         => Y,
                          Style     => Font_5,
                          Color     => Line_Red,
                          Alignment => Glex.Fonts.Alignment_LL);
            
      end if;

      if Show_Sector_List then
                  
         Sector_List.Draw_List (Flight.Data.Position, Blink);
      
      elsif Selected_Reference.Is_Valid then
         
         Selected_Reference.Draw;
         
      end if;
         
   end Draw;
   -----------------------------------------------------------------------------
      



   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
      
      use Flight.Aircraft;
      use Math.Vector2;
      use Maps.Terrain;
      use Utility.Strings;
      
      Changed : Boolean := True; 
      
   begin
      
      if 
        Show_Sector_List and then 
        Sector_List.Contains (X, Y)
      then
         
         Sector_List.Focus_Item (X, Y);
       
      elsif 
        Selected_Reference.Contains (X, Y)
      then
         
         Selected_Reference.Clear;
        
      elsif
        Btn_Center.Contains (X, Y)
      then
         
         if not Auto_Center then
               
            View.Center := Flight.Data.Position;
         
            Auto_Center := True;
            
         end if;
         
      elsif
        Btn_Range.Contains (X, Y)
      then
         
         if not View.Show_Terrain then
            
            View.Show_Terrain := True;
            View.Cone_Active  := False;
            
         else
            
            if not View.Cone_Active then
               
               View.Cone_Active  := True;
                              
            else               
               View.Show_Terrain := False;
               View.Cone_Active  := False;
                        
            end if;
               
         end if;
         
         Update_Range_Buttons;
         
      elsif
        View.Show_Terrain and then
        View.Cone_Active  and then
        Btn_Range_Mode.Contains (X, Y)
      then
         
         if Get_Range_Mode = Range_Straight then
            Flight.Aircraft.Set_Reference  (Flight.Plan.Home_Waypoint.Position);
            Flight.Aircraft.Set_Range_Mode (Range_Local);
         else
            Flight.Aircraft.Set_Range_Mode (Range_Straight);
         end if;
         
         Update_Range_Buttons;
         
      elsif
        View.Show_Terrain and then
        View.Cone_Active  and then
        Btn_Cone_Mode.Contains (X, Y)
      then
         
         if Get_Cone_Mode = Cone_Optimal then
            Flight.Aircraft.Set_Cone_Mode (Cone_10_To_1);
         else
            Flight.Aircraft.Set_Cone_Mode (Cone_Optimal);
         end if;
                 
         Update_Range_Buttons;
         
      elsif
        Btn_Sink_Plus.Contains (X, Y)
      then
         
         Flight.Aircraft.Set_Sink (Flight.Aircraft.Get_Sink + 0.1);
                  
      elsif
        Btn_Sink_Min.Contains (X, Y)
      then
          
         Flight.Aircraft.Set_Sink (Flight.Aircraft.Get_Sink - 0.1);
         
      elsif 
        Btn_Zoom_In.Contains (X, Y) 
      then
         
         View.Zoom_In;
         
         Center_Frame;
         
      elsif 
        Btn_Zoom_Out.Contains (X, Y) 
      then
         
         View.Zoom_Out;
         
         Center_Frame;
        
      elsif X in 0.17..0.77 then -- NOTE: On_Clip uses a wider area that overlaps components
         
         declare
            Position : Position_Record := View.Screen_To_Position (X, Y);
         begin
            
            Selected_Reference := Maps.Reference.Select_Reference (Position);
            
            if Selected_Reference.Is_Valid then
               
               Sector_List.Clear;
               
            else
               
               Sector_List.Build_List (View, Position, True);
                   
            end if;
            
            Show_Sector_List := not Sector_List.Is_Empty;
                       
         end;
                        
      elsif 
        Display.Panels.Gauges.Altitude_Pressed (X, Y)
      then
         
         Show_Vario_Panel := not Show_Vario_Panel;
         
         if Show_Vario_Panel then 
            View.Y := 0.35;
            View.H := 0.65;
         else
            View.Y := 0.00;
            View.H := 1.00;
         end if;
         
         Changed := True;
         
      else
         
         Changed := False;
         
         Display.Panels.Gauges.Screen_Pressed (X ,Y);
            
      end if;
      
      if Changed then 
         
         Display.Refresh := True;
      
      end if;
               
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Move_Step (X, Y, Dx, Dy : Float) is
   begin
      
      if Show_Sector_List and Sector_List.Contains (X, Y) then
        
         if abs Dx > abs Dy then
                        
            -- Swipe right or left will change the page
            ------------------------------------------------------------
            if Dx < 0.0 then 
               Sector_List.Next_Page;
            else
               Sector_List.Previous_Page;
            end if;
            
         elsif Dy < 0.0 then
            
            -- Swipe down on the list will close it and save the changes
            ------------------------------------------------------------
            Show_Sector_List := False;
            Maps.Airspaces.Save_Configuration_If_Changed;
            Maps.Airspaces.Clear_Focus;
            
         end if;
         
      elsif View.On_Clip (X, Y) then
         
         View.Center.Lon := View.Center.Lon - Long_Float (Dx * View.Zoom * Display.Aspect);
         View.Center.Lat := View.Center.Lat - Long_Float (Dy * View.Zoom * Maps.Get_Shrinkage);
      
         Auto_Center := False;
         Refresh     := True;

      end if;
      
   end Screen_Move_Step;
   -----------------------------------------------------------------------------
      
   
   
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reference_X,
   Reference_Y : Float := 0.0;
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean) is
      Delta_X, 
      Delta_Y,
      Delta_W : Float;
   begin
      
      if First then
         
         Reference_X := X;
         Reference_Y := Y;
         
      else
         
         Delta_X := X + Dx - Reference_X;
         Delta_Y := Y + Dy - Reference_Y;
         Delta_W := Delta_X * Display.Aspect;

         if Delta_W * Delta_W + Delta_Y * Delta_Y > 0.02 then
            
            Screen_Move_Step (X, Y, Delta_X, Delta_Y);
            
            Reference_X := X + Dx;
            Reference_Y := Y + Dy;
            
         end if;
         
      end if;

   end Screen_Move;
   -----------------------------------------------------------------------------
      
   
   
   
   --===========================================================================
   --
   --===========================================================================
   function Get_Map_View return Maps.Map_View_Record is
   begin
      
      return View;
      
   end Get_Map_View;
   -----------------------------------------------------------------------------
   
end Display.Pages.Navigation;
--------------------------------------------------------------------------------
