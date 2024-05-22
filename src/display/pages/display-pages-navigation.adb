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
with Flight;
with Flight.Aircraft;
with Flight.Plan;
with Flight.Representation;
with Flight.Traffic;
--with Flight.Stream;
with Glex;
with Glex.Basic;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Math.Vector2;
with Maps;
use  Maps;
with Maps.Terrain;
with Maps.Layers;
with Maps.Airspaces;
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

   -- Main function buttons
   ---------------------------------
   
   Frm_Functions : Widget_Record;
   
   Btn_Zoom_In   : Button_Record;
     
   Btn_Zoom_Out  : Button_Record;
   
   Btn_Left      : Button_Record;
   
   Btn_Right     : Button_Record;
   
   Btn_Down      : Button_Record;
   
   Btn_Up        : Button_Record;
       
   Btn_Move      : Button_Record;
    
   Btn_Center    : Button_Record;
    
   -- Range function buttons
   ---------------------------------
   
   Btn_Range      : Button_Record;
      
   Btn_Range_Mode : Button_Record;
      
   Btn_Cone_Mode  : Button_Record;
        
   -- Mac Cready adjustment buttons
   ---------------------------------
   
   Btn_Ascent_Plus  : Button_Record; -- Inter thermal sink/lift (+)
     
   Btn_Ascent_Min   : Button_Record; -- Inter thermal sink/lift (-)
    
   -- Status variables
   ---------------------------------
   
   Auto_Center   : Boolean := True;
   
   Show_Move     : Boolean := False;
   
   Frame_SW      : Position_Record := No_Position_Record;
      
   Frame_NE      : Position_Record := No_Position_Record;
   
   Altitude_Unit : Altitude_Units := Unit_Meter;
   
   Distance_Unit : Distance_Units := Unit_Kilometer;
   
   Velocity_Unit : Velocity_Units := Unit_Kilometer_Hour;
   
   -- Fonts
   ---------------------------------
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.010, 
                                             Height    => 0.033, 
                                             Space     => 0.004,
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
   Font_5 : Glex.Fonts.Font_Style_Record := (Width     => 0.012, 
                                             Height    => 0.035, 
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

      Arrow_Buffer.Load_Node ( 0.25, 0.00);
      Arrow_Buffer.Load_Node (-0.08, 0.12);
      Arrow_Buffer.Load_Node ( 0.00, 0.00);
      Arrow_Buffer.Load_Node (-0.08,-0.12);

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
         
      X      : Float := 0.25;
      
      Y      : Float := 0.88;
      
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

         Glex.Get_Transform.Translate (X, Y);

         Glex.Get_Transform.Scale     (Size, Size * Aspect);

         Glex.Get_Transform.Rotate    (Angle);

         Wind_Arrow.Draw (Color_Sky, Triangle_Fan);
         
         Glex.Get_Transform.Copy (Matrix);
           
         Glex.Fonts.Draw (Text      => Utility.Strings.Float_Image (Wind, 0),
                          X         => X - 0.04 * Cos (Angle),
                          Y         => Y - 0.04 * Sin (Angle) * Aspect,
                          Style     => Font_5,
                          Color     => Line_Cyan,
                          Alignment => Glex.Fonts.Alignment_CC);
         
      else
         
         Y := 0.95;
         
         Glex.Fonts.Draw (Text      => "NO WIND",
                          X         => X,
                          Y         => Y,
                          Style     => Font_5,
                          Color     => Line_Cyan,
                          Alignment => Glex.Fonts.Alignment_TC);
                  
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
      
      -- Update cone
      -------------------------------------------------
      
      --if View.Cone_Active and Flight.Aircraft.Cone_Changed then
         
      --   Maps.Terrain.Force_Reload;
         
      --end if;
      
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
            
            Btn_Range.Set_Label ("-R>");
            
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
            Btn_Range.Set_Label ("-T>");
            
            Btn_Range.Set_Background_Color (Color_Green);
            
            Btn_Range.Set_Label_Color      (Line_Grass);
         
         end if;
         
      else
         Btn_Range.Set_Label ("-S>");
            
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
      
      M : Dimension_Float := 0.01;
      H : Dimension_Float := 0.08;
      W : Dimension_Float := 0.05;
      V : Dimension_Float := 0.04;
      
      Allocation : Allocation_Record;
      
   begin

      M := 0.01;
      H := 0.10;
      W := 0.06;
      
      -- Functions panel
      ------------------------------------------------------
      
      Frm_Functions.Set_Allocation ((0.0,0.0,0.16,1.0));
        
      Frm_Functions.Set_Background_Color (Color_Gray_2);
      
      Frm_Functions.Set_Border_Color (Color_Black);
      
      -- Down button
      ------------------------------------------------------
      
      Allocation.H := H;      
      Allocation.W := W;             
      Allocation.X := 0.4 + 0.5 * W;      
      Allocation.Y := M;
      
      Btn_Down.Set_Label ("{");
      
      Btn_Down.Set_Allocation (Allocation);
      
      Btn_Down.Set_Style (Button_Action);
      
      Btn_Down.Set_Font_Size (0.5, 0.5);
                   
      -- Up button
      ------------------------------------------------------
      
      Allocation.Y := 1.0 - H - M;
      
      Btn_Up.Set_Label ("}");
      
      Btn_Up.Set_Allocation (Allocation);
      
      Btn_Up.Set_Style (Button_Action);
      
      Btn_Up.Set_Font_Size (0.5, 0.5);
                             
      -- Left button
      ------------------------------------------------------
      
      Allocation.X := Allocation.X - 0.22 - 0.5 * W;      
      Allocation.Y := 0.5 - 0.5 * H;
               
      Btn_Left.Set_Label ("<");
      
      Btn_Left.Set_Allocation (Allocation);
      
      Btn_Left.Set_Style (Button_Action);
      
      Btn_Left.Set_Font_Size (0.5, 0.5);
                        
      -- Right button
      ------------------------------------------------------
      
      Allocation.X := 0.7;      
      Allocation.Y := 0.5 - 0.5 * H;
      
      Btn_Right.Set_Label (">");
      
      Btn_Right.Set_Allocation (Allocation);
      
      Btn_Right.Set_Style (Button_Action);
      
      Btn_Right.Set_Font_Size (0.5, 0.5);
             
      -- Zoom out button
      ------------------------------------------------------   
    
      Allocation.X := Btn_Right.Get_Allocation.X;      
      Allocation.Y := Btn_Right.Get_Allocation.Y - H - 2.0 * M;      
      Allocation.H := H;          
      Allocation.W := W;
           
      Btn_Zoom_Out.Set_Label ("-");
      
      Btn_Zoom_Out.Set_Allocation (Allocation);
      
      Btn_Zoom_Out.Set_Style (Button_Action);
      
      Btn_Zoom_Out.Set_Font_Size (0.6);
                       
      -- Zoom in button
      ------------------------------------------------------
      
      Btn_Zoom_In.Set_Label ("+");
      
      Allocation.Y := Btn_Right.Get_Allocation.Y + H + 2.0 * M;  
      
      Btn_Zoom_In.Set_Allocation (Allocation);
      
      Btn_Zoom_In.Set_Style (Button_Action);
      
      Btn_Zoom_In.Set_Font_Size (0.5);
                       
      -- Flight data panel
      ------------------------------------------------------  
      
      Display.Panels.Gauges.Initialize;
      
      -- Range button
      ------------------------------------------------------  
      
      M := 0.01;
      H := (1.0 - 9.0 * M) / 8.0;
      W := 0.15;
                      
      -- Range mode button
      ------------------------------------------------------  
      
      Allocation.X := 2.0 * M + W;      
      Allocation.H := 0.7 * H;      
      Allocation.Y := M;      
      Allocation.W := 0.8 * W;
           
      Btn_Range_Mode.Set_Label ("---");
      
      Btn_Range_Mode.Set_Allocation (Allocation);
      
      Btn_Range_Mode.Set_Label_Color (Color_Red);
      
      Btn_Range_Mode.Set_Font_Size (0.4, 0.3, 0.5);
                             
      Btn_Range_Mode.Set_Background_Color (Color_Pink);
               
      Allocation.X := 3.0 * M + 1.8 * W;      
      Allocation.H := 0.7 * H;      
      Allocation.Y := M;      
      Allocation.W := 0.8 * W;
           
      Btn_Cone_Mode.Set_Label ("---");
      
      Btn_Cone_Mode.Set_Allocation (Allocation);
      
      Btn_Cone_Mode.Set_Label_Color (Color_Red);
      
      Btn_Cone_Mode.Set_Font_Size (0.4, 0.3, 0.5);
                             
      Btn_Cone_Mode.Set_Background_Color (Color_Pink);
             
      -- Mac Cready setup buttons
      ------------------------------------------------------  
          
      Allocation.X := 0.7;      
      Allocation.H := 0.7 * H;      
      Allocation.Y := M;      
      Allocation.W := 0.4 * W;
           
      Btn_Ascent_Min.Set_Label ("-");
      
      Btn_Ascent_Min.Set_Allocation (Allocation);
      
      Btn_Ascent_Min.Set_Label_Color (Line_White);
      
      Btn_Ascent_Min.Set_Font_Size (0.6, 0.6, 0.5);
                             
      Btn_Ascent_Min.Set_Background_Color (Color_Gray_5);
     
      Allocation.Y := 3.0 * M + 1.4 * H;      
           
      Btn_Ascent_Plus.Set_Label ("+");
      
      Btn_Ascent_Plus.Set_Allocation (Allocation);
      
      Btn_Ascent_Plus.Set_Label_Color (Line_White);
      
      Btn_Ascent_Plus.Set_Font_Size (0.6, 0.6, 0.5);
                             
      Btn_Ascent_Plus.Set_Background_Color (Color_Gray_5);
       
      -- Range button
      ------------------------------------------------------  
      
      Allocation.X := 0.005;      
      Allocation.H := H;      
      Allocation.Y := M;      
      Allocation.W := W;
           
      Btn_Range.Set_Label ("RANGE");
      
      Btn_Range.Set_Allocation (Allocation);
      
      Btn_Range.Set_Label_Color (Line_White);
      
      Btn_Range.Set_Font_Size (0.3, 0.3, 0.5);
                             
      Btn_Range.Set_Background_Color (Color_Gray_5);

      -- Move button
      ------------------------------------------------------
      
      Allocation.Y := Allocation.Y + H + M;
      
      Btn_Move.Set_Label ("MOVE");
      
      Btn_Move.Set_Allocation (Allocation);
      
      Btn_Move.Set_Label_Color (Line_White);
      
      Btn_Move.Set_Font_Size (0.3, 0.3, 0.5);
                      
      Btn_Move.Set_Background_Color (Color_Gray_5);
                 
      -- Center/follow view button
      ------------------------------------------------------
      Btn_Center.Set_Label ("> X <");
      
      Allocation.Y := Allocation.Y + H + M;
      
      Btn_Center.Set_Allocation (Allocation);
            
      Btn_Center.Set_Background_Color (Color_Gray_5);
      
      Btn_Center.Set_Label_Color (Line_White);
      
      Btn_Center.Set_Font_Size (0.3, 0.3, 0.5);
            
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
      ------------------------------------------------------
      
      Load_Wind_Arrow;
      
      -- Sync range buttons
      ------------------------------------------------------
      
      Update_Range_Buttons;
            
   end Initialize;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is
      
      use Utility.Strings;
            
      X     : Float := 0.0;
      Color : Line_Color_Record;

   begin
      
      -- Map
      --------------------------------------------------------------------------
      
      Maps.Terrain.Draw (View);
      
      Maps.Layers.Draw (View);
      
      Maps.Airspaces.Draw (View);
      
      Maps.Reference.Draw (View);
      
      Maps.Airspaces.Draw_Labels (View);
      
      -- Flight representation
      --------------------------------------------------------------------------
      
      Flight.Representation.Draw (View);
      
      -- Flight data
      --------------------------------------------------------------------------
      
      Display.Panels.Gauges.Draw;
      
      Frm_Functions.Draw;
      
      Btn_Range.Draw;
      
      Btn_Range_Mode.Draw;
  
      Btn_Cone_Mode.Draw;
         
      -- Map moving buttons
      --------------------------------------------------------------------------
      
      Btn_Center.Draw;
      
      Btn_Move.Draw;
      
      if Show_Move then
         
         Btn_Down.Draw;
      
         Btn_Up.Draw;
      
         Btn_Left.Draw;
      
         Btn_Right.Draw;
      
         Btn_Zoom_In.Draw;
      
         Btn_Zoom_Out.Draw;
           
      end if;
        
      -- MacCready setup buttons
      --------------------------------------------------------------------------
      
      Btn_Ascent_Plus.Draw;
      
      Btn_Ascent_Min.Draw;
      
      if Flight.Aircraft.Get_Sink > 0.0 then
         
         Color := Line_Grass;

      else
         
         Color := Line_Red;
         
      end if;
             
      Glex.Fonts.Draw (Text      => Utility.Strings.Float_Image (Flight.Aircraft.Get_Sink, 1),
                       X         => Btn_Ascent_Plus.Get_Allocation.X + 0.5 * Btn_Ascent_Plus.Get_Allocation.W,
                       Y         => 0.5 * (Btn_Ascent_Plus.Get_Allocation.Y + Btn_Ascent_Min.Get_Allocation.Y + Btn_Ascent_Min.Get_Allocation.H),
                       Style     => Font_1,
                       Color     => Color,
                       Alignment => Glex.Fonts.Alignment_CC);
      
      -- Optimal airspeed in current direction
      --------------------------------------------------------------------------
      
      -- TODO: manage units to synchronize with gauges panel (km/h <-> kts)
       
      if Flight.Data.Is_Recent (Flight.Field_Course) then
         
         declare
            Speed : Float := Flight.Aircraft.Get_Optimal_Speed;
         begin
         
            if Speed > 0.0 then
            
               Glex.Fonts.Draw (Text      => Float_Image (3.6 * Speed, 0),
                                X         => Btn_Ascent_Min.Get_Allocation.X - 0.02,
                                Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.16,
                                Style     => Font_3,
                                Color     => Line_White, 
                                Alignment => Glex.Fonts.Alignment_LR);
         
            end if;
         
         end;
         
      else
         
         Glex.Fonts.Draw (Text      => "---",
                          X         => Btn_Ascent_Min.Get_Allocation.X - 0.03,
                          Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.16,
                          Style     => Font_1,
                          Color     => Line_Red, 
                          Alignment => Glex.Fonts.Alignment_LR);
         
      end if;
         
      -- Necessary altitude to reach waypoint
      --------------------------------------------------------------------------
      
      -- TODO: manage units to synchronize with gauges panel (m <-> FT)
 
      Glex.Fonts.Draw (Text      => "\",
                       X         => Btn_Ascent_Min.Get_Allocation.X - 0.14,
                       Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.02,
                       Style     => Font_4,
                       Color     => Line_Magenta,
                       Alignment => Glex.Fonts.Alignment_LL);
      
      if Flight.Data.Is_Recent (Flight.Field_Position) then
         
         Glex.Fonts.Draw (Text      => Flight.Plan.Next_Waypoint.Get_Required_Altitude,
                          X         => Btn_Ascent_Min.Get_Allocation.X - 0.02,
                          Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.02,
                          Style     => Font_3,
                          Color     => Line_Magenta,
                          Alignment => Glex.Fonts.Alignment_LR);
                
      else
         
         Glex.Fonts.Draw (Text      => "---",
                          X         => Btn_Ascent_Min.Get_Allocation.X - 0.03,
                          Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.02,
                          Style     => Font_1, 
                          Color     => Line_Red,
                          Alignment => Glex.Fonts.Alignment_LR);         
         
      end if;
      
      -- Optimal airspeed to reach waypoint
      --------------------------------------------------------------------------
      
      Glex.Fonts.Draw (Text      => ">",
                       X         => Btn_Ascent_Min.Get_Allocation.X - 0.14,
                       Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.09,
                       Style     => Font_4,
                       Color     => Line_Magenta,
                       Alignment => Glex.Fonts.Alignment_LL);
      
      if Flight.Data.Is_Recent (Flight.Field_Position) then
         
         Glex.Fonts.Draw (Text      => Flight.Plan.Next_Waypoint.Get_Optimal_Speed,
                          X         => Btn_Ascent_Min.Get_Allocation.X - 0.02,
                          Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.09,
                          Style     => Font_3,
                          Color     => Line_Magenta, 
                          Alignment => Glex.Fonts.Alignment_LR);
         
      else
         
         Glex.Fonts.Draw (Text      => "---",
                          X         => Btn_Ascent_Min.Get_Allocation.X - 0.03,
                          Y         => Btn_Ascent_Min.Get_Allocation.Y + 0.09,
                          Style     => Font_1, 
                          Color     => Line_Red,
                          Alignment => Glex.Fonts.Alignment_LR);         
         
      end if;
      
      -- Wind speed and direction
      --------------------------------------------------------------------------

      Draw_Wind;
      
      -- Show blinking GNSS flag when no fix
      --------------------------------------------------------------------------

      X := Btn_Ascent_Min.Get_Allocation.X + Btn_Ascent_Min.Get_Allocation.W;

      if not Flight.Data.Is_Recent (Flight.Field_Position, Lapse => 4.0) then
             
         Glex.Fonts.Draw (Text      => "GNSS",
                          X         => X,
                          Y         => 0.94,
                          Style     => Font_1,
                          Color     => Line_Red,
                          Alignment => Glex.Fonts.Alignment_LR);
            
      end if;
      
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
                             Y         => 0.88,
                             Style     => Font_1,
                             Color     => Color,
                             Alignment => Glex.Fonts.Alignment_LR);
               
         end if;
         
      end;
      
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
      
      Display.Panels.Gauges.Screen_Pressed (X ,Y);
      
      if 
        Btn_Move.Contains (X, Y) 
      then
                  
         Show_Move := not Show_Move;
         
         if Show_Move then
            
            Btn_Move.Set_Background_Color (Color_Amber.With_Alpha (0.8));	
         
         else
                        
            Btn_Move.Set_Background_Color (Color_Black.With_Alpha (0.5));
            
         end if;
         
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
        Btn_Ascent_Plus.Contains (X, Y)
      then
         Flight.Aircraft.Set_Sink (Flight.Aircraft.Get_Sink + 0.5);
                  
      elsif
        Btn_Ascent_Min.Contains (X, Y)
      then
         Flight.Aircraft.Set_Sink (Flight.Aircraft.Get_Sink - 0.5);
         
      elsif 
        Show_Move and 
        Btn_Zoom_In.Contains (X, Y) 
      then
         
         View.Zoom_In;
         
         Center_Frame;
         
      elsif 
        Show_Move and 
        Btn_Zoom_Out.Contains (X, Y) 
      then
         
         View.Zoom_Out;
         
         Center_Frame;
           
      elsif 
        Show_Move and 
        Btn_Right.Contains (X, Y)
      then
         
         View.Center.Lon := View.Center.Lon + 0.1 * Long_Float (View.Zoom);
         
         Auto_Center := False;
         
      elsif 
        Show_Move and
        Btn_Left.Contains (X, Y)
      then
         
         View.Center.Lon := View.Center.Lon - 0.1 * Long_Float (View.Zoom);
         
         Auto_Center := False;
         
      elsif 
        Show_Move and
        Btn_Up.Contains (X, Y)
      then
         
         View.Center.Lat := View.Center.Lat + 0.1 * Long_Float (View.Zoom);
         
         Auto_Center := False;
         
      elsif 
        Show_Move and 
        Btn_Down.Contains (X, Y) 
      then
         
         View.Center.Lat := View.Center.Lat - 0.1 * Long_Float (View.Zoom);
         
         Auto_Center := False;
             
      else
         
         Changed := False;
         
      end if;
      
      if Changed then 
         
         Display.Refresh := True;
      
      end if;
               
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin 
      
      case Key is
         
         when Panel_Wheel_Left =>
            
            -- Zoom out
            ----------------------------
            
            View.Zoom_Out;
            
            Display.Refresh := True;
      
         when Panel_Wheel_Right =>
            
            -- Zoom in
            ----------------------------
            
            View.Zoom_In;
            
            Display.Refresh := True;
      
         when Panel_Wheel_Button =>
            
            -- Reset the view
            ----------------------------
                 
            Show_Move := False;
                     
            Auto_Center := True;
            
            View.Zoom := Lower_Zoom;
                      
            Btn_Move.Set_Background_Color (Color_Black.With_Alpha (0.5));
            
            Center_Frame;
            
            Update_View;
            
            Display.Refresh := True;
            
         when Panel_Button_Right =>
            
            -- Toggle cone
            ----------------------------
            
            View.Cone_Active := not View.Cone_Active;
            
         when Panel_Button_Left =>
              
            -- Not assigned
            ----------------------------
            
            null;
            
         when others =>
            
            null;
            
      end case;
                        
   end Key_Changed;
   -----------------------------------------------------------------------------
   
end Display.Pages.Navigation;
--------------------------------------------------------------------------------
