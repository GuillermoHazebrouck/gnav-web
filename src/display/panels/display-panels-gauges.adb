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
with Display.Compass;
with Flight;
with Flight.Aircraft;
with Flight.Plan;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Timing.Events;
with Utility.Calendar;
with Utility.Atmosphere;
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
package body Display.Panels.Gauges is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The timer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Timer : Utility.Calendar.Times;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Flag that indicates if the timer must be reset on the next click
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reset_Timer : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the UTC time must be displayed
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Utc_Time : Boolean := False;

   -- Flight data indicators
   ---------------------------------

   Frm_Data      : Widget_Record;

   Pnl_Clock     : Panel_Record;

   Pnl_Timer     : Panel_Record;

   Pnl_Speed     : Panel_Record;

   Pnl_Altitude  : Panel_Record;

   Pnl_Elevation : Panel_Record;

   Pnl_Waypoint  : Panel_Record;

   Pnl_Home      : Panel_Record;

   -- Status variables
   ---------------------------------

   Initialized   : Boolean := False;

   Altitude_Unit : Altitude_Units := Unit_Meter;

   Distance_Unit : Distance_Units := Unit_Kilometer;

   Velocity_Unit : Velocity_Units := Unit_Kilometer_Hour;

   -- Fonts
   ---------------------------------
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.020,
                                             Height    => 0.060,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   -- Fonts
   ---------------------------------
   Font_2 : Glex.Fonts.Font_Style_Record := (Width     => 0.015,
                                             Height    => 0.035,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   -- Fonts
   ---------------------------------
   Font_3 : Glex.Fonts.Font_Style_Record := (Width     => 0.008,
                                             Height    => 0.030,
                                             Space     => 0.003,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   -- Fonts for clock (hour/minutes)
   ---------------------------------
   Font_4 : Glex.Fonts.Font_Style_Record := (Width     => 0.018,
                                             Height    => 0.070,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Thin);

   -- Fonts for clock (minutes/seconds)
   ---------------------------------
   Font_5 : Glex.Fonts.Font_Style_Record := (Width     => 0.010,
                                             Height    => 0.040,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   -- Fonts
   ---------------------------------
   Font_6 : Glex.Fonts.Font_Style_Record := (Width     => 0.010,
                                             Height    => 0.040,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   Cancel_Timer : access Timing.Events.Timer_Record := null;

   --===========================================================================
   -- Cancels the reset of the timer
   --===========================================================================
   procedure Cancel_Timer_Reset is
   begin

      Reset_Timer := False;

      Refresh     := True;

      Cancel_Timer.Pause;

   end Cancel_Timer_Reset;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is

      use Utility.Strings;
      use Widgets.Panel;

      M : Dimension_Float := 0.01;
      H : Dimension_Float := 0.08;
      W : Dimension_Float := 0.05;
      V : Dimension_Float := 0.04;

      Allocation : Allocation_Record;

   begin

      if Initialized then
         return;
      end if;

      -- Data frame background
      ------------------------------------------------------

      Allocation.X := 0.78;
      Allocation.H := 1.0;
      Allocation.Y := 0.0;
      Allocation.W := 0.22;

      Frm_Data.Set_Allocation (Allocation);

      Frm_Data.Set_Background_Color (Color_Gray_2);

      Frm_Data.Set_Show_Border (False);

      -- Timer panel
      ------------------------------------------------------

      Allocation.X := 0.785;
      Allocation.Y := 0.870;
      Allocation.W := 0.100;
      Allocation.H := 0.100;

      Pnl_Timer.Set_Allocation (Allocation);

      Pnl_Timer.Set_Background_Color (Color_Black);

      Pnl_Timer.Set_Transparency (0.60);

      Pnl_Timer.Set_Show_Border (True);

      Pnl_Timer.Set_Label ("TMR", Label_Right);

      Pnl_Timer.Set_Font_Size (0.03, 0.25);

      Pnl_Timer.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      -- Clock panel
      ------------------------------------------------------

      Allocation.X := 0.895;
      Allocation.Y := 0.870;
      Allocation.W := 0.100;
      Allocation.H := 0.100;

      Pnl_Clock.Copy (Pnl_Timer);

      Pnl_Clock.Set_Allocation (Allocation);

      Pnl_Clock.Set_Label ("L/T", Label_Right);

      -- Speed panel
      ------------------------------------------------------

      Pnl_Speed.Copy (Pnl_Timer);

      Allocation.X := 0.785;
      Allocation.Y := 0.740;
      Allocation.W := 0.210;
      Allocation.H := 0.100;

      Pnl_Speed.Set_Allocation (Allocation);

      Pnl_Speed.Set_Label ("G/S", Label_Right);

      -- Altitude panel
      ------------------------------------------------------

      Pnl_Altitude.Copy (Pnl_Speed);

      Allocation.Y := 0.61;

      Pnl_Altitude.Set_Allocation (Allocation);

      Pnl_Altitude.Set_Label ("ASL", Label_Right);

      -- Elevation panel
      ------------------------------------------------------

      Pnl_Elevation.Copy (Pnl_Altitude);

      Allocation.Y := 0.48;

      Pnl_Elevation.Set_Allocation (Allocation);

      Pnl_Elevation.Set_Label ("AGL", Label_Right);

      -- Compass
      ------------------------------------------------------

      Display.Compass.Initialize;

      -- Waypoint vector panel
      ------------------------------------------------------

      Pnl_Waypoint.Copy (Pnl_Altitude);

      Allocation.Y := 0.010;
      Allocation.W := 0.100;
      Allocation.H := 0.140;

      Pnl_Waypoint.Set_Allocation (Allocation);

      Pnl_Waypoint.Set_Label_Color (Fore => Line_Magenta.Fore,
                                    Glow => Line_Magenta.Glow);

      Pnl_Waypoint.Set_Label ("WPT");

      -- Home vector panel
      ------------------------------------------------------

      Pnl_Home.Copy (Pnl_Waypoint);

      Allocation.X := 0.895;

      Pnl_Home.Set_Allocation (Allocation);

      Pnl_Home.Set_Label_Color (Fore => Line_Grass.Fore,
                                Glow => Line_Grass.Glow);

      Pnl_Home.Set_Label ("B/H");

      -- Setup done
      ------------------------------------------------------

      Timer := Utility.Calendar.Cached_Time;

      Cancel_Timer := Timing.Events.Register_Timer (3.0, Cancel_Timer_Reset'Access);

      Cancel_Timer.Pause;

      Initialized := True;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Utility.Calendar;
      use Utility.Strings;

      Left  : constant Float := 0.790;

      Clock       : Day_Lapse;
      Clock_Color : Line_Color_Record;
      Timer_Span  : Lapses    := Cached_Time - Timer;
      Timer_Clock : Day_Lapse := Get_Clock (Zero_Hour (Timer) + Timer_Span);

   begin

      if not Initialized then

         return;

      end if;

      -- Transparent background frame
      ------------------------------------------------------
      Frm_Data.Draw;

      -- Time
      ------------------------------------------------------

      if Reset_Timer then

         Pnl_Timer.Set_Label_Color (Color_Red);

      else

         Pnl_Timer.Set_Label_Color (Color_Gray_8);

      end if;

      Pnl_Timer.Draw;

      if Timer_Span < One_Hour then

         Glex.Fonts.Draw (Utility.Strings.Minute_Image (Timer_Clock),
                          0.835,
                          0.885,
                          Font_4,
                          Line_White,
                          Glex.Fonts.Alignment_LR);

         Glex.Fonts.Draw (Utility.Strings.Second_Image (Timer_Clock),
                          0.880,
                          0.885,
                          Font_5,
                          Line_White,
                          Glex.Fonts.Alignment_LR);

      else

         Glex.Fonts.Draw (Utility.Strings.Hour_Image (Timer_Clock),
                          0.835,
                          0.885,
                          Font_4,
                          Line_White,
                          Glex.Fonts.Alignment_LR);

         Glex.Fonts.Draw (Utility.Strings.Minute_Image (Timer_Clock),
                          0.880,
                          0.885,
                          Font_5,
                          Line_White,
                          Glex.Fonts.Alignment_LR);

      end if;

      -- Clock
      ------------------------------------------------------

      if Utc_Time then

         Clock := Get_Clock (Flight.Get_Utc_Time);

      else

         Clock := Get_Clock (Flight.Get_Local_Time);

      end if;

      Pnl_Clock.Draw;

      if Flight.Time_Synchronized then

         Clock_Color := Line_White;

      else

         Clock_Color := Line_Gray;

      end if;

      Glex.Fonts.Draw (Utility.Strings.Hour_Image (Clock),
                       0.945,
                       0.885,
                       Font_4,
                       Clock_Color,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Utility.Strings.Minute_Image (Clock),
                       0.990,
                       0.885,
                       Font_5,
                       Clock_Color,
                       Glex.Fonts.Alignment_LR);

      -- Ground speed
      ------------------------------------------------------

      Pnl_Speed.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Speed) then

         Glex.Fonts.Draw (Utility.Strings.Float_Image (Convert (Flight.Data.Speed, Unit_Meter_Second, Velocity_Unit), 0),
                          0.935,
                          0.760,
                          Font_1,
                          Line_Cyan,
                          Glex.Fonts.Alignment_LR);

      else

         Glex.Fonts.Draw ("---",
                          0.935,
                          0.760,
                          Font_6,
                          Line_Red,
                          Glex.Fonts.Alignment_LR);

      end if;

      Glex.Fonts.Draw (Image (Velocity_Unit),
                       0.968,
                       0.760,
                       Font_3,
                       Line_Grass,
                       Glex.Fonts.Alignment_LC);

      -- Altitude above mean sea level (not barometric)
      ------------------------------------------------------

      Pnl_Altitude.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Altitude) then

         Glex.Fonts.Draw (Utility.Strings.Float_Image (Convert (Flight.Data.Altitude, Unit_Meter, Altitude_Unit), 0),
                          0.935,
                          0.630,
                          Font_1,
                          Line_Cyan,
                          Glex.Fonts.Alignment_LR);

      else

         Glex.Fonts.Draw ("---",
                          0.935,
                          0.630,
                          Font_6,
                          Line_Red,
                          Glex.Fonts.Alignment_LR);

      end if;

      Glex.Fonts.Draw (Image (Altitude_Unit),
                       0.968,
                       0.630,
                       Font_3,
                       Line_Grass,
                       Glex.Fonts.Alignment_LC);

      -- Elevation above ground level (not barometric)
      ------------------------------------------------------

      Pnl_Elevation.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Elevation) then

         Glex.Fonts.Draw (Utility.Strings.Float_Image (Convert (Flight.Data.Elevation, Unit_Meter, Altitude_Unit), 0),
                          0.935,
                          0.500,
                          Font_1,
                          Line_Gray,
                          Glex.Fonts.Alignment_LR);

      else

         Glex.Fonts.Draw ("---",
                          0.935,
                          0.500,
                          Font_6,
                          Line_Red,
                          Glex.Fonts.Alignment_LR);

      end if;

      Glex.Fonts.Draw (Image (Altitude_Unit),
                       0.968,
                       0.500,
                       Font_3,
                       Line_Grass,
                       Glex.Fonts.Alignment_LC);

      -- Compass
      ------------------------------------------------------

      Display.Compass.Draw (0.89, 0.320, 0.12);

      -- Waypoint and home panels
      ------------------------------------------------------

      Pnl_Waypoint.Set_Label (Trim (Flight.Plan.Next_Waypoint.Name));

      Pnl_Waypoint.Draw;

      Pnl_Home.Set_Label (Trim (Flight.Plan.Home_Waypoint.Name));

      Pnl_Home.Draw;

      Glex.Fonts.Draw (Flight.Plan.Next_Waypoint.Get_Distance,
                       0.88,
                       0.07,
                       Font_2,
                       Line_Magenta,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Flight.Plan.Next_Waypoint.Get_Bearing,
                       0.88,
                       0.02,
                       Font_2,
                       Line_Magenta,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Flight.Plan.Home_Waypoint.Get_Distance,
                       0.99,
                       0.07,
                       Font_2,
                       Line_Grass,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Flight.Plan.Home_Waypoint.Get_Bearing,
                       0.99,
                       0.02,
                       Font_2,
                       Line_Grass,
                       Glex.Fonts.Alignment_LR);

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      Changed : Boolean := True;

   begin

      if not Initialized then
         return;
      end if;

      if
        Pnl_Altitude.Contains  (X, Y) or
        Pnl_Elevation.Contains (X, Y)
      then

         if Altitude_Unit = Unit_Meter then

            Altitude_Unit := Unit_Feet;

         else

            Altitude_Unit := Unit_Meter;

         end if;

         Utility.Atmosphere.Set_Altitude_Unit (Altitude_Unit);

      elsif
        Pnl_Speed.Contains (X, Y)
      then

         if Velocity_Unit = Unit_Kilometer_Hour then

            Velocity_Unit := Unit_Knot;

         else

            Velocity_Unit := Unit_Kilometer_Hour;

         end if;

      elsif
        Pnl_Timer.Contains (X, Y)
      then

         if Reset_Timer then

            Timer := Utility.Calendar.Cached_Time;

         end if;

         Reset_Timer := not Reset_Timer;

         if Reset_Timer then

            Cancel_Timer.Restart;

         end if;

      elsif
        Pnl_Clock.Contains (X, Y)
      then

         Utc_Time := not Utc_Time;

         if Utc_Time then

            Pnl_Clock.Set_Label ("UTC", Label_Right);

         else

            Pnl_Clock.Set_Label ("L/T", Label_Right);

         end if;

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

            null;

         when Panel_Wheel_Right =>

            null;

         when Panel_Wheel_Button =>

            null;

         when Panel_Button_Right =>

            null;

         when Panel_Button_Left =>

            null;

         when others =>

            null;

      end case;

   end Key_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Drag (X, Y : Float) is
   begin

      null;

   end Drag;
   -----------------------------------------------------------------------------

end Display.Panels.Gauges;
--------------------------------------------------------------------------------
