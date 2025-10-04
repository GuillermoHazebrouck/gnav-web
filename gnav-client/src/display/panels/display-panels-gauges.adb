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
with Utility.Log;
with Utility.Strings;
with Utility.Storage;
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

   Waypoint_Only : Boolean := True;

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
                                             Space     => 0.012,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   -- Fonts home and waypoint data
   ---------------------------------
   Font_2 : Glex.Fonts.Font_Style_Record := (Width     => 0.012,
                                             Height    => 0.030,
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

   -- Fonts (Altimeter)
   ---------------------------------
   Font_7 : Glex.Fonts.Font_Style_Record := (Width     => 0.008,
                                             Height    => 0.030,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Bold);

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
   procedure Load_Timer is

      use Utility.Strings;
      Time_Reader : String_Buffer (20);

   begin

      Time_Reader.Load (Utility.Storage.Get_Item ("TIMER"));

      if not Time_Reader.End_Of_Stream then

         Timer := Utility.Calendar.Time_Of (Year    => Integer_Value (Time_Reader.Read_Next ('/'), 2025),
                                            Month   => Integer_Value (Time_Reader.Read_Next ('/'), 1),
                                            Day     => Integer_Value (Time_Reader.Read_Next ('/'), 1),
                                            Seconds => Float_Value   (Time_Reader.Read_Next ('/'), 0.0));
      else
         Utility.Log.Put_Message ("timer not set");

      end if;

   end Load_Timer;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Save_Timer is

      use Utility.Calendar;
      use Utility.Strings;

      Year    : Years;
      Month   : Months;
      Day     : Days;
      Seconds : Day_Lapse;

   begin

      Utility.Calendar.Split (Timer, Year, Month, Day, Seconds);

      Utility.Storage.Set_Item ("TIMER",
                          Integer_Image (Year)  & "/" &
                          Integer_Image (Month) & "/" &
                          Integer_Image (Day)   & "/" &
                          Float_Image (Seconds, 0));

   end Save_Timer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
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

      Pnl_Timer.Set_Background_Color (Color_Gray_7);

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

      -- Elevation panel
      ------------------------------------------------------

      Pnl_Elevation.Copy (Pnl_Speed);

      Allocation.Y := 0.61;

      Pnl_Elevation.Set_Allocation (Allocation);

      Pnl_Elevation.Set_Label ("AGL", Label_Right);

      -- Altitude panel
      ------------------------------------------------------

      Pnl_Altitude.Copy (Pnl_Elevation);

      Allocation.Y := 0.48;

      Pnl_Altitude.Set_Allocation (Allocation);

      Pnl_Altitude.Set_Label ("ASL", Label_Right);

      -- Compass
      ------------------------------------------------------

      Display.Compass.Initialize;

      -- Waypoint vector panel
      ------------------------------------------------------

      Pnl_Waypoint.Copy (Pnl_Altitude);

      Allocation.Y := 0.010;
      Allocation.W := 0.210;
      Allocation.H := 0.140;

      Pnl_Waypoint.Set_Allocation (Allocation);

      Pnl_Waypoint.Set_Label_Color (Fore => Line_Magenta.Fore,
                                    Glow => Line_Magenta.Glow);

      Pnl_Waypoint.Set_Label ("WPT");

      -- Home vector panel
      ------------------------------------------------------

      Pnl_Home.Copy (Pnl_Waypoint);

      Allocation.X := 0.895;
      Allocation.W := 0.100;

      Pnl_Home.Set_Allocation (Allocation);

      Pnl_Home.Set_Label_Color (Fore => Line_Grass.Fore,
                                Glow => Line_Grass.Glow);

      Pnl_Home.Set_Label ("B/H");

      -- Restore the timer
      ------------------------------------------------------

      Timer := Utility.Calendar.Cached_Time;

      Load_Timer;

      Cancel_Timer := Timing.Events.Register_Timer (3.0, Cancel_Timer_Reset'Access);

      Cancel_Timer.Pause;

      Initialized := True;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw is

      use all type Flight.Plan.Range_Kinds;
      use Utility.Calendar;
      use Utility.Strings;

      Left  : constant Float := 0.790;

      Clock       : Day_Lapse;
      Line_Color  : Line_Color_Record;
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

         Line_Color := Line_White;

      else

         Line_Color := Line_Gray;

      end if;

      Glex.Fonts.Draw (Utility.Strings.Hour_Image (Clock),
                       0.945,
                       0.885,
                       Font_4,
                       Line_Color,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Utility.Strings.Minute_Image (Clock),
                       0.990,
                       0.885,
                       Font_5,
                       Line_Color,
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
                          0.500,
                          Font_1,
                          Line_Cyan,
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

      -- Elevation above ground level (not barometric)
      ------------------------------------------------------

      Pnl_Elevation.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Elevation) then

         if    Flight.Data.Elevation > 300.0 then
            Line_Color := Line_Yellow;
         elsif Flight.Data.Elevation > 100.0 then
            Line_Color := Line_Orange;
         else
            Line_Color := Line_Gray;
         end if;

         Glex.Fonts.Draw (Utility.Strings.Float_Image (Convert (Flight.Data.Elevation, Unit_Meter, Altitude_Unit), 0),
                          0.935,
                          0.630,
                          Font_1,
                          Line_Color,
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

      -- Compass
      ------------------------------------------------------

      Display.Compass.Draw (0.89, 0.320, 0.12);

      -- Waypoint and home panels
      ------------------------------------------------------

      Pnl_Waypoint.Set_Label (Trim (Flight.Plan.Next_Waypoint.Name));

      Pnl_Waypoint.Draw;

      if Flight.Data.Is_Recent (Flight.Field_Position) then

         Glex.Fonts.Draw (Flight.Plan.Next_Waypoint.Get_Distance,
                          0.88,
                          0.075,
                          Font_2,
                          Line_Magenta,
                          Glex.Fonts.Alignment_LR);

         -- If the altitude is available, show the margin,
         -- otherwise show the required descent
         ------------------------------------------------------
         if Flight.Data.Is_Recent (Flight.Field_Altitude) then

            case Flight.Plan.Next_Waypoint.In_Range is

               when Range_Safe        => Line_Color := Line_Grass;
               when Range_Unsafe      => Line_Color := Line_Yellow;
               when Range_Unreachable => Line_Color := Line_Red;

            end case;

            Glex.Fonts.Draw (Flight.Plan.Next_Waypoint.Get_Margin,
                             0.88,
                             0.025,
                             Font_2,
                             Line_Color,
                             Glex.Fonts.Alignment_LR);

         else

            Glex.Fonts.Draw ("\" & Flight.Plan.Next_Waypoint.Get_Required_Altitude,
                             0.88,
                             0.025,
                             Font_2,
                             Line_Magenta,
                             Glex.Fonts.Alignment_LR);

         end if;

         if Waypoint_Only then

            Glex.Fonts.Draw (Flight.Plan.Next_Waypoint.Get_Bearing,
                             0.99,
                             0.075,
                             Font_2,
                             Line_Magenta,
                             Glex.Fonts.Alignment_LR);

            Glex.Fonts.Draw ("}" & Flight.Plan.Next_Waypoint.Get_Elevation,
                             0.99,
                             0.025,
                             Font_2,
                             Line_Brown,
                             Glex.Fonts.Alignment_LR);

         else

            Pnl_Home.Set_Label (Trim (Flight.Plan.Home_Waypoint.Name));

            Pnl_Home.Draw;

            Glex.Fonts.Draw (Flight.Plan.Home_Waypoint.Get_Distance,
                             0.99,
                             0.075,
                             Font_2,
                             Line_Grass,
                             Glex.Fonts.Alignment_LR);

            case Flight.Plan.Home_Waypoint.In_Range is

               when Range_Safe        => Line_Color := Line_Grass;
               when Range_Unsafe      => Line_Color := Line_Yellow;
               when Range_Unreachable => Line_Color := Line_Red;

            end case;

            Glex.Fonts.Draw (Flight.Plan.Home_Waypoint.Get_Margin,
                             0.99,
                             0.025,
                             Font_2,
                             Line_Color,
                             Glex.Fonts.Alignment_LR);

         end if;

      else

         if Waypoint_Only then

            Glex.Fonts.Draw ("---",
                             0.890,
                             0.065,
                             Font_2,
                             Line_Red,
                             Glex.Fonts.Alignment_CC);
         else

            Glex.Fonts.Draw ("---",
                             0.835,
                             0.065,
                             Font_2,
                             Line_Red,
                             Glex.Fonts.Alignment_CC);

            Pnl_Home.Set_Label (Trim (Flight.Plan.Home_Waypoint.Name));

            Pnl_Home.Draw;

            Glex.Fonts.Draw ("---",
                             0.945,
                             0.065,
                             Font_2,
                             Line_Red,
                             Glex.Fonts.Alignment_CC);

         end if;

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   Bar : Widgets.Widget.Widget_Record;
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw_Altimeter (X, Y, H : Float) is

      Allocation : Allocation_Record;
      Color_Bar  : Allocation_Record;

      Scale : constant Float := 400.0; -- [m] up and down
      Step  : constant Float := 100.0; -- [m] between dashes

      Base  : Float;
      Floor : Float;
      D, S  : Float;
      C, M  : Float;
      Down  : Float;
      B, T  : Float;
      G     : Float := 0.0; -- ground
      Z     : Float := 0.0; -- critical altitude
      Color : Line_Color_Record := Line_Cyan;

   begin

      if Flight.Data.Is_Recent (Flight.Field_Altitude) then

         if Flight.Data.Is_Recent (Flight.Field_Elevation) then
            G := Float'Max (0.0, Flight.Data.Altitude - Flight.Data.Elevation);
            Z := G + 300.0;
         end if;

         Allocation.H := H;
         Allocation.W := 0.010;
         Allocation.X := X - Allocation.W;
         Allocation.Y := Y - Allocation.H * 0.5;

         Bar.Set_Allocation (Allocation);
         Bar.Set_Background_Color (Color_Ocean);
         Bar.Draw;

         C := Allocation.X + 0.5 * Allocation.W;
         T := C - 0.012;

         Floor := Flight.Data.Altitude - Scale;

         Color_Bar := Allocation;

         -- Critical area
         --------------------------------------
         if Floor < Z then

            Color_Bar.H := H * (Z - Floor) / Scale * 0.5;

            Bar.Set_Allocation (Color_Bar);
            Bar.Set_Background_Color (Color_Orange);
            Bar.Draw;

         end if;

         -- Ground bar
         --------------------------------------
         if Floor < G then

            Color_Bar.H := H * (G - Floor) / Scale * 0.5;

            Bar.Set_Allocation (Color_Bar);
            Bar.Set_Background_Color (Color_Gray_3);
            Bar.Draw;

         end if;

         Base  := Float'Ceiling ((Floor) / Step) * Step;
         Down  := Flight.Data.Altitude - Base;

         S := Step / Scale * Allocation.H * 0.5;
         D := Down / Scale * Allocation.H * 0.5;

         M := Allocation.Y + Allocation.H * 0.5 - D;
         B := Base;

         -- Markers
         --------------------------------------
         for I in 1..8 loop

            if B > 0.0 then

               if    B >= Z then

                  -- Above critical level
                  -----------------------------
                  Color := Line_Cyan;

               elsif B >= G then

                  -- Above ground level
                  -----------------------------
                  Color := Line_Orange;

               else
                  -- Below ground level
                  -----------------------------
                  Color := Line_Gray;

               end if;

               Glex.Fonts.Draw ("-",
                                C, M,
                                Font_6,
                                Color,
                                Glex.Fonts.Alignment_CC);

               Glex.Fonts.Draw (Utility.Strings.Float_Image (B, 0),
                                T, M,
                                Font_7,
                                Color,
                                Glex.Fonts.Alignment_CR);

            end if;

            B := B + Step;
            M := M + S;

         end loop;

         -- Current altitude indicator
         --------------------------------------
         Glex.Fonts.Draw ("-",
                          C, Allocation.Y + Allocation.H * 0.5,
                          Font_1,
                          Line_Yellow,
                          Glex.Fonts.Alignment_CL);

      end if;

   end Draw_Altimeter;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
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

            Save_Timer;

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

      elsif
        Pnl_Waypoint.Contains (X, Y) or else
        Pnl_Home.Contains (X, Y)
      then

         Waypoint_Only := not Waypoint_Only;

         declare
            Allocation : Allocation_Record := Pnl_Waypoint.Get_Allocation;
         begin

            if Waypoint_Only then

               Allocation.W := Pnl_Home.Get_Allocation.X + Pnl_Home.Get_Allocation.W - Allocation.X;

            else

               Allocation.W := Pnl_Home.Get_Allocation.W;

            end if;

            Pnl_Waypoint.Set_Allocation (Allocation);

         end;

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
   function Altitude_Pressed (X, Y : Float) return Boolean is
   begin

      return Pnl_Altitude.Contains (X, Y);

   end Altitude_Pressed;
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
