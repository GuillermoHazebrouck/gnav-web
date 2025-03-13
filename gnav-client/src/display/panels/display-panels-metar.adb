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
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Flight.Meteo;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Log;
with Utility.Calendar;
use  Utility.Calendar;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Metar is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Widgets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Pnl_Metar : Panel_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.012,
                                             Height    => 0.040,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Thin);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Font_2 : Glex.Fonts.Font_Style_Record := (Width     => 0.008,
                                             Height    => 0.030,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Thin);

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize (X, Y, W, H : Float) is

      Allocation : Allocation_Record;

   begin

      Allocation.X := X;
      Allocation.Y := Y;
      Allocation.W := W;
      Allocation.H := H;

      Pnl_Metar.Set_Allocation (Allocation);

      Pnl_Metar.Set_Background_Color (Color_Gray_1);

      Pnl_Metar.Set_Show_Border (True);

      Pnl_Metar.Set_Label ("METAR", Label_Left);

      Pnl_Metar.Set_Font_Size (0.03, 0.25);

      Pnl_Metar.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Flight.Meteo;
      use Glex.Colors;
      use Glex.Fonts;
      use Utility.Strings;

      X : Float   := Pnl_Metar.Get_Allocation.X + 0.01;
      Y : Float   := Pnl_Metar.Get_Allocation.Y + Pnl_Metar.Get_Allocation.H - 0.1;

      Color   : Line_Color_Record := Line_Cyan;
      Station : access Meteo_Station_Record := Get_Local_Station;

   begin

      Pnl_Metar.Draw;

      if Station.Loaded then

         Draw (Station.Name,
               X, Y,
               Font_1,
               Line_Green,
               Alignment_LL);

         X := X + 0.20;

         Draw (Float_Image (Maps.Distance (Flight.Data.Position, Station.Position), 0) & " KM",
               X, Y,
               Font_1,
               Line_Green,
               Alignment_LR);

         X := Pnl_Metar.Get_Allocation.X + 0.01;
         Y := Y - 2.0 * Font_1.Height;

         Draw ("QNH " & Integer_Image (Station.Qnh),
               X, Y,
               Font_1,
               Line_Yellow,
               Alignment_LL);

         Y := Y - 2.0 * Font_1.Height;

         Draw (Integer_Image (Station.Temperature) & "*C / " & Integer_Image (Station.Dew_Point) & "*C",
               X, Y,
               Font_1,
               Line_Green,
               Alignment_LL);

         Y := Y - 2.0 * Font_1.Height;

         Draw (Integer_Image (Station.Wind_Speed) & " KM/H " & Integer_Image (Station.Wind_Course) & "*",
               X, Y,
               Font_1,
               Line_Cyan,
               Alignment_LL);

         Y := Y - 2.0 * Font_1.Height;

         if Station.Cloud_Base = 0 then

            Draw ("CLEAR SKY",
                  X, Y,
                  Font_1,
                  Line_White,
                  Alignment_LL);

         elsif Station.Cloud_Base = 1 then

            Draw ("FOG",
                  X, Y,
                  Font_1,
                  Line_White,
                  Alignment_LL);


         else

            Draw ("} " & Integer_Image (Station.Cloud_Base) & " M ASL",
                  X, Y,
                  Font_1,
                  Line_White,
                  Alignment_LL);

         end if;

         Y := Y - 2.0 * Font_1.Height;

         if Station.Visibility = 9999 then

            Draw ("> ABOVE 10 KM",
                  X, Y,
                  Font_1,
                  Line_White,
                  Alignment_LL);

         else

            Draw ("> " & Integer_Image (Station.Visibility) & " M",
                  X, Y,
                  Font_1,
                  Line_White,
                  Alignment_LL);

         end if;

         if Station.Date /= No_Time then

            Y := Y - 2.0 * Font_1.Height;

            if not Flight.Meteo.Updated then
               Color := Line_Red;
            end if;

            Draw (Day_Lapse_Image (Station.Date),
                  Pnl_Metar.Get_Allocation.X + Pnl_Metar.Get_Allocation.W - 0.01, Y,
                  Font_1,
                  Color,
                  Alignment_LR);

         end if;

         Draw (Integer_Image (Flight.Meteo.Get_Number_Of_Stations) & " ACTIVE STATIONS",
               Pnl_Metar.Get_Allocation.X + 0.01, Pnl_Metar.Get_Allocation.Y + 0.02,
               Font_2,
               Line_Gray,
               Alignment_LL);

      else

         Draw ("NOT LOADED",
               X, Y,
               Font_1,
               Line_Orange,
               Alignment_LL);

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      null;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin

      null;

   end Key_Changed;
   -----------------------------------------------------------------------------



end Display.Panels.Metar;
--------------------------------------------------------------------------------
