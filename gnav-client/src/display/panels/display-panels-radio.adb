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
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Log;
with Utility.Strings;
with Maps;
use  Maps;
with Maps.Reference;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Radio is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Widgets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Pnl_Radio : Panel_Record;

   Btn_All   : Button_Record;

   Btn_10km  : Button_Record;

   Btn_20km  : Button_Record;

   Btn_Air   : Button_Record;

   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.010,
                                             Height    => 0.036,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   First_Row  : Natural := 1;

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

      Pnl_Radio.Set_Allocation (Allocation);

      Pnl_Radio.Set_Background_Color (Color_Gray_1);

      Pnl_Radio.Set_Show_Border (True);

      Pnl_Radio.Set_Label ("FREQUENCY", Label_Left);

      Pnl_Radio.Set_Font_Size (0.03, 0.25);

      Pnl_Radio.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Allocation.X := X + 0.005;
      Allocation.Y := Y + 0.020;
      Allocation.W := (W - 0.025) / 4.0;
      Allocation.H := 0.08;

      Btn_10km.Set_Allocation (Allocation);
      Btn_10km.Set_Label ("10KM");
      Btn_10km.Set_Font_Size (0.4, 0.3);
      Btn_10km.Set_Style (Button_Normal);

      Allocation.X := Allocation.X + Allocation.W + 0.005;

      Btn_20km.Set_Allocation (Allocation);
      Btn_20km.Set_Label ("20KM");
      Btn_20km.Set_Font_Size (0.4, 0.3);
      Btn_10km.Set_Style (Button_Normal);

      Allocation.X := Allocation.X + Allocation.W + 0.005;

      Btn_Air.Set_Allocation (Allocation);
      Btn_Air.Set_Label ("AIR");
      Btn_Air.Set_Font_Size (0.4, 0.3);
      Btn_Air.Set_Style (Button_Normal);

      Allocation.X := Allocation.X + Allocation.W + 0.005;

      Btn_All.Set_Allocation (Allocation);
      Btn_All.Set_Label ("ALL");
      Btn_All.Set_Font_Size (0.4, 0.3);
      Btn_All.Set_Style (Button_Enabled);

   end Initialize;
   -----------------------------------------------------------------------------



   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Filter : Natural := 100;
   Count  : Natural := 0;
   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      Xn : constant Float := Pnl_Radio.Get_Allocation.X + 0.01;
      Xf : constant Float := Xn + 0.24;
      B  : constant Float := Pnl_Radio.Get_Allocation.Y + 0.1;
      Y  : Float := Pnl_Radio.Get_Allocation.Y + Pnl_Radio.Get_Allocation.H - 0.09;
      Line_Color : Line_Color_Record := Line_Green;

   begin

      Pnl_Radio.Draw;

      Count := 0;

      for R in First_Row .. Maps.Reference.Get_Number_Of_Stations loop

         exit when Y < B;

         -- 0 is only for stations without location
         if Filter = 0 then
            if Maps.Reference.Station_Has_Airfield (R) then
               goto Next_Station;
            end if;

         -- Under 100km, only stations with location and within range
         elsif Filter < 100 then
            if
              not Maps.Reference.Station_Has_Airfield (R) or else
                  Maps.Reference.Get_Distance_To_Station (R) > Filter
            then
               goto Next_Station;
            end if;
         end if;

         if not Maps.Reference.Station_Has_Airfield (R) then

            Line_Color := Line_Yellow;

         elsif Maps.Reference.Get_Distance_To_Station (R) <= 10 then

            Line_Color := Line_Green;

         elsif Maps.Reference.Get_Distance_To_Station (R) <= 20 then

            Line_Color := Line_Orange;

         else
            Line_Color := Line_Gray;

         end if;

         Glex.Fonts.Draw (Maps.Reference.Get_Station_Name (R),
                          Xn, Y,
                          Font_1,
                          Line_Color);

         Glex.Fonts.Draw (Maps.Reference.Get_Station_Frequency (R),
                          Xf, Y,
                          Font_1,
                          Line_Color);

         Y := Y - 1.9 * Font_1.Height;

         Count := Count + 1;

         <<Next_Station>>

      end loop;

      Btn_All.Draw;

      Btn_10km.Draw;

      Btn_20km.Draw;

      Btn_Air.Draw;

   end Draw;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reference : Integer := 0;
   Offset    : Integer := 0;
   --===========================================================================
   -- (See specifications file)
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean) is
   begin

      if Pnl_Radio.Contains (X, Y) then

         -- Only scroll down when the table is full
         ------------------------------------------
         if Count < 10 and Dy > 0.0 then

            return;

         end if;

         -- Compute new first item
         ------------------------------------------
         if First then

            Reference := First_Row;

         end if;

         Offset := Integer (Dy / (1.9 * Font_1.Height));

         declare
            New_First : Integer := Reference + Offset;
         begin

            New_First := Natural'Min (New_First, Maps.Reference.Get_Number_Of_Stations);
            New_First := Natural'Max (New_First, 1);

            if New_First /= First_Row then

               First_Row := New_First;

               Display.Refresh := True;

            end if;
         end;

      end if;

   end Screen_Move;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      if Btn_10km.Contains (X, Y) then

         First_Row := 1;
         Filter    := 10;
         Btn_10km.Set_Style (Button_Enabled);
         Btn_20km.Set_Style (Button_Normal);
         Btn_Air .Set_Style (Button_Normal);
         Btn_All .Set_Style (Button_Normal);

         Refresh := True;

      elsif Btn_20km.Contains (X, Y) then

         First_Row := 1;
         Filter    := 20;
         Btn_10km.Set_Style (Button_Normal);
         Btn_20km.Set_Style (Button_Enabled);
         Btn_Air .Set_Style (Button_Normal);
         Btn_All .Set_Style (Button_Normal);

         Refresh := True;

      elsif Btn_Air.Contains (X, Y) then

         First_Row := 1;
         Filter    := 0;
         Btn_10km.Set_Style (Button_Normal);
         Btn_20km.Set_Style (Button_Normal);
         Btn_Air .Set_Style (Button_Enabled);
         Btn_All .Set_Style (Button_Normal);

         Refresh := True;

      elsif Btn_All.Contains (X, Y) then

         First_Row := 1;
         Filter    := 100;
         Btn_10km.Set_Style (Button_Normal);
         Btn_20km.Set_Style (Button_Normal);
         Btn_Air .Set_Style (Button_Normal);
         Btn_All .Set_Style (Button_Enabled);

         Refresh := True;

      end if;

   end Screen_Pressed;
   -----------------------------------------------------------------------------

end Display.Panels.Radio;
--------------------------------------------------------------------------------
