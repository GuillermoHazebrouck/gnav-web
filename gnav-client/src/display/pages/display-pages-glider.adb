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
with Flight.Aircraft;
use  Flight.Aircraft;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Strings;
with Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Glider is

   -- Fonts
   ---------------------------------
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.010,
                                             Height    => 0.048,
                                             Space     => 0.007,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   Font_2 : Glex.Fonts.Font_Style_Record := (Width     => 0.014,
                                             Height    => 0.040,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   Font_V : Glex.Fonts.Font_Style_Record := (Width     => 0.018,
                                             Height    => 0.060,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   Font_3 : Glex.Fonts.Font_Style_Record := (Width     => 0.011,
                                             Height    => 0.038,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   M : constant Dimension_Float := 0.01;
   H : constant Dimension_Float := 0.10;
   W : constant Dimension_Float := 0.15;

   -- Page widgets
   ---------------------------------

   Btn_Model  : Button_Record;

   Btn_Mass   : Button_Record;

   Btn_Next   : Button_Record;

   Pnl_Mass   : Panel_Record;

   Pnl_Info   : Panel_Record;

   Pnl_Drag   : Panel_Record;

   Frm_Green  : Widget_Record;

   Frm_Yellow : Widget_Record;

   Frm_Red    : Widget_Record;

   Btn_Drag   : Button_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Drag_Slider_Range : constant Float := 1.20;
   Drag_Slider_Min   : constant Float := 0.18;
   Drag_Slider_Max   : constant Float := 0.73;
   Drag_Slider_Delta : constant Float := Drag_Slider_Max - Drag_Slider_Min;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Control is record

      Btn_Mass : Button_Record;

      Btn_Plus : Button_Record;

      Btn_Less : Button_Record;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Controls : array (Mass_Point_Range) of Mass_Control;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Focus    : Mass_Point_Range := Mass_Point_Range'First;

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is

      use Utility.Strings;

      Allocation : Allocation_Record;

   begin

      -- Info panel
      ------------------------------------------------------

      Allocation.X := 0.58;
      Allocation.Y := 0.01;
      Allocation.H := 0.85;
      Allocation.W := 0.41;

      Pnl_Info.Set_Allocation (Allocation);

      Pnl_Info.Set_Background_Color (Color_Gray_1);

      Pnl_Info.Set_Font_Size (0.03, 0.25);

      Pnl_Info.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Info.Set_Label ("OPERATION");

      -- Mass panel
      ------------------------------------------------------

      Allocation.X := 0.01;
      Allocation.Y := 0.01;
      Allocation.H := 0.85;
      Allocation.W := 0.56;

      Pnl_Mass.Set_Allocation (Allocation);

      Pnl_Mass.Set_Background_Color (Color_Gray_1);

      Pnl_Mass.Set_Font_Size (0.03, 0.25);

      Pnl_Mass.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Mass.Set_Label ("MASS");

      -- Drag panel
      ------------------------------------------------------

      Allocation.X := 0.89;
      Allocation.Y := 0.14;
      Allocation.H := 0.66;
      Allocation.W := 0.09;

      Pnl_Drag.Set_Allocation (Allocation);

      Pnl_Drag.Set_Background_Color (Color_Gray_3);

      Pnl_Drag.Set_Font_Size (0.03, 0.25);

      Pnl_Drag.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Drag.Set_Label ("DRAG");

      -- Color scale
      ------------------------------------------------------
      Allocation.X := Allocation.X + Allocation.W - 0.03;
      Allocation.W := 0.02;
      Allocation.Y := Drag_Slider_Min + 0.003;
      Allocation.H := (Drag_Slider_Max - Drag_Slider_Min) / 3.0 - 0.006;

      Frm_Green.Set_Allocation (Allocation);
      Frm_Green.Set_Background_Color (Color_Green);

      Allocation.Y := Allocation.Y + Allocation.H + 0.006;

      Frm_Yellow.Set_Allocation (Allocation);
      Frm_Yellow.Set_Background_Color (Color_Yellow);

      Allocation.Y := Allocation.Y + Allocation.H + 0.006;

      Frm_Red.Set_Allocation (Allocation);
      Frm_Red.Set_Background_Color (Color_Red);

      -- Next button
      ------------------------------------------------------

      Allocation.X := Pnl_Mass.Get_Allocation.X + Pnl_Mass.Get_Allocation.W - 0.125;
      Allocation.Y := 2.0 * M;
      Allocation.W := 0.12;
      Allocation.H := H;

      Btn_Next.Set_Allocation (Allocation);

      Btn_Next.Set_Label (">");

      Btn_Next.Set_Style (Button_Action);

      Btn_Next.Set_Font_Size (0.6, 0.4);

      --

      Allocation.X := 0.16 + M;
      Allocation.Y := 1.0 - (H + M);
      Allocation.W := Btn_Next.Get_Allocation.X - M - Allocation.X;
      Allocation.H := H;

      Btn_Model.Set_Allocation (Allocation);

      Btn_Model.Set_Label (Trim (This_Aircraft.Model));

      Btn_Model.Set_Style (Button_Alive);

      Btn_Model.Set_Font_Size (0.55, 0.35);

      --

      Btn_Mass.Set_Background_Color (Color_Black);

      Btn_Mass.Set_Border_Color (Color_Gray_3);

      Btn_Mass.Set_Label_Color (Line_Magenta);

      --

      Allocation.Y := Pnl_Mass.Get_Allocation.Y + Pnl_Mass.Get_Allocation.H - 0.02;

      for I in Flight.Aircraft.Mass_Point_Range loop

         Mass_Controls (I).Btn_Mass.Set_Style (Button_Alive);

         Mass_Controls (I).Btn_Mass.Set_Font_Size (0.6, 0.3);

         Allocation.X := (Font_1.Width + Font_1.Space) * Float (String_12'Length) + M;

         Allocation.Y := Allocation.Y - (H + 2.0 * M);

         Allocation.W := 0.12;

         Allocation.H := H;

         Mass_Controls (I).Btn_Mass.Set_Allocation (Allocation);

         --

         Mass_Controls (I).Btn_Plus.Set_Label ("+");

         Mass_Controls (I).Btn_Plus.Set_Style (Button_Action);

         Mass_Controls (I).Btn_Plus.Set_Font_Size (0.7, 0.5);

         Allocation.X := Allocation.X + Allocation.W + 2.0 * (M + Font_1.Width + Font_1.Space) + M;

         Allocation.W := 0.07;

         Mass_Controls (I).Btn_Plus.Set_Allocation (Allocation);

         --

         Mass_Controls (I).Btn_Less.Set_Label ("-");

         Mass_Controls (I).Btn_Less.Set_Style (Button_Action);

         Mass_Controls (I).Btn_Less.Set_Font_Size (0.7, 0.5);

         Allocation.X := Allocation.X + Allocation.W + M;

         Mass_Controls (I).Btn_Less.Set_Allocation (Allocation);

      end loop;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Flight.Aircraft;
      use Utility.Strings;
      use Utility.Units;

      X : Float;
      Y : Float;
      C : Line_Color_Record := Line_Cyan;

   begin

    --Btn_Model.Set_Label (Trim (This_Aircraft.Model) & "/" & Trim (This_Aircraft.Registration));

    --Btn_Model.Draw;

      -- Mass panel
      --------------------------------------------------------------------------
      Pnl_Mass.Draw;

      for I in Mass_Point_Range loop

         if This_Aircraft.Mass_Points (I).Active then

            if Mass_Focus = I then

               Mass_Controls (I).Btn_Mass.Set_Style (Button_Focus);

            else

               Mass_Controls (I).Btn_Mass.Set_Style (Button_Alive);

            end if;

            Mass_Controls (I).Btn_Mass.Set_Label (Trim (Integer'Image (Integer (This_Aircraft.Mass_Points (I).Mass))));

            Mass_Controls (I).Btn_Mass.Set_Label_Color (Line_Cyan);

            if
              (This_Aircraft.Mass_Points (I).Mass > This_Aircraft.Mass_Points (I).Mass_Max or else
               This_Aircraft.Mass_Points (I).Mass < This_Aircraft.Mass_Points (I).Mass_Min) and then Blink
            then

               Mass_Controls (I).Btn_Mass.Set_Label_Color (Line_Red);

            end if;

            Y := Mass_Controls (I).Btn_Mass.Get_Allocation.Y + 0.5 * (Mass_Controls (I).Btn_Mass.Get_Allocation.H - Font_1.Height);

            X := 2.0 * M;

            Glex.Fonts.Draw (Trim (This_Aircraft.Mass_Points (I).Label),
                             X,
                             Y,
                             Font_1,
                             Line_White);

            X := Mass_Controls (I).Btn_Mass.Get_Allocation.X + Mass_Controls (I).Btn_Mass.Get_Allocation.W + M;

            Glex.Fonts.Draw ("KG",
                             X,
                             Y,
                             Font_3,
                             Line_Grass);

            Mass_Controls (I).Btn_Mass.Draw;

            Mass_Controls (I).Btn_Plus.Draw;

            Mass_Controls (I).Btn_Less.Draw;

         end if;

      end loop;

      Y := Pnl_Mass.Get_Allocation.Y + 2.0 * M;

      if This_Aircraft.Total_Mass > This_Aircraft.Maximum_Mass and Blink then

         C := Line_Red;

      end if;

      Glex.Fonts.Draw ("TOTAL = " & Float_Image (This_Aircraft.Total_Mass, 0),
                       Pnl_Mass.Get_Allocation.X + M, Y,
                       Font_V,
                       C);

      Glex.Fonts.Draw ("KG",
                       Pnl_Mass.Get_Allocation.X + 0.55 * Pnl_Mass.Get_Allocation.W, Y,
                       Font_3,
                       Line_Grass);

      -- Info panel
      --------------------------------------------------------------------------
      Pnl_Info.Draw;

      X := Pnl_Info.Get_Allocation.X + 0.02;
      Y := Pnl_Info.Get_Allocation.Y + Pnl_Info.Get_Allocation.H - 2.5 * Font_V.Height;

      Glex.Fonts.Draw ("V",
                       X, Y,
                       Font_V,
                       Line_White);

      Glex.Fonts.Draw ("S",
                       X + 0.024, Y - 0.5 * Font_2.Height,
                       Font_2,
                       Line_White);

      Glex.Fonts.Draw (">",
                       X + 0.082, Y,
                       Font_V,
                       Line_White);

      Glex.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_S0, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                       X + 0.190, Y,
                       Font_V,
                       Line_White,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Image (Unit_Kilometer_Hour),
                       X + 0.210, Y + 0.01,
                       Font_3,
                       Line_Grass);

      Y := Y - 2.2 * Font_V.Height;

      Glex.Fonts.Draw ("V",
                       X, Y,
                       Font_V,
                       Line_White);

      Glex.Fonts.Draw ("L",
                       X + 0.024, Y - 0.5 * Font_2.Height,
                       Font_2,
                       Line_White);

      Glex.Fonts.Draw (">",
                       X + 0.082, Y,
                       Font_V,
                       Line_White);

      Glex.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_LND, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                       X + 0.190, Y,
                       Font_V,
                       Line_White,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Image (Unit_Kilometer_Hour),
                       X + 0.210, Y + 0.01,
                       Font_3,
                       Line_Grass);

      Y := Y - 2.2 * Font_V.Height;

      Glex.Fonts.Draw ("V",
                       X, Y,
                       Font_V,
                       Line_Yellow);

      Glex.Fonts.Draw ("NO",
                       X + 0.024, Y - 0.5 * Font_2.Height,
                       Font_2,
                       Line_Yellow);

      Glex.Fonts.Draw (">",
                       X + 0.082, Y,
                       Font_V,
                       Line_Yellow);

      Glex.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_NO, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                       X + 0.190, Y,
                       Font_V,
                       Line_Yellow,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Image (Unit_Kilometer_Hour),
                       X + 0.210, Y + 0.01,
                       Font_3,
                       Line_Grass);

      Y := Y - 2.2 * Font_V.Height;

      Glex.Fonts.Draw ("V",
                       X, Y,
                       Font_V,
                       Line_Red);

      Glex.Fonts.Draw ("NE",
                       X + 0.024, Y - 0.5 * Font_2.Height,
                       Font_2,
                       Line_Red);

      Glex.Fonts.Draw (">",
                       X + 0.082, Y,
                       Font_V,
                       Line_Red);

      Glex.Fonts.Draw (Float_Image (Convert (This_Aircraft.V_NE, Unit_Meter_Second, Unit_Kilometer_Hour), 0),
                       X + 0.190, Y,
                       Font_V,
                       Line_Red,
                       Glex.Fonts.Alignment_LR);

      Glex.Fonts.Draw (Image (Unit_Kilometer_Hour),
                       X + 0.210, Y + 0.01,
                       Font_3,
                       Line_Grass);

      -- Drag factor slider
      --------------------------------------
      Pnl_Drag.Draw;

      Frm_Green.Draw;

      Frm_Yellow.Draw;

      Frm_Red.Draw;

      Glex.Fonts.Draw (">",
                       Pnl_Drag.Get_Allocation.X + 0.025, Drag_Slider_Min + (This_Aircraft.Drag_Factor) / Drag_Slider_Range * Drag_Slider_Delta,
                       Font_V,
                       Line_White,
                       Glex.Fonts.Alignment_CL);

      -- CL/CD_max
      --------------------------------------
      Y := Y - 2.2 * Font_V.Height;

      Glex.Fonts.Draw ("CL/CD",
                       X, Y,
                       Font_V,
                       Line_White);

      Glex.Fonts.Draw ("MAX",
                       X + 0.135, Y - 0.5 * Font_2.Height,
                       Font_2,
                       Line_Gray);

      Glex.Fonts.Draw (Float_Image (This_Aircraft.Cl_Cd_Max, 0),
                       X + 0.240, Y,
                       Font_V,
                       Line_White,
                       Glex.Fonts.Alignment_LC);

      Glex.Fonts.Draw (Float_Image (100.0 * This_Aircraft.Drag_Factor, 0),
                       X + 0.335, 0.06,
                       Font_2,
                       Line_Gray,
                       Glex.Fonts.Alignment_LC);

      -- Change button
      --------------------------------------
      Btn_Next.Draw;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      use Flight.Aircraft;
      use Utility.Strings;

   begin

      if Btn_Next.Contains (X, Y) then

         Next_Aircraft;

       --Btn_Model.Set_Label (Trim (This_Aircraft.Model) & "/" & Trim (This_Aircraft.Registration));

         Flight.Aircraft.Modified := True;

         Display.Refresh := True;

      else

         for I in Mass_Point_Range loop

            if This_Aircraft.Mass_Points (I).Active then

               if Mass_Controls (I).Btn_Mass.Contains (X, Y) then

                  Mass_Focus := I;

                  Display.Refresh := True;

                  return;

               elsif Mass_Controls (I).Btn_Plus.Contains (X, Y) then

                  This_Aircraft.Mass_Points (I).Mass := Float'Min (150.0, This_Aircraft.Mass_Points (I).Mass + 1.0);

                  Flight.Aircraft.Recalculate_Mass;

                  Flight.Aircraft.Modified := True;

                  Display.Refresh := True;

                  return;

               elsif Mass_Controls (I).Btn_Less.Contains (X, Y) then

                  This_Aircraft.Mass_Points (I).Mass := Float'Max (0.0, This_Aircraft.Mass_Points (I).Mass - 1.0);

                  Flight.Aircraft.Recalculate_Mass;

                  Flight.Aircraft.Modified := True;

                  Display.Refresh := True;

                  return;

               end if;

            end if;

         end loop;

      end if;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reference   : Float   := 0.0;
   Sliding     : Boolean := False;
   Slider_Step : Float   := 0.02;
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean) is

      S : Float := Y + Dy;

   begin

      if First then

         if Pnl_Drag.Contains (X, Y) then
            Sliding := True;
            Reference := S;
         else
            Sliding := False;
         end if;

      elsif Sliding and then abs (S - Reference) >= Slider_Step then

         if    S < Drag_Slider_Min then --> snap to the minimum
            S := Drag_Slider_Min;
         elsif S > Drag_Slider_Max then --> snap to the maximum
            S := Drag_Slider_Max;
         end if;

         Reference := S;

         This_Aircraft.Drag_Factor := (S - Drag_Slider_Min) / Drag_Slider_Delta * Drag_Slider_Range;

         Flight.Aircraft.Modified := True;

         Refresh := True;

      end if;

   end Screen_Move;


end Display.Pages.Glider;
--------------------------------------------------------------------------------
