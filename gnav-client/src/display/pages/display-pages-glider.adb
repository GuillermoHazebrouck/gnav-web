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
with Glex.Symbols;
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

   Font_4 : Glex.Fonts.Font_Style_Record := (Width     => 0.008,
                                             Height    => 0.025,
                                             Space     => 0.004,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Thin);

   M : constant Dimension_Float := 0.01;
   H : constant Dimension_Float := 0.10;
   W : constant Dimension_Float := 0.15;

   -- Page widgets
   ---------------------------------

   Btn_Next   : Button_Record;

   Pnl_Mass   : Panel_Record;

   Pnl_Load   : Panel_Record;

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
   Drag_Slider_Min   : constant Float := 0.09;
   Drag_Slider_Max   : constant Float := 0.73;
   Drag_Slider_Delta : constant Float := Drag_Slider_Max - Drag_Slider_Min;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Slider_Range : constant Float := 150.0;
   Mass_Slider_Min   : constant Float := 0.09;
   Mass_Slider_Max   : constant Float := 0.73;
   Mass_Slider_Delta : constant Float := Mass_Slider_Max - Mass_Slider_Min;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Mass_Controls : array (Mass_Point_Range) of Button_Record;

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

      Allocation.X := 0.52;
      Allocation.Y := 0.01;
      Allocation.H := 0.85;
      Allocation.W := 0.47;

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
      Allocation.W := 0.50;

      Pnl_Mass.Set_Allocation (Allocation);

      Pnl_Mass.Set_Background_Color (Color_Gray_1);

      Pnl_Mass.Set_Font_Size (0.03, 0.25);

      Pnl_Mass.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Mass.Set_Label ("MASS");

      -- Load panel
      ------------------------------------------------------

      Allocation.X := 0.40;
      Allocation.Y := 0.04;
      Allocation.H := 0.76;
      Allocation.W := 0.09;

      Pnl_Load.Set_Allocation (Allocation);

      Pnl_Load.Set_Background_Color (Color_Gray_3);

      Pnl_Load.Set_Font_Size (0.03, 0.25);

      Pnl_Load.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      Pnl_Load.Set_Label ("LOAD");

      -- Drag panel
      ------------------------------------------------------

      Allocation.X := 0.89;
      Allocation.Y := 0.04;
      Allocation.H := 0.76;
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

      Allocation.X := 0.02;
      Allocation.Y := 0.04;
      Allocation.W := 0.09;
      Allocation.H := H;

      Btn_Next.Set_Allocation (Allocation);

      Btn_Next.Set_Symbol (Glex.Symbols.Triangle_Right);

      Btn_Next.Set_Style (Button_Action);

      Btn_Next.Set_Font_Size (0.6, 0.4);

      -- Mass selection
      ------------------------------------------------------

      Allocation.Y := Pnl_Mass.Get_Allocation.Y + Pnl_Mass.Get_Allocation.H - 0.06;

      for I in Flight.Aircraft.Mass_Point_Range loop

         Mass_Controls (I).Set_Style (Button_Alive);

         Mass_Controls (I).Set_Font_Size (0.6, 0.3);

         Allocation.X := (Font_1.Width + Font_1.Space) * Float (String_12'Length) + M;

         Allocation.Y := Allocation.Y - (H + 2.0 * M);

         Allocation.W := 0.12;

         Allocation.H := H;

         Mass_Controls (I).Set_Allocation (Allocation);

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

      -- Mass panel
      --------------------------------------------------------------------------
      Pnl_Mass.Draw;

      for I in Mass_Point_Range loop

         if This_Aircraft.Mass_Points (I).Active then

            if Mass_Focus = I then

               Mass_Controls (I).Set_Style (Button_Focus);

            else

               Mass_Controls (I).Set_Style (Button_Alive);

            end if;

            Mass_Controls (I).Set_Label (Trim (Integer'Image (Integer (This_Aircraft.Mass_Points (I).Mass))));

            Mass_Controls (I).Set_Label_Color (Line_Cyan);

            if
              (This_Aircraft.Mass_Points (I).Mass > This_Aircraft.Mass_Points (I).Mass_Max or else
               This_Aircraft.Mass_Points (I).Mass < This_Aircraft.Mass_Points (I).Mass_Min) and then Blink
            then

               Mass_Controls (I).Set_Label_Color (Line_Red);

            end if;

            Y := Mass_Controls (I).Get_Allocation.Y + 0.5 * (Mass_Controls (I).Get_Allocation.H - Font_1.Height);

            X := 2.0 * M;

            Glex.Fonts.Draw (Trim (This_Aircraft.Mass_Points (I).Label),
                             X,
                             Y,
                             Font_1,
                             Line_White);

            X := Mass_Controls (I).Get_Allocation.X + Mass_Controls (I).Get_Allocation.W + M;

            Glex.Fonts.Draw ("KG",
                             X,
                             Y,
                             Font_3,
                             Line_Grass);

            Mass_Controls (I).Draw;

         end if;

      end loop;

      Y := Y - 0.15;

      if This_Aircraft.Total_Mass > This_Aircraft.Maximum_Mass and Blink then

         C := Line_Red;

      end if;

      Glex.Fonts.Draw ("TOTAL = " & Float_Image (This_Aircraft.Total_Mass, 0),
                       Pnl_Mass.Get_Allocation.X + M, Y,
                       Font_V,
                       C);

      Glex.Fonts.Draw ("KG",
                       Pnl_Mass.Get_Allocation.X + 0.62 * Pnl_Mass.Get_Allocation.W, Y,
                       Font_3,
                       Line_Grass);

      -- Info panel
      --------------------------------------------------------------------------
      Pnl_Info.Draw;

      X := Pnl_Info.Get_Allocation.X + 0.05;
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

      Glex.Symbols.Draw (Glex.Symbols.Triangle_Right,
                         Pnl_Drag.Get_Allocation.X + 0.015, Drag_Slider_Min + (This_Aircraft.Drag_Factor) / Drag_Slider_Range * Drag_Slider_Delta,
                         0.03,
                         Color_White,
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

      -- Drag factor slider
      --------------------------------------

      if This_Aircraft.Mass_Points (Mass_Focus).Active then

         Pnl_Load.Draw;

         for I in 0..3 loop

            Glex.Fonts.Draw (Integer_Image (I * 50),
                             Pnl_Load.Get_Allocation.X + 0.08,
                             Mass_Slider_Min + Float (I) * 50.0 / Mass_Slider_Range * Mass_Slider_Delta,
                             Font_4,
                             Line_White,
                             Glex.Fonts.Alignment_CR);

         end loop;

         Glex.Symbols.Draw (Glex.Symbols.Triangle_Right,
                            Pnl_Load.Get_Allocation.X + 0.015, Mass_Slider_Min + This_Aircraft.Mass_Points (Mass_Focus).Mass / Mass_Slider_Range * Mass_Slider_Delta,
                            0.03,
                            Color_White,
                            Glex.Fonts.Alignment_CL);

      end if;

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

               if Mass_Controls (I).Contains (X, Y) then

                  Mass_Focus := I;

                  Display.Refresh := True;

                  return;

               end if;

            end if;

         end loop;

      end if;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Sliding_Drag  : Boolean := False;
   Sliding_Mass  : Boolean := False;
   Slider_Step   : constant Float := 0.01;
   Slider_Drag_S : Float   := 0.0;
   Slider_Mass_S : Float   := 0.0;
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Move (X, Y, Dx, Dy : Float; First : Boolean) is

      S : Float := Y + Dy;

   begin

      if First then

         Sliding_Drag := False;
         Sliding_Mass := False;

         if    Pnl_Drag.Contains (X, Y) then

            Sliding_Drag  := True;
            Slider_Drag_S := S;

         elsif Pnl_Load.Contains (X, Y) then

            Sliding_Mass  := True;
            Slider_Mass_S := S;

         end if;

      elsif Sliding_Drag and then abs (S - Slider_Drag_S) >= Slider_Step then

         if    S < Drag_Slider_Min then --> snap to the minimum
            S := Drag_Slider_Min;
         elsif S > Drag_Slider_Max then --> snap to the maximum
            S := Drag_Slider_Max;
         end if;

         Slider_Drag_S := S;

         This_Aircraft.Drag_Factor := (S - Drag_Slider_Min) / Drag_Slider_Delta * Drag_Slider_Range;

         Flight.Aircraft.Modified := True;

         Refresh := True;

      elsif
        This_Aircraft.Mass_Points (Mass_Focus).Active and then
        Sliding_Mass and then abs (S - Slider_Mass_S) >= Slider_Step
      then

         if    S < Mass_Slider_Min then --> snap to the minimum
            S := Drag_Slider_Min;
         elsif S > Mass_Slider_Max then --> snap to the maximum
            S := Drag_Slider_Max;
         end if;

         Slider_Mass_S := S;

         This_Aircraft.Mass_Points (Mass_Focus).Mass := (S - Mass_Slider_Min) / Mass_Slider_Delta * Mass_Slider_Range;

         Flight.Aircraft.Recalculate_Mass;

         Flight.Aircraft.Modified := True;

         Refresh := True;

      end if;

   end Screen_Move;
   -----------------------------------------------------------------------------

end Display.Pages.Glider;
--------------------------------------------------------------------------------
