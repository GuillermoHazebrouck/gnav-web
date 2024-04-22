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
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
-- Gnav
with Flight;
with Flight.Plan;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Glex.Lines;
with Glex.Basic;
with Utility.Strings;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Compass is

   -- Needle vertex ids
   ---------------------------

   Waypoint  : Glex.Basic.Resource_Type;

   North     : Glex.Basic.Resource_Type;

   -- Quadrant vertex data
   ---------------------------

   Quadrant  : Glex.Lines.Resource_Type;

   Reference : Glex.Lines.Resource_Type;

   Rosetta   : Glex.Lines.Resource_Type;

   Quadrant_Size : constant Natural := 36;

   Allocation : Allocation_Record;

   -- Fonts
   ---------------------------------
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.020,
                                             Height    => 0.060,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      S     : constant Float := 2.0 * Pi / Float (Quadrant_Size);
      Angle : Float := 0.0;

      Lines_Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (Quadrant_Size+1);
      Basic_Buffer : Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (10);

   begin

      -- Load quadrant buffer in GPU
      --------------------------------------------------------------------------

      for I in 0..Quadrant_Size loop

         Lines_Buffer.Line_To (0.5 * Cos (Angle), 0.5 * Sin (Angle));

         Angle := Angle + S;

      end loop;

      Quadrant.Load (Lines_Buffer);

      -- Rosetta (N-E-S-W dashes)
      --------------------------------------------------------------------------

      Lines_Buffer.Reset;

      Lines_Buffer.Move_To ( 0.55, 0.00);
      Lines_Buffer.Line_To ( 0.40, 0.00);

      Lines_Buffer.Move_To (-0.55, 0.00);
      Lines_Buffer.Line_To (-0.40, 0.00);

      Lines_Buffer.Move_To ( 0.00, 0.55);
      Lines_Buffer.Line_To ( 0.00, 0.40);

      Lines_Buffer.Move_To ( 0.00,-0.55);
      Lines_Buffer.Line_To ( 0.00,-0.40);

      Rosetta.Load (Lines_Buffer);

      -- Reference
      --------------------------------------------------------------------------

      Lines_Buffer.Reset;

      Lines_Buffer.Move_To ( 0.00, 0.20);
      Lines_Buffer.Line_To ( 0.00, 0.45);

      Reference.Load (Lines_Buffer);

      -- Load north arrow
      --------------------------------------------------------------------------

      Basic_Buffer.Load_Node ( 0.00, 0.60);
      Basic_Buffer.Load_Node ( 0.10, 0.45);
      Basic_Buffer.Load_Node (-0.10, 0.45);

      North.Load (Basic_Buffer);

      -- Load waypoint arrow
      --------------------------------------------------------------------------

      Basic_Buffer.Reset;

      Basic_Buffer.Load_Node ( 0.00, 0.50);
      Basic_Buffer.Load_Node ( 0.07, 0.34);
      Basic_Buffer.Load_Node (-0.07, 0.34);

      Waypoint.Load (Basic_Buffer);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw (X, Y, Size: Float) is

      use Glex.Basic;
      use Glex.Lines;

      use Flight;
      use Ada.Numerics;

      Transform : Glex.Transform_Record := Glex.Get_Transform.all;
      Course    : Float := 0.0;
      Angle     : Float;

   begin

      if Flight.Data.Is_Recent (Field_Course) then

         Course := Flight.Data.Course * Pi / 180.0;

      end if;

      Glex.Get_Transform.Translate (X, Y);
      Glex.Get_Transform.Scale     (Size, Size * Aspect);
      Glex.Get_Transform.Rotate    (Course);

      Quadrant.Draw (Line_White, 0.02, 0.01, 1.0, True);

      -- Reference mark
      --------------------------------------------------------------------------

      Glex.Get_Transform.Copy      (Transform);
      Glex.Get_Transform.Translate (X, Y);
      Glex.Get_Transform.Scale     (Size, Size * Aspect);

      Reference.Draw (Line_Cyan, 0.02, 0.01);

      -- N-E-S-W marks
      --------------------------------------------------------------------------

      Glex.Get_Transform.Rotate (Course);

      Rosetta.Draw (Line_White, 0.02, 0.01);

      -- North arrow
      --------------------------------------------------------------------------

      North.Draw (Color_White, Triangles);
      North.Draw (Color_Black, Line_Loop);

      if Flight.Data.Is_Recent (Field_Course) then

         -- Home arrow
         -----------------------------------------------------------------------

         Angle := Flight.Data.Course - Flight.Plan.Home_Waypoint.Bearing;

         if Angle < 0.0 then

            Angle := 360.0 + Angle;

         end if;

         Angle := Angle * Pi / 180.0;

         Glex.Get_Transform.Copy      (Transform);
         Glex.Get_Transform.Translate (X, Y);
         Glex.Get_Transform.Scale     (Size, Size * Aspect);
         Glex.Get_Transform.Rotate    (Angle);

         Waypoint.Draw (Color_Green, Triangles);
         Waypoint.Draw (Color_Grass, Line_Loop);

         -- Waypoint arrow
         -----------------------------------------------------------------------

         Angle := Flight.Data.Course - Flight.Plan.Next_Waypoint.Bearing;

         if Angle < 0.0 then

            Angle := 360.0 + Angle;

         end if;

         Angle := Angle * Pi / 180.0;

         Glex.Get_Transform.Copy      (Transform);
         Glex.Get_Transform.Translate (X, Y);
         Glex.Get_Transform.Scale     (Size, Size * Aspect);
         Glex.Get_Transform.Rotate    (Angle);

         Waypoint.Draw (Color_Magenta, Triangles);
         Waypoint.Draw (Color_Purple,  Line_Loop);

      end if;

      -- Course indicator
      --------------------------------------------------------------------------

      Glex.Get_Transform.Copy (Transform);

      Font_1.Height := 0.30 * Size;
      Font_1.Width  := 0.35 * Font_1.Height;

      if Flight.Data.Is_Recent (Field_Course) then

         Glex.Fonts.Draw (Utility.Strings.Float_Image (Flight.Data.Course, 0),
                          X,
                          Y,
                          Font_1,
                          Line_Cyan,
                          Alignment_CC);

      else

         Glex.Fonts.Draw ("-",
                          X,
                          Y,
                          Font_1,
                          Line_Red,
                          Alignment_CC);

      end if;

      -- Allocation
      --------------------------------------------------------------------------

      Allocation.X := X - 0.5 * Size;
      Allocation.W := Size;
      Allocation.Y := Y - 0.5 * Size * Aspect;
      Allocation.H := Size * Aspect;

   end Draw;
   -----------------------------------------------------------------------------

end Display.Compass;
--------------------------------------------------------------------------------
