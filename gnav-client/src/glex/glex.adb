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
-- AdaWebPack
with Web.HTML.Canvases;
with Web.HTML.Scripts;
with Web.Window;
-- Standard
with Ada.Numerics.Generic_Elementary_Functions;
-- Gnav
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Glex is

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      Glex.GL := Web.Window.Document.Get_Element_By_Id (+"glcanvas").As_HTML_Canvas.Get_Context (+"webgl");
      Glex.GL.Clear_Color (0.0, 0.0, 0.0, 1.0);
    --Glex.GL.Enable (GL_BLEND);
    --Glex.GL.Blend_Function (SRC_ALPHA, ONE_MINUS_SRC_ALPHA);
      Glex.GL.Clear (Web.GL.Rendering_Contexts.COLOR_BUFFER_BIT);
      Glex.Get_Transform.Load_Unit;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Clear_Screen is
   begin

      Glex.GL.Clear_Color (0.0, 0.0, 0.0, 1.0);
    --Glex.GL.Enable (GL_BLEND);
      Glex.GL.Clear (Web.GL.Rendering_Contexts.COLOR_BUFFER_BIT);
      Glex.Transform.Load_Unit;

   end Clear_Screen;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Multiply (M1, M2 : GLfloat_Matrix_4x4) return GLfloat_Matrix_4x4 is

      Result : GLfloat_Matrix_4x4;
      Sum    : GlFloat;

   begin

      for I in 1..4 loop
         for J in 1..4 loop
            Sum := 0.0;
            for K in 1..4 loop
               Sum := Sum + M1 (I, K) * M2 (K, J);
            end loop;
            Result (I, J) := Sum;
         end loop;
      end loop;

      return Result;

   end Multiply;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Identity (This : in out Transform_Record) is
   begin

      This.Matrix := Gl_Mat_4_Identity;

   end Load_Identity;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Unit (This : in out Transform_Record) is
   begin

      This.Load_Identity;
      if Vertical then
         This.Rotate;
      end if;
      This.Translate (-1.0, -1.0);
      This.Scale     ( 2.0,  2.0);

   end Load_Unit;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Conformal (This : in out Transform_Record) is
   begin

      This.Load_Unit;
      This.Scale (1.0, Aspect);

   end Load_Conformal;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Scale (This : in out Transform_Record; X, Y : Float) is

      Scaling : Transform_Record;

   begin

      Scaling.Matrix (1, 1) := GLfloat (X);
      Scaling.Matrix (2, 2) := GLfloat (Y);
      Scaling.Matrix (3, 3) := 1.0;

      This.Matrix := Multiply (This.Matrix, Scaling.Matrix);

   end Scale;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Translate (This : in out Transform_Record; X, Y : Float) is

      Offset : Transform_Record;

   begin

      Offset.Matrix (1, 4) := GLfloat (X);
      Offset.Matrix (2, 4) := GLfloat (Y);
      Offset.Matrix (3, 4) := 0.0;

      This.Matrix := Multiply (This.Matrix, Offset.Matrix);

   end Translate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Postmultiplies Matrix by a flat rotation matrix about the Z axis
   --===========================================================================
   procedure Rotate (This : in out Transform_Record; A : Float) is

      Rotatation : Transform_Record;

      C : GLfloat := Math.Cos (GLfloat (A));
      S : GLfloat := Math.Sin (GLfloat (A));

   begin

      Rotatation.Matrix (1, 1) := C;
      Rotatation.Matrix (1, 2) :=-S;
      Rotatation.Matrix (2, 1) := S;
      Rotatation.Matrix (2, 2) := C;

      This.Matrix := Multiply (This.Matrix, Rotatation.Matrix);

   end Rotate;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Postmultiplies Matrix by a flat rotation matrix about the Z axis
   --===========================================================================
   procedure Rotate (This : in out Transform_Record) is

      Rotatation : Transform_Record;

      C : GLfloat := 0.0;
      S : GLfloat :=-1.0;

   begin

      Rotatation.Matrix (1, 1) := C;
      Rotatation.Matrix (1, 2) :=-S;
      Rotatation.Matrix (2, 1) := S;
      Rotatation.Matrix (2, 2) := C;

      This.Matrix := Multiply (This.Matrix, Rotatation.Matrix);

   end Rotate;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Copies the transformation.
   --===========================================================================
   procedure Copy (This : in out Transform_Record; Other : Transform_Record) is
   begin

      This.Matrix := Other.Matrix;

   end Copy;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Multiply (T1, T2 : Transform_Record) return Transform_Record is

      Result : Transform_Record;

   begin

      Result.Matrix := Multiply (T1.Matrix, T2.Matrix);

      return Result;

   end Multiply;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Transform return access Transform_Record is
   begin

      return Transform'Access;

   end Get_Transform;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Displays the content in the standard output
   --===========================================================================
   procedure Dump (Matrix : GLfloat_Matrix_4x4) is

   begin
      for I in 1..4 loop

         Utility.Log.Put_Message (GLfloat'Image (Matrix (I, 1)));
         Utility.Log.Put_Message (GLfloat'Image (Matrix (I, 2)));
         Utility.Log.Put_Message (GLfloat'Image (Matrix (I, 3)));
         Utility.Log.Put_Message (GLfloat'Image (Matrix (I, 4)));

      end loop;

      Utility.Log.Put_Message ("");

   end Dump;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Log_Trace (Message : String) is
   begin

      Utility.Log.Put_Message ("OpenGL " & Message);

   end Log_Trace;
   -----------------------------------------------------------------------------




   --///////////////////////////////////////////////////////////////////////////
   -- Small math unit to deal with the buffer vectors en vertex data
   --///////////////////////////////////////////////////////////////////////////

   function Norm (V : Vector_Record) return GLfloat is
   begin
      return Math.Sqrt (V.X * V.X + V.Y * V.Y);
   end Norm;
   pragma Inline (Norm);

   procedure Unit (V : in out Vector_Record) is
      N : GLfloat := Norm (V);
   begin
      if N > 0.0 then
         V.X := V.X / N;
         V.Y := V.Y / N;
      end if;
   end Unit;
   pragma Inline (Unit);

   procedure Oppose (V : in out Vector_Record) is
   begin
      V.X := -V.X;
      V.Y := -V.Y;
   end Oppose;
   pragma Inline (Oppose);

   procedure Flip (V : in out Vector_Record) is
      X : GLfloat := V.X;
   begin
      V.X :=-V.Y;
      V.Y := X;
   end Flip;
   pragma Inline (Flip);

   procedure Scale (V : in out Vector_Record; S : GLfloat) is
   begin
      V.X := S * V.X;
      V.Y := S * V.Y;
   end Scale;
   pragma Inline (Scale);

   procedure Rotate (V : in out Vector_Record; R : GLfloat) is
      S : GLfloat := Math.Sin (R);
      C : GLfloat := Math.Cos (R);
      X : GLfloat := V.X;
   begin
      V.X := C * X - S * V.Y;
      V.Y := S * X + C * V.Y;
   end Rotate;
   pragma Inline (Rotate);

   function Image (V : Vector_Record) return String is
   begin
      return "(" & GLfloat'Image (V.X) & " ;" & GLfloat'Image (V.Y) & " )";
   end Image;
   pragma Inline (Image);

   function "+" (V1, V2 : Vector_Record) return Vector_Record is
   begin
      return (V1.X + V2.X, V1.Y + V2.Y);
   end;
   pragma Inline ("+");

   function "*" (V1, V2 : Vector_Record) return GLfloat is
   begin
      return V1.X * V2.X + V1.Y * V2.Y;
   end;
   pragma Inline ("*");

   function "/" (V1, V2 : Vector_Record) return GLfloat is
   begin
      return V1.X * V2.Y - V1.Y * V2.X;
   end;
   pragma Inline ("/");

   function "-" (V1, V2 : Vector_Record) return Vector_Record is
   begin
      return (V1.X - V2.X, V1.Y - V2.Y);
   end;
   pragma Inline ("-");

   type Vertex_Record is record
      P : Vector_Record;
      N : Vector_Record;
      A : GLfloat;
   end record;
   --///////////////////////////////////////////////////////////////////////////


end Glex;
--------------------------------------------------------------------------------
