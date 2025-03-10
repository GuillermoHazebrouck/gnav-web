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
--Standard
with Interfaces;
with Ada.Numerics.Generic_Elementary_Functions;
--AdaWebPack
with Web.Gl;
use  Web.Gl;
with Web.Gl.Rendering_Contexts;
use  Web.Gl.Rendering_Contexts;
with Web.Strings;

--//////////////////////////////////////////////////////////////////////////////
-- Glex is a wrapper on top of the WebGL infrastructure. This extension provides
-- an organized way of dealing with predefined shaders.
--//////////////////////////////////////////////////////////////////////////////
package Glex is

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   --
   --===========================================================================
   procedure Clear_Screen;

   --///////////////////////////////////////////////////////////////////////////
   -- Suplementaty types and transformations
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Transform_Record is tagged private;

   --===========================================================================
   -- Loads the identity clip matrix.
   --===========================================================================
   procedure Load_Identity (This : in out Transform_Record);

   --===========================================================================
   -- Loads a clip matrix where the width and height are unitary
   --===========================================================================
   procedure Load_Unit (This : in out Transform_Record);

   --===========================================================================
   -- Loads a conformal clip matrix where the with is unitary
   --===========================================================================
   procedure Load_Conformal (This : in out Transform_Record);

   --===========================================================================
   -- Postmultiplies the matrix by a scaling matrix
   --===========================================================================
   procedure Scale (This : in out Transform_Record; X, Y : Float);

   --===========================================================================
   -- Postmultiplies the matrix by an offset matrix
   --===========================================================================
   procedure Translate (This : in out Transform_Record; X, Y : Float);

   --===========================================================================
   -- Rotates 90 degrees
   --===========================================================================
   procedure Rotate (This : in out Transform_Record);

   --===========================================================================
   -- Postmultiplies the matrix by a flat rotation matrix about the Z axis
   -- NOTE: the angle must be in radians.
   --===========================================================================
   procedure Rotate (This : in out Transform_Record; A : Float);

   --===========================================================================
   -- Copies the transformation.
   --===========================================================================
   procedure Copy (This : in out Transform_Record; Other : Transform_Record);

   --===========================================================================
   -- Returns the matrix product
   --===========================================================================
   function Multiply (T1, T2 : Transform_Record) return Transform_Record;

   --===========================================================================
   -- Returns the current transformation
   --===========================================================================
   function Get_Transform return access Transform_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the screen goes vertical
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertical : Boolean := True;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The aspect of the screen (display width/height ratio)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Aspect : Float := 1.0;

private

   function "+" (Item : Wide_Wide_String) return Web.Strings.Web_String
                    renames Web.Strings.To_Web_String;


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Size in bytes of the single float
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GLfloat_Size : constant GLsizei := Web.GL.GLfloat'Max_Size_In_Storage_Elements;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Internal type used for vertex buffers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type GLfloat_Vector is array (Positive range <>) of GLfloat;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Internal type used for element buffers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type GLushort is new Interfaces.Unsigned_16;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Internal array used for element buffers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type GLushort_Vector is array (Positive range <>) of GLushort;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Size in bytes of the Unsigned short
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GLushort_Size : constant GLsizei := GLushort'Max_Size_In_Storage_Elements;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Identity matrix
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Gl_Mat_4_Identity : constant GLfloat_Matrix_4x4 := ((1.0, 0.0, 0.0, 0.0),
                                                       (0.0, 1.0, 0.0, 0.0),
                                                       (0.0, 0.0, 1.0, 0.0),
                                                       (0.0, 0.0, 0.0, 1.0));

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Transform_Record is tagged record
      Matrix : GLfloat_Matrix_4x4 := Gl_Mat_4_Identity;
   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current transformation
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Transform : aliased Transform_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The WebGL context
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL : WebGL_Rendering_Context;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   GL_FLOAT                : constant := Web.GL.Rendering_Contexts.FLOAT;
   GL_POINTS               : constant := Web.GL.Rendering_Contexts.POINTS;
   GL_LINES                : constant := Web.GL.Rendering_Contexts.LINES;
   GL_LINE_STRIP           : constant := Web.GL.Rendering_Contexts.LINE_STRIP;
   GL_LINE_LOOP            : constant := Web.GL.Rendering_Contexts.LINE_LOOP;
   GL_TRIANGLE_STRIP       : constant := Web.GL.Rendering_Contexts.TRIANGLE_STRIP;
   GL_TRIANGLE_FAN         : constant := Web.GL.Rendering_Contexts.TRIANGLE_FAN;
   GL_TRIANGLES            : constant := Web.GL.Rendering_Contexts.TRIANGLES;
   GL_ARRAY_BUFFER         : constant := Web.GL.Rendering_Contexts.ARRAY_BUFFER;
   GL_ELEMENT_ARRAY_BUFFER : constant := Web.GL.Rendering_Contexts.ELEMENT_ARRAY_BUFFER;
   GL_VERTEX_SHADER        : constant := Web.GL.Rendering_Contexts.VERTEX_SHADER;
   GL_FRAGMENT_SHADER      : constant := Web.GL.Rendering_Contexts.FRAGMENT_SHADER;
   GL_STATIC_DRAW          : constant := Web.GL.Rendering_Contexts.STATIC_DRAW;
   GL_BLEND                : constant := 16#0BE2#;

   --===========================================================================
   -- Displays the content in the standard output
   --===========================================================================
   procedure Dump (Matrix : GLfloat_Matrix_4x4);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Math on floating point numbers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Math is new Ada.Numerics.Generic_Elementary_Functions (GLfloat);

   --///////////////////////////////////////////////////////////////////////////
   -- Small math unit to deal with the buffer vectors en vertex data
   --///////////////////////////////////////////////////////////////////////////
   type Vector_Record is record
      X : GLfloat;
      Y : GLfloat;
   end record;

   function Norm (V : Vector_Record) return GLfloat;
   pragma Inline (Norm);

   procedure Unit (V : in out Vector_Record);
   pragma Inline (Unit);

   procedure Oppose (V : in out Vector_Record);
   pragma Inline (Oppose);

   procedure Flip (V : in out Vector_Record);
   pragma Inline (Flip);

   procedure Scale (V : in out Vector_Record; S : GLfloat);
   pragma Inline (Scale);

   procedure Rotate (V : in out Vector_Record; R : GLfloat);
   pragma Inline (Rotate);

   function Image (V : Vector_Record) return String;
   pragma Inline (Image);

   function "+" (V1, V2 : Vector_Record) return Vector_Record;
   pragma Inline ("+");

   function "*" (V1, V2 : Vector_Record) return GLfloat;
   pragma Inline ("*");

   function "/" (V1, V2 : Vector_Record) return GLfloat;
   pragma Inline ("/");

   function "-" (V1, V2 : Vector_Record) return Vector_Record;
   pragma Inline ("-");

   --///////////////////////////////////////////////////////////////////////////

end Glex;
--------------------------------------------------------------------------------
