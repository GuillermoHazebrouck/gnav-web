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
--AdaWebPack
with Web.GL.Rendering_Contexts;
with Web.GL.Uniform_Locations;
use  Web.GL.Uniform_Locations;
with Web.GL.Shaders;
use  Web.GL.Shaders;
with Web.GL.Programs;
use  Web.GL.Programs;
with Web.Strings;
use  Web.Strings;
-- Gnav
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides an interface to update GPU resources. The buffers are
-- encapsulated and managed by this unit.
--//////////////////////////////////////////////////////////////////////////////
package body Glex.Colormap is

   pragma Warnings (Off);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Shader_Code : constant Web_String := +(
      "attribute vec2 aVertex;" &
      "attribute vec3 aColor;" &
      "uniform   mat4 uMatrix;" &
      "varying   vec3 vColor;" &
      "void main() {" &
      "  vColor = aColor;" &
      "  vec4 Vector = vec4(aVertex.x,aVertex.y,0.0,1.0);" &
      "  gl_Position = uMatrix * Vector;" &
      "}");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Fragment_Shader_Code : constant Web_String := +(
      "precision mediump float;" &
      "uniform vec4 uColor;" &
      "varying vec3 vColor;" &
      "void main() {" &
      "  gl_FragColor = vec4(vColor.x,vColor.y,vColor.z,1.0);" &
      "}");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Number of floats in the vertex data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Element_Data_Size : constant Positive := 3;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Number of bytes in the vertex data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Element_Data_Bytes : constant := GLsizei (Element_Data_Size) * GLushort_Size;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Number of floats in the vertex data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Data_Size  : constant Positive := 5;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Number of bytes in the vertex data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Data_Bytes : constant := GLsizei (Vertex_Data_Size) * GLfloat_Size;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Shader : WebGL_Shader;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Fragment_Shader : WebGL_Shader;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Shader_Program : WebGL_Program;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Matrix_Location : WebGL_Uniform_Location;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Location : Web.GL.GLint;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Color_Location  : Web.GL.GLint;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      --------------------------------------------------------------------------
      -- Vertex shader
      --------------------------------------------------------------------------
      Vertex_Shader := GL.Create_Shader (GL_VERTEX_SHADER);

      GL.Shader_Source  (Vertex_Shader, Vertex_Shader_Code);

      GL.Compile_Shader (Vertex_Shader);

      --------------------------------------------------------------------------
      -- Fragment shader
      --------------------------------------------------------------------------
      Fragment_Shader := GL.Create_Shader (GL_FRAGMENT_SHADER);

      GL.Shader_Source  (Fragment_Shader, Fragment_Shader_Code);

      GL.Compile_Shader (Fragment_Shader);

      --------------------------------------------------------------------------
      -- Create program, attach shaders and link the program
      --------------------------------------------------------------------------
      Shader_Program := GL.Create_Program;

      GL.Attach_Shader (Shader_Program, Fragment_Shader);

      GL.Attach_Shader (Shader_Program, Vertex_Shader);

      Gl.Link_Program  (Shader_Program);

      --------------------------------------------------------------------------
      -- Get uniforms
      --------------------------------------------------------------------------
      Matrix_Location := GL.Get_Uniform_Location (Shader_Program, +"uMatrix");

      Vertex_Location := GL.Get_Attrib_Location  (Shader_Program, +"aVertex");

      Color_Location  := GL.Get_Attrib_Location  (Shader_Program, +"aColor");

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load (This : in out Resource_Type'Class; Buffer : Buffer_Type) is

      Length : GLsizeiptr;

   begin

      if not This.Loaded then

         This.Node_Buffer    := GL.Create_Buffer;

         This.Element_Buffer := GL.Create_Buffer;

         This.Loaded := True;

      end if;

      -- Nodes

      This.Nodes := Buffer.N;

      GL.Bind_Buffer (GL_ARRAY_BUFFER, This.Node_Buffer);

      Length := GLsizeiptr (Buffer.N * Vertex_Data_Bytes);

      GL.Buffer_Data (GL_ARRAY_BUFFER, Length, Buffer.B (Buffer.B'First)'Address, GL_STATIC_DRAW);

      -- Elements

      This.Elements := 3 * Buffer.E;

      if This.Elements > 0 then

         GL.Bind_Buffer (GL_ELEMENT_ARRAY_BUFFER, This.Element_Buffer);

         Length := GLsizeiptr (Buffer.E * Element_Data_Bytes);

         GL.Buffer_Data (GL_ELEMENT_ARRAY_BUFFER, Length, Buffer.Q (Buffer.Q'First)'Address, GL_STATIC_DRAW);

      end if;

   exception
      when others =>
         Utility.Log.Put_Message ("error while loading basic buffer");

   end Load;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : Resource_Type'Class) is
   begin

      if This.Nodes > 0 then

         GL.Bind_Buffer (GL_ARRAY_BUFFER, This.Node_Buffer);

         GL.Vertex_Attrib_Pointer (Web.GL.GLuint (Vertex_Location), 2, Web.GL.Rendering_Contexts.FLOAT, False, Vertex_Data_Bytes, 0);

         GL.Enable_Vertex_Attrib_Array (Web.GL.GLuint (Vertex_Location));

         GL.Vertex_Attrib_Pointer (Web.GL.GLuint (Color_Location),  3, Web.GL.Rendering_Contexts.FLOAT, False, Vertex_Data_Bytes, GLintptr (2 * GLfloat_Size));

         GL.Enable_Vertex_Attrib_Array (Web.GL.GLuint (Color_Location));

         if This.Elements > 0 then

            GL.Use_Program (Shader_Program);

            GL.Uniform_Matrix_4fv (Matrix_Location, False, Transform.Matrix);

            GL.Bind_Buffer (GL_ELEMENT_ARRAY_BUFFER, This.Element_Buffer);

            GL.Draw_Elements (GL_TRIANGLES, GLsizei (This.Elements), Web.GL.Rendering_Contexts.UNSIGNED_SHORT, 0);

         else

            GL.Use_Program (Shader_Program);

            GL.Uniform_Matrix_4fv (Matrix_Location, False, Transform.Matrix);

            GL.Draw_Arrays (GL_TRIANGLES, 0, GLsizei (This.Nodes));

         end if;

      end if;

   exception
      when others =>
         Utility.Log.Put_Message ("error while updating GPU resources");

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function New_Buffer (Nodes : Positive; Elements : Positive) return Buffer_Type is

      Buffer : Buffer_Type (Vertex_Data_Size * Nodes, Element_Data_Size * Elements);

   begin

      Buffer.N := 0;
      Buffer.E := 0;
      Buffer.B := (others => 0.0);
      Buffer.Q := (others => 0);

      return Buffer;

   end New_Buffer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Node (This : in out Buffer_Type'Class; X, Y, R, G, B : Float) is
   begin

      if 5 * This.N + 5 <= This.B'Last then

         This.N := This.N + 1;

         This.B (5 * This.N - 4) := GlFloat (X);
         This.B (5 * This.N - 3) := GlFloat (Y);
         This.B (5 * This.N - 2) := GlFloat (R);
         This.B (5 * This.N - 1) := GlFloat (G);
         This.B (5 * This.N    ) := GlFloat (B);

      else
         Utility.Log.Put_Message ("nodes buffer overflow");

      end if;

   end Load_Node;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Element (This : in out Buffer_Type'Class; N1, N2, N3 : Natural) is
   begin

      if 3 * This.E + 3 <= This.Q'Last then

         This.E := This.E + 1;

         This.Q (3 * This.E - 2) := Glushort (N1);
         This.Q (3 * This.E - 1) := Glushort (N2);
         This.Q (3 * This.E    ) := Glushort (N3);

      else
         Utility.Log.Put_Message ("elements buffer overflow");

      end if;

   end Load_Element;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Copy_Node (This : in out Buffer_Type'Class) is
      begin

      if This.N > 1 and 5 * This.N <= This.B'Last then

         This.N := This.N + 1;

         This.B (5 * This.N - 4) := This.B (5 * This.N - 9);
         This.B (5 * This.N - 3) := This.B (5 * This.N - 8);
         This.B (5 * This.N - 2) := This.B (5 * This.N - 7);
         This.B (5 * This.N - 1) := This.B (5 * This.N - 6);
         This.B (5 * This.N    ) := This.B (5 * This.N - 5);

      end if;

   end Copy_Node;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Reset (This : in out Buffer_Type'Class) is
   begin

      This.N := 0;
      This.E := 0;

   end Reset;
   -----------------------------------------------------------------------------


end Glex.Colormap;
--------------------------------------------------------------------------------
