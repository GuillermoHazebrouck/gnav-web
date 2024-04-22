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
package body Glex.Basic is

   pragma Warnings (Off);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader GLSL function for monochrome
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Shader_Code : constant Web_String := +(
      "attribute vec2 aVertex;" &
      "uniform   mat4 uMatrix;" &
      "void main() {" &
      "  vec4 Vector = vec4(aVertex.x,aVertex.y,0.0,1.0);" &
      "  gl_Position = uMatrix * Vector;" &
      "}");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader GLSL function for monochrome
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Fragment_Shader_Code : constant Web_String := +(
      "precision mediump float;" &
      "uniform vec4 uColor;" &
      "void main() {" &
      "  gl_FragColor = uColor;" &
      "}");

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
   Color_Location : WebGL_Uniform_Location;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Location : Web.GL.GLint;

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

      Color_Location  := GL.Get_Uniform_Location (Shader_Program, +"uColor");

      Vertex_Location := GL.Get_Attrib_Location  (Shader_Program, +"aVertex");

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load (This : in out Resource_Type'Class; Buffer : Buffer_Type) is

      Data_Size : constant := 2 * GLfloat_Size;
      Length    : GLsizeiptr;

   begin

      if not This.Loaded then

         This.Buffer := GL.Create_Buffer;

         This.Loaded := True;

      end if;

      This.Nodes  := Buffer.N;

      Length := GLsizeiptr (Buffer.N * Data_Size);

      GL.Bind_Buffer (GL_ARRAY_BUFFER, This.Buffer);

      GL.Buffer_Data (GL_ARRAY_BUFFER, Length, Buffer.B (Buffer.B'First)'Address, GL_STATIC_DRAW);

   exception
      when others =>
         Utility.Log.Put_Message ("error while loading basic buffer");

   end Load;
   -----------------------------------------------------------------------------



   Mode_Mapping : constant array (Basic_Modes) of GLenum := (Lines          => GL_LINES,
                                                             Line_Strip     => GL_LINE_STRIP,
                                                             Line_Loop      => GL_LINE_LOOP,
                                                             Triangles      => GL_TRIANGLES,
                                                             Triangle_Strip => GL_TRIANGLE_STRIP,
                                                             Triangle_Fan   => GL_TRIANGLE_FAN);

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : Resource_Type'Class; Color : Color_Record; Mode : Basic_Modes) is

      Gl_Color : GLfloat_Vector_4 := (GLfloat (Color.R),
                                      GLfloat (Color.G),
                                      GLfloat (Color.B),
                                      GLfloat (Color.A));

   begin

      if This.Nodes > 0 then

         GL.Bind_Buffer (GL_ARRAY_BUFFER, This.Buffer);

         GL.Vertex_Attrib_Pointer (Web.GL.GLuint (Vertex_Location), 2, Web.GL.Rendering_Contexts.FLOAT, False, 0, 0);

         GL.Enable_Vertex_Attrib_Array (Web.GL.GLuint (Vertex_Location));

         GL.Use_Program (Shader_Program);

         GL.Uniform_Matrix_4fv (Matrix_Location, False, Transform.Matrix);

         GL.Uniform_4fv        (Color_Location,  Gl_Color);

         GL.Draw_Arrays (Mode_Mapping (Mode), 0, GLsizei (This.Nodes));

      end if;

   exception
      when others =>
         Utility.Log.Put_Message ("error while updating GPU resources");

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function New_Buffer (Nodes : Positive) return Buffer_Type is

      Buffer : Buffer_Type (2 * Nodes);

   begin

      Buffer.N := 0;
      Buffer.B := (others => 0.0);

      return Buffer;

   end New_Buffer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Node (This : in out Buffer_Type'Class; X, Y : Float) is
   begin

      if 2 * This.N + 2 <= This.B'Last then

         This.N := This.N + 1;

         This.B (2 * This.N - 1) := GLfloat (X);
         This.B (2 * This.N    ) := GLfloat (Y);

      end if;

   end Load_Node;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Copy_Node (This : in out Buffer_Type'Class) is
      begin

      if This.N > 1 and 2 * This.N <= This.B'Last then

         This.N := This.N + 1;

         This.B (2 * This.N - 1) := This.B (2 * This.N - 3);
         This.B (2 * This.N    ) := This.B (2 * This.N - 2);

      end if;

   end Copy_Node;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Copy_Node (This : in out Buffer_Type'Class; I : Positive) is
      begin

      if I <= This.N then

         This.N := This.N + 1;

         This.B (2 * This.N - 1) := This.B (2 * I - 1);
         This.B (2 * This.N    ) := This.B (2 * I    );

      end if;

   end Copy_Node;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Reset (This : in out Buffer_Type'Class) is
   begin

      This.N := 0;

   end Reset;
   -----------------------------------------------------------------------------


end Glex.Basic;
--------------------------------------------------------------------------------
