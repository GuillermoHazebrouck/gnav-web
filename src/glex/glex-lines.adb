--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Glex
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 17 Nov 2023
--
--------------------------------------------------------------------------------
-- Standard
with Ada.Numerics.Generic_Elementary_Functions;
-- AdaWebPack
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

--******************************************************************************
-- This module of Glex provides basic graphic primitives on a vertex buffer.
--******************************************************************************
package body Glex.Lines is

   pragma Warnings (Off);

   --///////////////////////////////////////////////////////////////////////////
   -- Monocrome 2D shaders
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Vertex shader
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Shader_Code : constant Web_String := +(
      "attribute vec2  aVertex;" &
      "attribute vec2  aExpand;" &
      "uniform   mat4  uMatrix;" &
      "uniform   float uWidth;" &
      "uniform   float uRatio;" &
      "void main() {" &
      "  vec4 Vector = vec4(aVertex.x + uWidth * aExpand.x, aVertex.y + uRatio * uWidth * aExpand.y, 0.0, 1.0);" &
      "  gl_Position = uMatrix * Vector;" &
      "}");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fragment shader
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Fragment_Shader_Code : constant Web_String := +(
      "precision mediump float;" &
      "uniform vec4 uColor;" &
      "void main() {" &
      "  gl_FragColor = uColor;" &
      "}");

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
   -- The transformation
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Matrix_Location : WebGL_Uniform_Location;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The line color
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Color_Location : WebGL_Uniform_Location;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The line width
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Width_Location : WebGL_Uniform_Location;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ration between the width and the height of the canvas
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Ratio_Location : WebGL_Uniform_Location;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vertex_Location : Web.GL.GLint;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Expand_Location : Web.GL.GLint;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 --Alpha_Location : Web.GL.GLint;

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

      Width_Location  := GL.Get_Uniform_Location (Shader_Program, +"uWidth");

      Ratio_Location  := GL.Get_Uniform_Location (Shader_Program, +"uRatio");

      Vertex_Location := GL.Get_Attrib_Location  (Shader_Program, +"aVertex");

      Expand_Location := GL.Get_Attrib_Location  (Shader_Program, +"aExpand");

    --Alpha_Location  := GL.Get_Attrib_Location  (Shader_Program, +"aAlpha");

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function New_Buffer (Segments : Positive) return Buffer_Type is

      Result : Buffer_Type (Vertex_Data_Size * (24 * Segments));

   begin

      Result.N := 0;
      Result.B := (others => 0.0);
      Result.L := 0;
      Result.M := Segments;

      return Result;

   end New_Buffer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the index of a given node in the buffer
   --==========================================================================
   function Get_Index (N : Natural) return Positive is
   begin
      return Vertex_Data_Size * (N - 1) + 1;
   end Get_Index;
   pragma Inline (Get_Index);
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Builds a cap from the specified node N by adding 8 extra nodes
   -- A is the point where the cap is, B is the other side of the segment
   -- N is the first node of the cap.
   --===========================================================================
   procedure Build_Cap (This : in out Buffer_Type'Class; N : Natural; A : Vector_Record; B : Vector_Record) is

      BX : Vector_Record := A - B;
      N1 : Vector_Record;
      N2 : Vector_Record;

   begin

    --Utility.Log.Put_Message ("building cap at " & Image (A));

      Unit (BX);

      N1 := BX;
      Flip (N1);

      N2 := N1;
      Oppose (N2);

      declare
         V1 : Vertex_Record; for V1'Address use This.B (Get_Index (N + 0))'Address;
         V2 : Vertex_Record; for V2'Address use This.B (Get_Index (N + 1))'Address;
         V3 : Vertex_Record; for V3'Address use This.B (Get_Index (N + 2))'Address;
         V4 : Vertex_Record; for V4'Address use This.B (Get_Index (N + 3))'Address;
         V5 : Vertex_Record; for V5'Address use This.B (Get_Index (N + 4))'Address;
         V6 : Vertex_Record; for V6'Address use This.B (Get_Index (N + 5))'Address;
         V7 : Vertex_Record; for V7'Address use This.B (Get_Index (N + 6))'Address;
         V8 : Vertex_Record; for V8'Address use This.B (Get_Index (N + 7))'Address;
         V9 : Vertex_Record; for V9'Address use This.B (Get_Index (N + 8))'Address;
      begin

         V1.P := A;
         V1.N := N1;
         V1.A := 1.0;
         V2.P := A;
         V2.N := N1 + BX;
         Unit (V2.N);
         V2.A := 1.0;
         V3.P := A;
         V3.N := (0.0,0.0);
         V3.A := 0.0;

         V4.P := A;
         V4.N := V2.N;
         V4.A := 1.0;
         V5.P := A;
         V5.N := N2 + BX;
         Unit (V5.N);
         V5.A := 1.0;
         V6.P := A;
         V6.N := (0.0,0.0);
         V6.A := 0.0;

         V7.P := A;
         V7.N := V5.N;
         V7.A := 1.0;
         V8.P := A;
         V8.N := N2;
         V8.A := 1.0;
         V9.P := A;
         V9.N := (0.0,0.0);
         V9.A := 0.0;

      end;

      This.N := This.N + 8;

   end Build_Cap;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Move_To (This : in out Buffer_Type'Class; X, Y : Float) is

      N : Natural renames This.N;

   begin

      if This.N = 0 then
         This.Start := False;
         This.Close := False;
      end if;

      if This.L < This.M then

         -- Close last path with cap
         ---------------------------------------------

         if This.Close then

            This.Close := False;

            declare
               A : Vector_Record; for A'Address use This.B (Get_Index (N    ))'Address;
               B : Vector_Record; for B'Address use This.B (Get_Index (N - 2))'Address;
            begin

               N := N + 1;

               Build_Cap (This, N, A, B);

            end;

         end if;

         -- Move to new node
         ---------------------------------------------

         if not This.Start then
            N := N + 1;
         end if;

         This.Start := True;

         declare
            A : Vector_Record; for A'Address use This.B (Get_Index (N))'Address;
         begin
            A.X := GLfloat (X);
            A.Y := GLfloat (Y);
          --Utility.Log.Put_Message ("move to " & Image (A));
         end;

      end if;

   end Move_To;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Line_To (This : in out Buffer_Type'Class; X, Y : Float) is

      N : Natural renames This.N;

   begin

      if This.N = 0 then

         This.Start := False;
         This.Close := False;

         This.Move_To (X, Y);

      elsif This.L < This.M then

         if This.Start then

            -- Start path with cap
            -- NOTE: the first node was added on Move_To
            ---------------------------------------------

            This.Start := False;

            declare
               A : Vector_Record; for A'Address use This.B (Get_Index (N))'Address;
            begin

               Build_Cap (This, N, A, (GLfloat (X), GLfloat (Y)));

            end;

         else
            -- Build joint
            ---------------------------------------------

            declare

               A  : Vector_Record := (GLfloat (X), GLfloat (Y));
               B  : Vector_Record; for B'Address use This.B (Get_Index (N    ))'Address;
               C  : Vector_Record; for C'Address use This.B (Get_Index (N - 2))'Address;

               T1 : Vector_Record := A - B;
               T2 : Vector_Record := C - B;

               N1 : Vector_Record;
               N2 : Vector_Record;
               BX : Vector_Record; -- Bisectriz
               E  : GLfloat;

            begin

             --Utility.Log.Put_Message ("jont at " & Image (A));

               Unit (T1);
               Unit (T2);

               N1 := T1;
               Flip (N1);

               N2 := T2;
               Flip (N2);

               E := T1 * T2;

               if E = 1.0 then

                  goto Skip_Joint;

               elsif E = -1.0 then

                  goto Skip_Joint;

               elsif E > 0.0 then
                  BX := T1 + T2;
                  Unit   (BX);
                  Oppose (BX);
                  if T1 / T2 > 0.0 then
                     Oppose (N1);
                  else
                     Oppose (N2);
                  end if;
               else
                  Oppose (N1);
                  BX := N1 + N2;
                  Unit (BX);
                  if N1 / N2 > 0.0 then
                     Oppose (N1);
                     Oppose (N2);
                     Oppose (BX);
                  end if;
               end if;

               if E < -0.5 then

                  -- 1 triangle in the joint

                  declare
                     V1 : Vertex_Record; for V1'Address use This.B (Get_Index (N + 1))'Address;
                     V2 : Vertex_Record; for V2'Address use This.B (Get_Index (N + 2))'Address;
                     V3 : Vertex_Record; for V3'Address use This.B (Get_Index (N + 3))'Address;
                  begin
                     V1.P := B;
                     V1.N := N1;
                     V1.A := 1.0;
                     V2.P := B;
                     V2.N := N2;
                     V2.A := 1.0;
                     V3.P := B;
                     V3.N := (0.0,0.0);
                     V3.A := 0.0;
                  end;

                  This.N := This.N + 3;

               elsif E < 0.0 then

                  -- 2 triangles in the joint

                  declare
                     V1 : Vertex_Record; for V1'Address use This.B (Get_Index (N + 1))'Address;
                     V2 : Vertex_Record; for V2'Address use This.B (Get_Index (N + 2))'Address;
                     V3 : Vertex_Record; for V3'Address use This.B (Get_Index (N + 3))'Address;
                     V4 : Vertex_Record; for V4'Address use This.B (Get_Index (N + 4))'Address;
                     V5 : Vertex_Record; for V5'Address use This.B (Get_Index (N + 5))'Address;
                     V6 : Vertex_Record; for V6'Address use This.B (Get_Index (N + 6))'Address;
                  begin
                     V1.P := B;
                     V1.N := N1;
                     V1.A := 1.0;
                     V2.P := B;
                     V2.N := BX;
                     V2.A := 1.0;
                     V3.P := B;
                     V3.N := (0.0,0.0);
                     V3.A := 0.0;

                     V4.P := B;
                     V4.N := BX;
                     V4.A := 1.0;
                     V5.P := B;
                     V5.N := N2;
                     V5.A := 1.0;
                     V6.P := B;
                     V6.N := (0.0,0.0);
                     V6.A := 0.0;

                  end;

                  This.N := This.N + 6;

               else

                  -- 3 triangles in the joint

                  declare
                     V1 : Vertex_Record; for V1'Address use This.B (Get_Index (N + 1))'Address;
                     V2 : Vertex_Record; for V2'Address use This.B (Get_Index (N + 2))'Address;
                     V3 : Vertex_Record; for V3'Address use This.B (Get_Index (N + 3))'Address;
                     V4 : Vertex_Record; for V4'Address use This.B (Get_Index (N + 4))'Address;
                     V5 : Vertex_Record; for V5'Address use This.B (Get_Index (N + 5))'Address;
                     V6 : Vertex_Record; for V6'Address use This.B (Get_Index (N + 6))'Address;
                     V7 : Vertex_Record; for V7'Address use This.B (Get_Index (N + 7))'Address;
                     V8 : Vertex_Record; for V8'Address use This.B (Get_Index (N + 8))'Address;
                     V9 : Vertex_Record; for V9'Address use This.B (Get_Index (N + 9))'Address;
                  begin

                     V1.P := B;
                     V1.N := N1;
                     V1.A := 1.0;
                     V2.P := B;
                     V2.N := N1 + BX;
                     V2.A := 1.0;
                     Unit (V2.N);
                     V3.P := B;
                     V3.N := (0.0,0.0);
                     V3.A := 0.0;

                     V4.P := B;
                     V4.N := V2.N;
                     V4.A := 1.0;
                     V5.P := B;
                     V5.N := N2 + BX;
                     Unit (V5.N);
                     V5.A := 1.0;
                     V6.P := B;
                     V6.N := (0.0,0.0);
                     V6.A := 0.0;

                     V7.P := B;
                     V7.N := V5.N;
                     V7.A := 1.0;
                     V8.P := B;
                     V8.N := N2;
                     V8.A := 1.0;
                     V9.P := B;
                     V9.N := (0.0,0.0);
                     V9.A := 0.0;

                  end;

                  This.N := This.N + 9;

               end if;

               <<Skip_Joint>>

            end;

         end if;

         -- Construct segment
         ---------------------------------------------

         This.L := This.L + 1;

         This.Close := True; -- To be handled in next Move_To or Finish

         declare

            A  : Vector_Record := (GLfloat (X), GLfloat (Y));
            B  : Vector_Record; for B'Address use This.B (Get_Index (N))'Address;

            D1 : Vector_Record := A - B;
            D2 : Vector_Record;

         begin

          --Utility.Log.Put_Message ("segment from " & Image (B) & " to " & Image (A));

            Unit (D1);
            Flip (D1);

            D2 := D1;
            Oppose (D2);

          --Utility.Log.Put_Message ("D1=" & Image (D1));
          --Utility.Log.Put_Message ("D2=" & Image (D2));

            declare
               V1 : Vertex_Record; for V1'Address use This.B (Get_Index (N + 1))'Address;
               V2 : Vertex_Record; for V2'Address use This.B (Get_Index (N + 2))'Address;
               V3 : Vertex_Record; for V3'Address use This.B (Get_Index (N + 3))'Address;
               V4 : Vertex_Record; for V4'Address use This.B (Get_Index (N + 4))'Address;
               V5 : Vertex_Record; for V5'Address use This.B (Get_Index (N + 5))'Address;
               V6 : Vertex_Record; for V6'Address use This.B (Get_Index (N + 6))'Address;
            begin

               V1.P := B;
               V1.N := D1;
               V1.A := 1.0;

               V2.P := B;
               V2.N := D2;
               V2.A :=-1.0;

               V3.P := A;
               V3.N := D1;
               V3.A := 1.0;

               V4.P := B;
               V4.N := D2;
               V4.A :=-1.0;

               V5.P := A;
               V5.N := D1;
               V5.A := 1.0;

               V6.P := A;
               V6.N := D2;
               V6.A :=-1.0;

            end;

            N := N + 6;

         end;

         if This.L = This.M then

            -- Build end cap

            declare
               A : Vector_Record; for A'Address use This.B (Get_Index (N - 2))'Address;
            begin

               N := N + 1;

               Build_Cap (This, N, (GLfloat (X), GLfloat (Y)), A);

            end;

            This.Close := False;

         end if;

      end if;

   end Line_To;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Finish (This : in out Buffer_Type'Class) is
   begin

      if This.Close then

         declare
            A : Vector_Record; for A'Address use This.B (Get_Index (This.N    ))'Address;
            B : Vector_Record; for B'Address use This.B (Get_Index (This.N - 2))'Address;
         begin

            This.N := This.N + 1;

            Build_Cap (This, This.N, A, B);

         end;

         This.Close := False;

      end if;

   end Finish;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Close (This : in out Buffer_Type'Class) is

      P : Vector_Record; for P'Address use This.B (Get_Index (1))'Address;

   begin

      if This.N > 0 then

         This.Line_To (Float (P.X),
                       Float (P.Y));

      end if;

   end Close;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Restarts the buffer
   --===========================================================================
   procedure Reset (This : in out Buffer_Type'Class) is
   begin

      This.N := 0;
      This.L := 0;

      This.Start := False;
      This.Close := False;

      --B := (others => 0.0);

   end Reset;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Indicates how many segments have been loaded
   --===========================================================================
   function Line_Count (This : in out Buffer_Type'Class) return Natural is
   begin

      return This.L;

   end Line_Count;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Clear (This : in out Resource_Type'Class) is
   begin

      This.Nodes  := 0;

   end Clear;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Loaded (This : Resource_Type'Class) return Boolean is
   begin

      return This.Loaded;

   end Loaded;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load (This : in out Resource_Type'Class; Buffer : Buffer_Type) is

      Length : GLsizeiptr;

   begin

      if not This.Loaded then

         This.Buffer := GL.Create_Buffer;

         This.Loaded := True;

      end if;

      This.Nodes  := Buffer.N;

      Length      := GLsizeiptr (Buffer.N * Vertex_Data_Bytes);

      GL.Bind_Buffer (GL_ARRAY_BUFFER, This.Buffer);

      GL.Buffer_Data (GL_ARRAY_BUFFER, Length, Buffer.B (Buffer.B'First)'Address, GL_STATIC_DRAW);

   exception
      when others =>
         Utility.Log.Put_Message ("error while loading lines buffer");

   end Load;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This   : Resource_Type'Class;
                   Color  : Color_Record;
                   Width  : Float;
                   Aspect : Float   := 1.0;
                   Closed : Boolean := False) is

      Gl_Color : GLfloat_Vector_4 := (GLfloat (Color.R),
                                      GLfloat (Color.G),
                                      GLfloat (Color.B),
                                      GLfloat (Color.A));

   begin

      if This.Nodes > 0 then

         GL.Bind_Buffer (GL_ARRAY_BUFFER, This.Buffer);

         GL.Vertex_Attrib_Pointer (Web.GL.GLuint (Vertex_Location), 2, Web.GL.Rendering_Contexts.FLOAT, False, Vertex_Data_Bytes, 0);

         GL.Enable_Vertex_Attrib_Array (Web.GL.GLuint (Vertex_Location));

         GL.Vertex_Attrib_Pointer (Web.GL.GLuint (Expand_Location), 2, Web.GL.Rendering_Contexts.FLOAT, False, Vertex_Data_Bytes, GLintptr (2 * GLfloat_Size));

         GL.Enable_Vertex_Attrib_Array (Web.GL.GLuint (Expand_Location));

         GL.Use_Program (Shader_Program);

         GL.Uniform_Matrix_4fv (Matrix_Location, False, Transform.Matrix);

         GL.Uniform_4fv (Color_Location,  Gl_Color);

         GL.Uniform_1f  (Width_Location,  GLfloat (Width));

         GL.Uniform_1f  (Ratio_Location,  GLfloat (Aspect));

         GL.Draw_Arrays (GL_TRIANGLES, 0, GLsizei (This.Nodes));

      end if;

   exception
      when others =>
         Utility.Log.Put_Message ("error while drawing lines");

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This       : Resource_Type'Class;
                   Color      : Line_Color_Record;
                   Width_Glow : Float;
                   Width_Fore : Float;
                   Aspect     : Float   := 1.0;
                   Closed     : Boolean := False) is
   begin

      This.Draw (Color.Glow, Width_Glow, Aspect, Closed);
      This.Draw (Color.Fore, Width_Fore, Aspect, Closed);

   end Draw;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Remove (This : in out Resource_Type'Class) is
   begin

      null;

   end Remove;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Example : Resource_Type;

   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Example is

      Color : Glex.Colors.Color_Record;

   begin

      if not Example.Loaded then

         declare
            Line : Buffer_Type := New_Buffer (4);
         begin
            Utility.Log.Put_Message ("loading GLEX lines example");

            Line.Move_To (0.3, 0.20);
            Line.Line_To (0.4, 0.82);
            Line.Line_To (0.8, 0.82);
            Line.Line_To (0.8, 0.20);
            Line.Finish;

            Example.Load (Line);

         end;

      end if;

      Color.R :=  0.0;
      Color.G :=  1.0;
      Color.B :=  1.0;
      Color.A :=  1.0;

      Example.Draw (Color, 0.006);

      Color.R :=  0.0;
      Color.G :=  0.0;
      Color.B :=  1.0;
      Color.A :=  1.0;

      Example.Draw (Color, 0.002);

   end Draw_Example;
   -----------------------------------------------------------------------------

end Glex.Lines;
--------------------------------------------------------------------------------
