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
with Glex;
with Glex.Basic;
use  Glex.Basic;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Glex.Symbols is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A list of all possible font data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Character_Resource_Array is array (Symbol_Kinds) of Resource_Type;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The buffer used to create the font lines
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buffer : Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (8);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The list of all font data items
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Resources : Character_Resource_Array;

   --===========================================================================
   -- Returns the lines buffer for an specific character
   --===========================================================================
   procedure Load_Symbol (S : Symbol_Kinds) is
   begin

      Buffer.Reset;

      case S is

         when Triangle_Up =>

            Buffer.Load_Node (0.00, 0.00);
            Buffer.Load_Node (1.00, 0.00);
            Buffer.Load_Node (0.50, 1.00);

         when Triangle_Down =>

            Buffer.Load_Node (0.00, 1.00);
            Buffer.Load_Node (1.00, 1.00);
            Buffer.Load_Node (0.50, 0.00);

         when Triangle_Right =>

            Buffer.Load_Node (0.00, 0.00);
            Buffer.Load_Node (0.00, 1.00);
            Buffer.Load_Node (1.00, 0.50);

         when Triangle_Left =>

            Buffer.Load_Node (1.00, 0.00);
            Buffer.Load_Node (1.00, 1.00);
            Buffer.Load_Node (0.00, 0.50);

         when Square =>

            Buffer.Load_Node (0.00, 0.00);
            Buffer.Load_Node (0.00, 1.00);
            Buffer.Load_Node (1.00, 1.00);

            Buffer.Load_Node (0.00, 0.00);
            Buffer.Load_Node (1.00, 0.00);
            Buffer.Load_Node (1.00, 1.00);

         when Diamond =>

            Buffer.Load_Node (0.50, 0.00);
            Buffer.Load_Node (1.00, 0.50);
            Buffer.Load_Node (0.00, 0.50);

            Buffer.Load_Node (0.00, 0.50);
            Buffer.Load_Node (1.00, 0.50);
            Buffer.Load_Node (0.50, 1.00);

      end case;

      Resources (S).Load (Buffer);

   end Load_Symbol;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      -- Upper case
      ------------------------------
      for S in Symbol_Kinds loop

         Load_Symbol (S);

      end loop;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Draws the given symbol
   -- X, Y: the position using the current matrix transformation
   --===========================================================================
   procedure Draw (Symbol    : Symbol_Kinds;
                   X, Y      : Float;
                   Size      : Float;
                   Color     : Color_Record;
                   Alignment : Font_Alignment_Types := Alignment_CC) is

      use Glex;

      T      : Transform_Record := Glex.Transform;
      H      : Float := X;
      V      : Float := Y;
      Width  : Float := Size;
      Height : Float := Size * Aspect;

   begin

      case Alignment is

         when Alignment_LC =>

            H := H - 0.5 * Width;

         when Alignment_LR =>

            H := H - Width;

         when Alignment_TL =>

            V := V - Height;

         when Alignment_TC =>

            H := H - 0.5 * Width;

            V := V - Height;

         when Alignment_TR =>

            H := H - Width;

            V := V - Height;

         when Alignment_CC =>

            H := H - 0.5 * Width;

            V := V - 0.5 * Height;

         when Alignment_CR =>

            H := H - Width;

            V := V - 0.5 * Height;

         when Alignment_CL =>

            V := V - 0.5 * Height;

         when others =>

            null;

      end case;

      Glex.Transform.Translate (H, V);

      Glex.Transform.Scale (Width, Height);

      Resources (Symbol).Draw (Color, Glex.Basic.Triangles);

      Glex.Transform.Copy (T);

   end Draw;
   -----------------------------------------------------------------------------

end Glex.Symbols;
--------------------------------------------------------------------------------
