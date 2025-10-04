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
use  Glex.Fonts;

--AdaWebPack
with Web.Gl.Rendering_Contexts;
use  Web.Gl.Rendering_Contexts;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides a simple monospaced vector font rendering in OpenGL
-- independent from any fonts library.
-- The packages supports both, Core and Legacy modes.
--//////////////////////////////////////////////////////////////////////////////
package Glex.Symbols is

   --===========================================================================
   -- Initializes the font buffers
   --===========================================================================
   procedure Initialize;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Different kind of symbols
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Symbol_Kinds is (Triangle_Up,
                         Triangle_Down,
                         Triangle_Right,
                         Triangle_Left,
                         Square,
                         Diamond,
                         None);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Sizing_Kinds is (Size_Width,
                         Size_Height);

   --===========================================================================
   -- Draws the given symbol
   -- X, Y: the position using the current matrix transformation
   --===========================================================================
   procedure Draw (Symbol    : Symbol_Kinds;
                   X, Y      : Float;
                   Size      : Float;
                   Color     : Color_Record;
                   Alignment : Font_Alignment_Types := Alignment_CC;
                   Sizing    : Sizing_Kinds := Size_Width);

end Glex.Symbols;
--------------------------------------------------------------------------------
