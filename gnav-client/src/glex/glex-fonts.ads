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

--AdaWebPack
with Web.Gl.Rendering_Contexts;
use  Web.Gl.Rendering_Contexts;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides a simple monospaced vector font rendering in OpenGL
-- independent from any fonts library.
-- The packages supports both, Core and Legacy modes.
--//////////////////////////////////////////////////////////////////////////////
package Glex.Fonts is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Font_Float is Float range 0.0..Float'Last;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Font_Rendering_Types is (Font_Simple, Font_Glow, Font_Extra_Glow);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Font_Thickness_Types is (Font_Thin,
                                 Font_Regular,
                                 Font_Bold);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Font_Alignment_Types is (Alignment_LL,
                                 Alignment_LC,
                                 Alignment_LR,
                                 Alignment_TL,
                                 Alignment_TC,
                                 Alignment_TR,
                                 Alignment_CC,
                                 Alignment_CR,
                                 Alignment_CL);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Font_Style_Record is record

      Rendering : Font_Rendering_Types := Font_Glow;

      Width     : Font_Float := 0.1;

      Height    : Font_Float := 0.1;

      Space     : Font_Float := 0.1;

      Thickness : Font_Thickness_Types := Font_Regular;

   end record;
   -----------------------------------------------------------------------------

   --===========================================================================
   -- Initializes the font buffers
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Draws the given text
   -- X, Y: the position using the current matrix transformation
   --===========================================================================
   procedure Draw (Text      : String;
                   X, Y      : Float;
                   Style     : Font_Style_Record;
                   Color     : Line_Color_Record;
                   Alignment : Font_Alignment_Types := Alignment_LL);

end Glex.Fonts;
--------------------------------------------------------------------------------
