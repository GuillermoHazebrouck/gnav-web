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
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Log;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Radio is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Widgets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Pnl_Radio : Panel_Record;

   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.012,
                                             Height    => 0.040,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Station names
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Station_String   is String (1..20);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Frequencies
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Frequency_String is String (1..7);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Frequencies
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Location_String is String (1..4);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Radio station
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Radio_Record is record

      Station   : Station_String   := (others => ' ');

      Frequency : Frequency_String := (others => ' ');

      Location  : Location_String  := (others => ' ');

      Valid     : Boolean          := False;

   end record;

   Radios  : array (1..20) of Radio_Record;

   Space_1 : Float := (1.0 + Float (Station_String'Length))   * (Font_1.Width + Font_1.Space);

   Space_2 : Float := (1.0 + Float (Frequency_String'Length)) * (Font_1.Width + Font_1.Space) + Space_1;

   --===========================================================================
   --
   --===========================================================================
   procedure Load_Radios is
   begin

      -- TODO: load radio frequencies (see native G-NAV)

      null;

   end Load_Radios;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize (X, Y, W, H : Float) is

      Allocation : Allocation_Record;

   begin

      Load_Radios;

      Allocation.X := X;
      Allocation.Y := Y;
      Allocation.W := W;
      Allocation.H := H;

      Pnl_Radio.Set_Allocation (Allocation);

      Pnl_Radio.Set_Background_Color (Color_Black);

      Pnl_Radio.Set_Transparency (0.60);

      Pnl_Radio.Set_Show_Border (True);

      Pnl_Radio.Set_Label ("FREQUENCY", Label_Left);

      Pnl_Radio.Set_Font_Size (0.03, 0.25);

      Pnl_Radio.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      --

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      X : Float := Pnl_Radio.Get_Allocation.X + 0.01;
      Y : Float := Pnl_Radio.Get_Allocation.Y + Pnl_Radio.Get_Allocation.H - 0.1;

   begin

      Pnl_Radio.Draw;

      for Radio of Radios loop

         exit when not Radio.Valid;

         Glex.Fonts.Draw (Radio.Station,
                          X, Y,
                          Font_1,
                          Line_Green);

         Glex.Fonts.Draw (Radio.Frequency,
                          X + Space_1, Y,
                          Font_1,
                          Line_Green);

         Glex.Fonts.Draw (Radio.Location,
                          X + Space_2, Y,
                          Font_1,
                          Line_Cyan);

         Y := Y - 2.0 * Font_1.Height;

      end loop;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin

      null;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin

      null;

   end Key_Changed;
   -----------------------------------------------------------------------------



end Display.Panels.Radio;
--------------------------------------------------------------------------------
