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
with Ada.Command_Line;
-- Gnav
with Flight.Aircraft;
with Flight.Stream;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Atmosphere;
use  Utility.Atmosphere;
with Utility.Strings;
use  Utility.Strings;
with Utility.Units;
use  Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Replay is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The selected recording item
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Recording_Id : Flight.Stream.Replay_Range := Flight.Stream.Get_Replay_Index;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Widgets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   Btn_Up   : Button_Record;

   Btn_Down : Button_Record;

   Region   : Allocation_Record;

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize (X, Y, W, H : Float) is

      Allocation : Allocation_Record;

   begin

      Region.X := X;
      Region.Y := Y;
      Region.W := W;
      Region.H := H;

      -- Move file list up

      Btn_Down.Set_Label ("{");

      Btn_Down.Set_Style (Button_Action);

      Btn_Down.Set_Font_Size (0.7, 0.5);

      Allocation.X := X + W - 0.07;
      Allocation.Y := Y + 0.18;
      Allocation.W := 0.07;
      Allocation.H := 0.10;

      Btn_Down.Set_Allocation (Allocation);

      -- Move file list down

      Btn_Up.Set_Label ("}");

      Btn_Up.Set_Style (Button_Action);

      Btn_Up.Set_Font_Size (0.7, 0.5);

      Allocation.Y := Y + H - 0.12;

      Btn_Up.Set_Allocation (Allocation);

      Recording_Id := Flight.Stream.Get_Replay_Index;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Gl.Fonts;

      X, Y   : Float;

      Font   : Font_Style_Record := (Width     => 0.010,
                                     Height    => 0.035,
                                     Space     => 0.008,
                                     Rendering => Gl.Fonts.Font_Glow,
                                     Thickness => Gl.Fonts.Font_Regular);

      Info   : Flight.Stream.Replay_Record;

   begin

      X := Region.X;
      Y := Region.Y + Region.H - 2.0 * Font.Height;

      Info := Flight.Stream.Get_Replay_Info (Recording_Id);

      Gl.Fonts.Draw ("RECORDING " & Integer_Image (Integer (Recording_Id)),
                     X         => X,
                     Y         => Y,
                     Style     => Font,
                     Color     => Line_Yellow,
                     Alignment => Alignment_LL);

      Gl.Fonts.Draw (Date_Image (Info.Start),
                     X         => X,
                     Y         => Y - 0.07,
                     Style     => Font,
                     Color     => Line_Gray,
                     Alignment => Alignment_LL);

      Gl.Fonts.Draw ("TOT " & Time_Image (Info.Start),
                     X         => X,
                     Y         => Y - 0.14,
                     Style     => Font,
                     Color     => Line_Gray,
                     Alignment => Alignment_LL);

      Gl.Fonts.Draw ("TFD " & Span_Image (Info.Span),
                     X         => X,
                     Y         => Y - 0.21,
                     Style     => Font,
                     Color     => Line_Gray,
                     Alignment => Alignment_LL);

      Gl.Fonts.Draw ("-D> " & Integer_Image (Info.Distance) & "KM",
                     X         => X,
                     Y         => Y - 0.28,
                     Style     => Font,
                     Color     => Line_Gray,
                     Alignment => Alignment_LL);

      Gl.Fonts.Draw ("ALT " & Integer_Image (Info.Altitude) & "M",
                     X         => X,
                     Y         => Y - 0.35,
                     Style     => Font,
                     Color     => Line_Gray,
                     Alignment => Alignment_LL);

      -- Up and down bottons

      Btn_Up.Draw;

      Btn_Down.Draw;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      use Flight.Stream;

   begin

      if Btn_Up.Contains (X, Y) then

         if
           Recording_Id < Replay_Range'Last and then
           Flight.Stream.Is_Valid (Recording_Id + 1)
         then

            Recording_Id := Recording_Id + 1;

            Refresh := True;

         end if;

      elsif Btn_Down.Contains (X, Y) then

         if
           Recording_Id > Replay_Range'First and then
           Flight.Stream.Is_Valid (Recording_Id - 1)
         then

            Recording_Id := Recording_Id - 1;

            Refresh := True;

         end if;

      end if;

   end Screen_Pressed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin

      -- Use knob to adapt QNH

      case Key is

         when Panel_Wheel_Left =>

            -- Decrease QNH

            if Get_Qnh_Valid then

               Utility.Atmosphere.Set_Qnh (Get_Qnh - 1.0, True);

               Refresh := True;

            end if;

         when Panel_Wheel_Right =>

            -- Increase QNH

            if Get_Qnh_Valid then

               Utility.Atmosphere.Set_Qnh (Get_Qnh + 1.0, True);

               Refresh := True;

            end if;

         when Panel_Wheel_Button =>

            Utility.Atmosphere.Set_Qnh (Get_Qnh, not Get_Qnh_Valid);

            Refresh := True;

         when others =>
            null;

      end case;

   end Key_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Selected_Id return Flight.Stream.Replay_Range is
   begin

      return Recording_Id;

   end Get_Selected_Id;
   -----------------------------------------------------------------------------


end Display.Panels.Replay;
--------------------------------------------------------------------------------
