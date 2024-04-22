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
--
with Web.Strings;
use  Web.Strings;
-- Gnav
with Gnav_Info;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Timing.Events;
use  Timing.Events;
with Utility.Log;
with Utility.Calendar;
use  Utility.Calendar;
with Utility.Strings;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Metar is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Widgets
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Pnl_Metar : Panel_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.012,
                                             Height    => 0.040,
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Metar content
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Metar_String is String (1..90);

   No_Metar_Data : constant Metar_String := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Metar_Data : Metar_String := No_Metar_Data;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Request : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Update : Times := No_Time;

   --===========================================================================
   --
   --===========================================================================
   procedure Send_Metar_Request;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Request a new metar every minute
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Metar_Request : access Timer_Record := Register_Timer (60.0, Send_Metar_Request'Access);




   --===========================================================================
   --
   --===========================================================================
   procedure Process_Metar_Request (S : in out Stream_Reader_Type) is
   begin

      Utility.Log.Put_Message ("processing metar");
      Utility.Log.Put_Message ("size" & Natural'Image (S.Get_Size));

      if not S.Is_Empty then

         Utility.Strings.Override (Metar_Data, S.Read_String (Natural'Min (60, S.Get_Size)));

         Last_Update := Cached_Time;

      end if;

      Metar_Request.Resume;

   end Process_Metar_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Request a new metar to the server. Give up after 4 seconds.
   --===========================================================================
   procedure Send_Metar_Request is
   begin

      if not Gnav_Info.Request_Metar then
         return;
      end if;

      Last_Request := Cached_Time;

      Utility.Resources.Request_Binary_Resource (Name    => "metar.dat",
                                                 Handler => Process_Metar_Request'Access,
                                                 Timeout => 4000);

      Metar_Request.Pause;

   end Send_Metar_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize (X, Y, W, H : Float) is

      Allocation : Allocation_Record;

   begin

      Allocation.X := X;
      Allocation.Y := Y;
      Allocation.W := W;
      Allocation.H := H;

      Pnl_Metar.Set_Allocation (Allocation);

      Pnl_Metar.Set_Background_Color (Color_Black);

      Pnl_Metar.Set_Transparency (0.60);

      Pnl_Metar.Set_Show_Border (True);

      Pnl_Metar.Set_Label ("METAR", Label_Left);

      Pnl_Metar.Set_Font_Size (0.03, 0.25);

      Pnl_Metar.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      -- Start requesting metar updates
      ---------------------------------------

      Send_Metar_Request;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Glex.Colors;
      use Glex.Fonts;
      use Utility.Strings;

      X : Float   := Pnl_Metar.Get_Allocation.X + 0.01;
      Y : Float   := Pnl_Metar.Get_Allocation.Y + Pnl_Metar.Get_Allocation.H - 0.1;
      I,
      J : Natural := Metar_Data'First;

      Color : Line_Color_Record := Line_Cyan;

   begin

      Pnl_Metar.Draw;

      -- First line
      ----------------------------
      for C in Metar_Data'Range loop
         if Metar_Data (C) = ' ' then
            J := C;
         end if;
         exit when J > 30;
      end loop;

      Draw (Metar_Data (I..J),
            X, Y,
            Font_1,
            Line_Green,
            Alignment_LL);

      -- Second line
      ----------------------------
      I := J + 1;

      for C in I .. Metar_Data'Last loop
         if Metar_Data (C) = ' ' then
            J := C;
         end if;
         exit when J > 60;
      end loop;

      Y := Y - 2.0 * Font_1.Height;

      Draw (Metar_Data (I..J),
            X, Y,
            Font_1,
            Line_Green,
            Alignment_LL);

      -- Third line
      ----------------------------
      I := J + 1;

      for C in I-1 .. Metar_Data'Last loop
         J := C;
         exit when Metar_Data (C-1) = ' ' and Metar_Data (C) = ' ';
      end loop;

      Y := Y - 2.0 * Font_1.Height;

      Draw (Metar_Data (I..J),
            X, Y,
            Font_1,
            Line_Green,
            Alignment_LL);

      if
        Last_Update  /= No_Time and
        Last_Request /= No_Time
      then

         if Cached_Time - Last_Update > Lapse_Of (90.0) then

            Color := Line_Red;

         end if;

         Draw (Day_Lapse_Image (Last_Update),
               Pnl_Metar.Get_Allocation.X + Pnl_Metar.Get_Allocation.W - 0.01, Y,
               Font_1,
               Color,
               Alignment_LR);

      end if;

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



end Display.Panels.Metar;
--------------------------------------------------------------------------------
