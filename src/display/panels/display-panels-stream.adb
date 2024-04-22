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

-- Gnav
with Display.Panels.Replay;
with Flight.Parsing;
with Flight.Signal;
use  Flight.Signal;
with Flight.Stream;
use  Flight.Stream;
with Gl.Colors;
use  Gl.Colors;
with Gl.Fonts;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Dialog;
use  Widgets.Dialog;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Stream is

   -- Widgets
   ---------------------------------

   Pnl_Stream    : Panel_Record;

   Btn_Sats      : Button_Record;

   Btn_Sat       : Button_Record;

   Btn_Stream    : Button_Record;

   Btn_Active    : Button_Record;

   Btn_Replay    : Button_Record;

   Btn_Channel   : array (Channel_Range) of Button_Record;

   Btn_Apply     : Button_Record;

   Btn_Cancel    : Button_Record;

   -- Fonts
   ---------------------------------

   Font_1        : Gl.Fonts.Font_Style_Record := (Width     => 0.010,
                                                  Height    => 0.030,
                                                  Space     => 0.004,
                                                  Rendering => Gl.Fonts.Font_Glow,
                                                  Thickness => Gl.Fonts.Font_Regular);
   -- Internal variables
   ---------------------------------

   type Panel_Pages is (Page_Main, Page_Sats, Page_Sream);

   Active_Page       : Panel_Pages := Page_Main;

   Selected_Sat      : Satellite_Kinds := Satellite_Kinds'Last;

   Selected_Stream   : Source_Kind     := Get_Source_Kind;

   Selected_Channel  : Channel_Range   := Get_Channel_Index;

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize (X, W : Float) is

      Allocation : Allocation_Record;

   begin

      Allocation.X := X;
      Allocation.Y := 0.010;
      Allocation.W := W;
      Allocation.H := 0.850;

      Pnl_Stream.Set_Allocation (Allocation);

      Pnl_Stream.Set_Background_Color (Color_Black);

      Pnl_Stream.Set_Transparency (0.60);

      Pnl_Stream.Set_Show_Border (True);

      Pnl_Stream.Set_Label ("STREAMING", Label_Left);

      Pnl_Stream.Set_Font_Size (0.03, 0.25);

      Pnl_Stream.Set_Label_Color ((0.8, 0.8, 0.8, 1.0), (0.1, 0.1, 0.1, 1.0));

      --

      Allocation.X := X + 0.010;
      Allocation.Y := 0.330;
      Allocation.W := W - 0.020;
      Allocation.H := 0.100;

      Btn_Sats.Set_Allocation (Allocation);

      Btn_Sats.Set_Label ("SATELLITES");

      Btn_Sats.Set_Style (Button_Action);

      Btn_Sats.Set_Font_Size (0.3, 0.3);

      --

      Allocation.X := X + 0.010;
      Allocation.Y := Pnl_Stream.Get_Allocation.Y + Pnl_Stream.Get_Allocation.H - 0.14;
      Allocation.W := W - 0.020;
      Allocation.H := 0.100;

      Btn_Sat.Set_Allocation (Allocation);

      Btn_Sat.Set_Label ("GPS");

      Btn_Sat.Set_Style (Button_Action);

      Btn_Sat.Set_Font_Size (0.4, 0.3);

      -- Streaming sources

      Allocation.X := X + 0.010;
      Allocation.Y := Pnl_Stream.Get_Allocation.Y + 0.04;
      Allocation.W := W - 0.020;
      Allocation.H := 0.100;

      Btn_Stream.Set_Allocation (Allocation);

      Btn_Stream.Set_Label ("STREAM SOURCE");

      Btn_Stream.Set_Style (Button_Action);

      Btn_Stream.Set_Font_Size (0.3, 0.3);

      --

      Allocation.X := X + 0.010;
      Allocation.Y := Pnl_Stream.Get_Allocation.Y + Pnl_Stream.Get_Allocation.H - 0.15;
      Allocation.W := (W - 0.020) / 2.0;

      Btn_Active.Set_Allocation (Allocation);

      Btn_Active.Set_Label ("CHANNELS");

      Btn_Active.Set_Style (Button_Disabled);

      Btn_Active.Set_Font_Size (0.3, 0.3);

      --

      Allocation.X := Allocation.X + Allocation.W;

      Btn_Replay.Set_Allocation (Allocation);

      Btn_Replay.Set_Label ("REPLAY");

      Btn_Replay.Set_Style (Button_Disabled);

      Btn_Replay.Set_Font_Size (0.3, 0.3);

      --

      Allocation.X := X + 0.020;
      Allocation.Y := Btn_Active.Get_Allocation.Y - Btn_Active.Get_Allocation.H - 0.02;
      Allocation.W := (W - 0.060) / 2.0;
      Allocation.H := 0.08;

      for C in Channel_Range loop

         Btn_Channel (C).Set_Allocation (Allocation);

         Btn_Channel (C).Set_Label (Get_Channel_Name (C));

         Btn_Channel (C).Set_Style (Button_Disabled);

         Btn_Channel (C).Set_Font_Size (0.3, 0.3);

         Allocation.Y := Allocation.Y - Allocation.H - 0.02;

      end loop;

      --

      Allocation.X := X + 0.020;
      Allocation.Y := Pnl_Stream.Get_Allocation.Y + 0.02;
      Allocation.W := (W - 0.060) / 2.0;
      Allocation.H := 0.08;

      Btn_Apply.Set_Allocation (Allocation);

      Btn_Apply.Set_Label ("OK");

      Btn_Apply.Set_Style (Button_Ok);

      Btn_Apply.Set_Font_Size (0.3, 0.3);

      --

      Allocation.X := Allocation.X + Allocation.W + 0.020;

      Btn_Cancel.Set_Allocation (Allocation);

      Btn_Cancel.Set_Label ("CANCEL");

      Btn_Cancel.Set_Style (Button_Cancel);

      Btn_Cancel.Set_Font_Size (0.3, 0.3);

      -- Replay panel
      ------------------------------------------------------

      Display.Panels.Replay.Initialize (Pnl_Stream.Get_Allocation.X + 0.01,
                                        Btn_Apply.Get_Allocation.Y  + 0.01,
                                        Pnl_Stream.Get_Allocation.W - 0.02,
                                        Btn_Replay.Get_Allocation.Y - Btn_Apply.Get_Allocation.Y - 0.02);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Gl.Fonts;

      X, Y  : Float;

   begin

      -- Acquisition panel
      --------------------------------------------

      Pnl_Stream.Draw;

      case Active_Page is

         when Page_Sats =>

            Btn_Sat.Draw;

            X := Btn_Sat.Get_Allocation.X + 0.01;

            Y := Btn_Sat.Get_Allocation.Y - 1.8 * Font_1.Height;

            if Number_Of_Satellites (Selected_Sat) > 0 then

               for Sat of Satellites (Selected_Sat) loop

                  if Sat.Valid then

                     Draw (Sat.Id & ">",
                           X, Y,
                           Font_1,
                           Line_Cyan,
                           Alignment_LL);

                     if Sat.Strength < 5 then

                        Draw (Natural'Image (Sat.Strength),
                              X + 0.12, Y,
                              Font_1,
                              Line_Gray,
                              Alignment_LR);

                     elsif Sat.Strength < 20 then

                        Draw (Natural'Image (Sat.Strength),
                              X + 0.12, Y,
                              Font_1,
                              Line_Yellow,
                              Alignment_LR);

                     else

                        Draw (Natural'Image (Sat.Strength),
                              X + 0.12, Y,
                              Font_1,
                              Line_Green,
                              Alignment_LR);

                     end if;

                     Draw (Natural'Image (Sat.Azimuth),
                           X + 0.20, Y,
                           Font_1,
                           Line_Gray,
                           Alignment_LR);

                     Draw (Natural'Image (Sat.Elevation),
                           X + 0.28, Y,
                           Font_1,
                           Line_Gray,
                           Alignment_LR);

                     Y := Y - 1.9 * Font_1.Height;

                  end if;

               end loop;

            else

               Draw ("NO SATELLITES",
                     X, Y,
                     Font_1,
                     Line_Red);

            end if;

         when Page_Main =>

            -- Source kind

            X := Pnl_Stream.Get_Allocation.X + 0.02;

            Y := Pnl_Stream.Get_Allocation.Y + Pnl_Stream.Get_Allocation.H - 0.1;

            Draw ("SOURCE:",
                  X, Y,
                  Font_1,
                  Line_Gray);

            X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

            case Flight.Stream.Get_Source_Kind is

            when Flight.Stream.Source_File =>

               Draw ("RECORDING",
                     X, Y,
                     Font_1,
                     Line_Gray,
                     Alignment_LR);

            when Flight.Stream.Source_Socket =>

               Draw ("UDP",
                     X, Y,
                     Font_1,
                     Line_Gray,
                     Alignment_LR);

            when Flight.Stream.Source_Serial =>

               Draw ("SERIAL",
                     X, Y,
                     Font_1,
                     Line_Gray,
                     Alignment_LR);

            when others =>

               Draw ("NOT SET",
                     X, Y,
                     Font_1,
                     Line_Gray,
                     Alignment_LR);

            end case;

            -- Data formating sytem

            X := Pnl_Stream.Get_Allocation.X + 0.02;

            Y := Y - 2.0 * Font_1.Height;

            Draw ("FORMAT:",
                  X, Y,
                  Font_1,
                  Line_Gray);

            X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

            Draw (Flight.Stream.Get_Protocol_Image (Flight.Stream.Get_Protocol_Kind),
                  X, Y,
                  Font_1,
                  Line_Gray,
                  Alignment_LR);

            --

            X := Pnl_Stream.Get_Allocation.X + 0.02;

            Y := Y - 2.0 * Font_1.Height;

            Draw ("STATUS:",
                  X, Y,
                  Font_1,
                  Line_Gray); -- OK / ERROR

            X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

            if Flight.Stream.Is_Active then

               Draw ("ACTIVE",
                     X, Y,
                     Font_1,
                     Line_Green,
                     Alignment_LR);

            elsif Blink then

               Draw ("ERROR",
                     X, Y,
                     Font_1,
                     Line_Red,
                     Alignment_LR);

            end if;

            -- Incoming data rate

            X := Pnl_Stream.Get_Allocation.X + 0.02;

            Y := Y - 2.0 * Font_1.Height;

            Draw ("RATE:",
                  X, Y,
                  Font_1,
                  Line_Gray); -- 4P/S

            X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

            if Flight.Stream.Get_Rate > 0 then

               Draw (Natural'Image (Flight.Stream.Get_Rate),
                     X, Y,
                     Font_1,
                     Line_Green,
                     Alignment_LR);

            else

               Draw ("NO INPUT",
                     X, Y,
                     Font_1,
                     Line_Red,
                     Alignment_LR);

            end if;

            -- GPS fix status

            X := Pnl_Stream.Get_Allocation.X + 0.02;

            Y := Y - 2.0 * Font_1.Height;

            Draw ("GPS:",
                  X, Y,
                  Font_1,
                  Line_Gray);

            X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

            if Flight.Data.Age (Flight.Field_Position)  < 3.0 then

               Draw ("OK",
                     X, Y,
                     Font_1,
                     Line_Green,
                     Alignment_LR);

            else

               Draw ("NO FIX",
                     X, Y,
                     Font_1,
                     Line_Red,
                     Alignment_LR);

            end if;

            -- Total number of satellites in view

            declare
               Total : Natural := 0;
            begin

               X := Pnl_Stream.Get_Allocation.X + 0.02;

               Y := Y - 2.0 * Font_1.Height;

               for K in Satellite_Kinds loop

                  Total := Total + Number_Of_Satellites (K);

               end loop;

               if Total > 0 then

                  Draw ("SATELLITES:",
                        X, Y,
                        Font_1,
                        Line_Gray);

                  X := Pnl_Stream.Get_Allocation.X + Pnl_Stream.Get_Allocation.W - 0.02;

                  Draw (Natural'Image (Total),
                        X, Y,
                        Font_1,
                        Line_Green,
                        Alignment_LR);

               else

                  Draw ("NO SATELLITES",
                        X, Y,
                        Font_1,
                        Line_Red);

               end if;

            end;

            Btn_Sats.Draw;

            X := Pnl_Stream.Get_Allocation.X + 0.02;

            Y := Btn_Sats.Get_Allocation.Y - 2.0 * Font_1.Height;

            if Flight.Parsing.Error_Detected then

               declare
                  Color : Line_Color_Record;
               begin

                  if Flight.Parsing.Recent_Error then
                     Color := Line_Red;
                  else
                     Color := Line_Gray;
                  end if;

                  Draw ("PARSING ERROR",
                        X, Y,
                        Font_1,
                        Color,
                        Alignment_LL);

               end;

            else

               Draw ("DATA OK",
                     X, Y,
                     Font_1,
                     Line_Gray,
                     Alignment_LL);

            end if;

            Y := Y - 2.0 * Font_1.Height;

            if Flight.Stream.Recording then

               Draw ("RECORDING",
                     X, Y,
                     Font_1,
                     Line_Green,
                     Alignment_LL);

            else

               Draw ("NOT RECORDING",
                     X, Y,
                     Font_1,
                     Line_Gray,
                     Alignment_LL);

            end if;

            -- Stream source

            Btn_Stream.Draw;

         when Page_Sream =>

            declare
               use Flight.Stream;
            begin

               Btn_Active.Set_Style (Button_Disabled);
               Btn_Replay.Set_Style (Button_Disabled);

               if Selected_Stream = Source_File then

                  Btn_Replay.Set_Style (Button_Enabled);

                  Btn_Active.Draw;
                  Btn_Replay.Draw;

                  Display.Panels.Replay.Draw;

               else
                  Btn_Active.Set_Style (Button_Enabled);

                  Btn_Active.Draw;
                  Btn_Replay.Draw;

                  for C in Channel_Range loop

                     if C = Selected_Channel then

                        Btn_Channel (C).Set_Style (Button_Enabled);

                     else
                        Btn_Channel (C).Set_Style (Button_Disabled);

                     end if;

                     if Get_Channel_Name (C) /= "" then

                        Btn_Channel (C).Draw;

                     end if;

                  end loop;

               end if;

            end;

            Btn_Apply.Draw;
            Btn_Cancel.Draw;

      end case;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Update_Source_Button is
   begin

      case Selected_Sat is

         when Sat_Gps =>

            Btn_Sat.Set_Label ("GPS");

         when Sat_Galileo =>

            Btn_Sat.Set_Label ("GALILEO");

         when Sat_Glonass =>

            Btn_Sat.Set_Label ("GLONASS");

         when Sat_Beidou =>

            Btn_Sat.Set_Label ("BEIDOU");

      end case;

   end Update_Source_Button;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Change_Stream (Result : Dialog_Result_Kind) is
   begin

      if Result = Dialog_Ok then

         if Selected_Stream = Source_File then

            Configure_Replay  (Display.Panels.Replay.Get_Selected_Id);

         else
            Configure_Channel (Selected_Channel);

         end if;

      end if;

      Active_Page := Page_Main;

      Refresh := True;

   end Change_Stream;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is

      procedure Cycle_Satellites is
      begin

         if Selected_Sat = Satellite_Kinds'Last then
            Selected_Sat := Satellite_Kinds'First;
         else
            Selected_Sat := Satellite_Kinds'Succ (Selected_Sat);
         end if;

         for S in Selected_Sat..Satellite_Kinds'Last loop

            Selected_Sat := S;

            if Number_Of_Satellites (S) > 0 then

               Active_Page := Page_Sats;

               Refresh     := True;

               Update_Source_Button;

               return;

            elsif S = Satellite_Kinds'Last then

               Active_Page := Page_Main;

               Refresh     := True;

            end if;

         end loop;

      end Cycle_Satellites;

   begin

      case Active_Page is

         when Page_Sats =>

            if Btn_Sat.Contains (X, Y) then

               Cycle_Satellites;

            end if;

         when Page_Main =>

            if Btn_Sats.Contains (X, Y) then

               Cycle_Satellites;

            elsif Btn_Stream.Contains (X, Y) then

               Selected_Stream  := Get_Source_Kind;

               Selected_Channel := Get_Channel_Index;

               Active_Page      := Page_Sream;

               Refresh          := True;

            end if;

         when Page_Sream =>

            if Selected_Stream /= Source_File then

               for C in Channel_Range loop

                  if Btn_Channel (C).Contains (X, Y) then

                     Selected_Stream   := Source_None;

                     Selected_Channel  := C;

                     Refresh           := True;

                     exit;

                  end if;

               end loop;

            end if;

            if Btn_Replay.Contains (X, Y) then

               Selected_Stream  := Source_File;

               Refresh          := True;

            elsif Btn_Active.Contains (X, Y) then

               Selected_Stream  := Source_None;

               Selected_Channel := Get_Channel_Index;

               Refresh          := True;

            elsif Btn_Cancel.Contains (X, Y) then

               Selected_Stream   := Get_Source_Kind;

               Active_Page       := Page_Main;

               Refresh           := True;

            elsif Btn_Apply.Contains (X, Y) then

               Widgets.Dialog.Confirm ("CONFIRM STREAM CHANGE", Change_Stream'Access);

               Refresh := True;

            else

               case Selected_Stream is

                  when Source_File =>

                     Display.Panels.Replay.Screen_Pressed (X, Y);

                  when others =>
                     null;

               end case;

            end if;

      end case;

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



end Display.Panels.Stream;
--------------------------------------------------------------------------------
