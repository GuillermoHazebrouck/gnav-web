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
with Web.Window;
with Web.HTML.Scripts;
with Web.Strings;
--Gnav
with Flight;
with Flight.Plan;
with Flight.Aircraft;
with Flight.Traffic;
with Gnav_Info;
with Glex;
with Glex.Colors;
with Glex.Basic;
with Glex.Fonts;
with Glex.Lines;
with Glex.Colormap;
with Widgets.Widget;
with Widgets.Button;
with Widgets.Panel;
with Widgets.Keyboard;
with Widgets.Dialog;
with Timing.Events;
with Utility.Calendar;
use  Utility.Calendar;
with Utility.Strings;
with Utility.Streams;
with Maps.Airspaces;
with Maps.Reference;
with Maps.Terrain.Loader;
with Maps.Layers;
with Math;
with Display.Menu;
with Utility.Log;


--******************************************************************************
--
--******************************************************************************
package body Gnav is


   --===========================================================================
   --
   --===========================================================================
   procedure Load_Service_Data is

      use Utility.Strings;
      use Web.Strings;

      Reader_Data  : String_Buffer (500);
      Reader_Value : String_Buffer (100);

   begin
      Utility.Log.Put_Message ("loading service information");
      Reader_Data.Load (To_String (To_Wide_Wide_String (Web.Window.Document.Get_Element_By_Id (
                        To_Web_String ("service-info")).As_HTML_Script.Get_Text)));

      for I in 1..4 loop
         Reader_Value.Load (Trim (Reader_Data.Read_Next (';')));
         declare
            Key : String := Reader_Value.Read_Next ('=');
            Val : String := Reader_Value.Read_Next ('=');
         begin
            if Key = "LOCATION" then
               Utility.Log.Put_Message ("startup location set to " & Val);
               Maps.Set_Reference (Maps.Value (Val));
               Gnav_Info.Home_Position := Maps.Value (Val);
               Flight.Data.Position    := Gnav_Info.Home_Position;
               -- TODO: it is more logic to initialize all positions with Home_Location
            elsif Key = "HOME" then
               Override (Gnav_Info.Home_Name, Val);
            elsif Key = "NAME" then
               Override (Gnav_Info.Service_Name, Val);
            elsif Key = "VERSION" then
               Override (Gnav_Info.Service_Version, Val);
            elsif Key = "TRAFFIC" then
               if Val = "TRUE" then
                  Gnav_Info.Request_Traffic := True;
               else
                  Gnav_Info.Request_Traffic := False;
               end if;
            elsif Key = "METAR" then
               if Val = "TRUE" then
                  Gnav_Info.Request_Metar := True;
               else
                  Gnav_Info.Request_Metar := False;
               end if;
            end if;
         end;
      end loop;

   end Load_Service_Data;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Initialize GNAV
   --===========================================================================
   procedure Initialize_Gnav is
   begin

      -- GLEX
      Glex.Initialize;
      Glex.Basic.Initialize;
      Glex.Lines.Initialize;
      Glex.Fonts.Initialize;
      Glex.Colormap.Initialize;

      Display.Width  := 1400.0;
      Display.Height := 720.0;

      declare
         S : Glex.Fonts.Font_Style_Record;
      begin

         S.Height    := 0.160;
         S.Space     := 0.035;
         S.Width     := 0.080;
         S.Thickness := Glex.Fonts.Font_Bold;

         Glex.Fonts.Draw
           ("G-NAV",
            X         => 0.5,
            Y         => 0.5,
            Style     => S,
            Color     => Glex.Colors.Line_Cyan,
            Alignment => Glex.Fonts.Alignment_CC);

      end;

      Load_Service_Data;

      Flight.Aircraft.Initialize;
      Flight.Plan.Initialize;

      Maps.Terrain.Loader.Initialize;
      Maps.Reference.Initialize;
      Maps.Layers.Initialize;
      Maps.Airspaces.Initialize;

      Flight.Traffic.Initialize;

      Display.Menu.Initialize;
      Display.Refresh := True;

   end Initialize_Gnav;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Cache_Time (M : Interfaces.IEEE_Float_64) is
      use Utility.Calendar;
      E : constant Times := Time_Of (1970, 1, 1, 0.0);
      S : Long_Float     := Long_Float (M) / 1000.0;
      L : Lapses         := Long_Lapse_Of (S);
   begin

      Utility.Calendar.Cache_Time (E + L);

   end Gnav_Cache_Time;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Set_Gnss_Data (Lat : Interfaces.IEEE_Float_64;
                                 Lon : Interfaces.IEEE_Float_64;
                                 Alt : Interfaces.IEEE_Float_64;
                                 Spd : Interfaces.IEEE_Float_64;
                                 Crs : Interfaces.IEEE_Float_64) is
      use Interfaces;
      use Flight;

   begin

      if Lat in -90.0..90.0 and Lon in -180.0..180.0 then
         Flight.Data.Position.Lon := Long_Float (Lon);
         Flight.Data.Position.Lat := Long_Float (Lat);
         Flight.Data.Ages   (Field_Position) := Cached_Time;
         Flight.Data.Origin (Field_Position) := Origin_External;
      end if;

      if Alt in 0.0..12_000.0 then
         Flight.Data.Altitude := Float (Alt);
         Flight.Data.Ages   (Field_Altitude) := Cached_Time;
         Flight.Data.Origin (Field_Altitude) := Origin_External;
      end if;

      if Spd in 0.0..300.0 then
         Flight.Data.Speed := Float (Spd);
         Flight.Data.Ages   (Field_Speed) := Cached_Time;
         Flight.Data.Origin (Field_Speed) := Origin_External;
      end if;

      if Crs in 0.0..360.0 then
         Flight.Data.Course := Float (Crs);
         Flight.Data.Ages   (Field_Course) := Cached_Time;
         Flight.Data.Origin (Field_Course) := Origin_External;
      end if;

      Flight.Cache_Data;

   end Gnav_Set_Gnss_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Process_Timer is
      use Flight;
   begin

      Timing.Events.Tick;

      Display.Blink := not Display.Blink;

      --Flight.Data.Course := 30.0;
      --Flight.Data.Ages   (Field_Course) := Cached_Time;
      --Flight.Data.Origin (Field_Course) := Origin_External;

   end Gnav_Process_Timer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Refresh_Screen is
   begin

      if Display.Refresh then

         Glex.Clear_Screen;

         Display.Menu.Draw;

      end if;

   end Gnav_Refresh_Screen;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Touch_Screen (X : Interfaces.IEEE_Float_64;
                                Y : Interfaces.IEEE_Float_64) is

      Mouse_X : Float := 0.0;
      Mouse_Y : Float := 0.0;

   begin

      if Glex.Vertical then

         if Y in 0.0..1.0 then
            Mouse_X := Float (Y);
         end if;

         if X in 0.0..1.0 then
            Mouse_Y := Float (X);
         end if;

      else

         if Y in 0.0..1.0 then
            Mouse_X := Float (X);
         end if;

         if X in 0.0..1.0 then
            Mouse_Y := 1.0 - Float (Y);
         end if;

      end if;

      Display.Menu.Screen_Pressed (Mouse_X, Mouse_Y);

   end Gnav_Touch_Screen;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Update_Size (W : Interfaces.IEEE_Float_64;
                               H : Interfaces.IEEE_Float_64) is

      use Interfaces;

   begin

      if H > W then
         Glex.Vertical  := True;
         Display.Height := Float (W);
         Display.Width  := Float (H);
      else
         Glex.Vertical  := False;
         Display.Height := Float (H);
         Display.Width  := Float (W);
      end if;

      Display.Aspect := Display.Width  / Display.Height;
      Glex.Aspect    := Display.Aspect;

   end Gnav_Update_Size;
   -----------------------------------------------------------------------------

begin
   Initialize_Gnav;

end Gnav;
--------------------------------------------------------------------------------
