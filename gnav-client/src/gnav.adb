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
with Display.Main;
with Flight;
with Flight.Plan;
with Flight.Aircraft;
with Flight.Meteo;
with Flight.Register;
with Flight.Simulation;
with Flight.Traffic;
with Flight.Wind;
with Gnav_Info;
with Glex;
with Glex.Colors;
with Glex.Basic;
with Glex.Fonts;
with Glex.Lines;
with Glex.Symbols;
with Glex.Colormap;
with Widgets.Widget;
with Widgets.Button;
with Widgets.Panel;
with Widgets.Keyboard;
with Widgets.Dialog;
with Maps.Airspaces;
with Maps.Airspaces.Monitor;
with Maps.Reference;
with Maps.Terrain.Loader;
with Maps.Layers;
with Timing.Events;
with Utility.Calendar;
use  Utility.Calendar;
with Utility.Notifications;
with Utility.Storage;
with Utility.Strings;
with Utility.Log;
--Test only
with Math;
with Math.Vector2;


--******************************************************************************
--
--******************************************************************************
package body Gnav is

   --===========================================================================
   -- Reads the service-info section of the document to extract the
   -- configuration parameters.
   --===========================================================================
   procedure Load_Service_Data is

      use Utility.Strings;
      use Web.Strings;

      Reader_Data  : String_Buffer (500);
      Reader_Value : String_Buffer (100);

   begin

      --------------------------------------------------------------------------
      -- Load user Id
      --------------------------------------------------------------------------

      Utility.Strings.Override (Gnav_Info.User_Id, Utility.Storage.Get_Item ("ID"));
      Utility.Log.Put_Message ("Id loaded: " & Gnav_Info.User_Id);

      --------------------------------------------------------------------------
      -- Load service information
      --------------------------------------------------------------------------

      Utility.Log.Put_Message ("loading service information");

      Reader_Data.Load (To_String (To_Wide_Wide_String (Web.Window.Document.Get_Element_By_Id (
                        To_Web_String ("service-info")).As_HTML_Script.Get_Text)));

      --------------------------------------------------------------------------
      -- IMPORTANT: do not forget to increase the counter when adding new fields
      --------------------------------------------------------------------------

      for I in 1..10 loop

         Reader_Value.Load (Trim (Reader_Data.Read_Next (';')));

         declare
            Key : String := Reader_Value.Read_Next ('=');
            Val : String := Reader_Value.Read_Next ('=');
         begin

            --------------------------------------------------------------------
            if Key = "LOCATION" then

               Utility.Log.Put_Message ("startup location set to " & Val);

               Maps.Set_Reference (Maps.Value (Val));

               Gnav_Info.Home_Position := Maps.Value (Val);

               Flight.Data.Position    := Gnav_Info.Home_Position;

               -- TODO: it is more logic to initialize all positions with Home_Location

            --------------------------------------------------------------------
            elsif Key = "HOME" then

               Override (Gnav_Info.Home_Name, Val);

            --------------------------------------------------------------------
            elsif Key = "NAME" then

               Override (Gnav_Info.Service_Name, Val);

            --------------------------------------------------------------------
            elsif Key = "VERSION" then

               Override (Gnav_Info.Html_Version, Val);

            --------------------------------------------------------------------
            elsif Key = "TRAFFIC" then

               if Val = "TRUE" then
                  Gnav_Info.Request_Traffic := True;
               else
                  Gnav_Info.Request_Traffic := False;
               end if;

            --------------------------------------------------------------------
            elsif Key = "METAR" then

               if Val = "TRUE" then
                  Gnav_Info.Request_Metar := True;
               else
                  Gnav_Info.Request_Metar := False;
               end if;

            elsif Key = "APRS_AIRBORNE_ONLY" then

               if Val = "TRUE" then
                  Gnav_Info.Aprs_Airborne_Only := True;
               else
                  Gnav_Info.Aprs_Airborne_Only := False;
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
      Glex.Symbols.Initialize;
      Glex.Colormap.Initialize;

      Display.Width  := 1400.0; -- (default)
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

         S.Height    := 0.050;
         S.Space     := 0.038;
         S.Width     := 0.018;
         Glex.Fonts.Draw
           ("VERSION " & Gnav_Info.Core_Version,
            X         => 0.5,
            Y         => 0.1,
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
      Flight.Meteo.Initialize;
      Flight.Wind.Read_Wind;

      Display.Main.Initialize;
      Display.Refresh := True;

   end Initialize_Gnav;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Epoch : constant Times := Time_Of (1970, 1, 1, 0.0);




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Cache_Time (Millis : Interfaces.IEEE_Float_64) is
      use Utility.Calendar;

      Seconds : Long_Float := Long_Float (Millis) / 1000.0;
      Lapse   : Lapses     := Long_Lapse_Of (Seconds);

   begin

      Utility.Calendar.Cache_Time (Epoch + Lapse);

      if Gnav_Info.Simulation_Mode then

         Flight.Simulation.Next_Simulation_Step (Millis);

      end if;

   end Gnav_Cache_Time;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Set_Gnss_Data (Tmr : Interfaces.IEEE_Float_64;
                                 Lat : Interfaces.IEEE_Float_64;
                                 Lon : Interfaces.IEEE_Float_64;
                                 Alt : Interfaces.IEEE_Float_64;
                                 Spd : Interfaces.IEEE_Float_64;
                                 Crs : Interfaces.IEEE_Float_64) is
      use Interfaces;
      use Flight;
      use Utility.Calendar;

      Seconds : Long_Float := Long_Float (Tmr) / 1000.0;
      Lapse   : Lapses     := Long_Lapse_Of (Seconds);

   begin

      if Gnav_Info.Simulation_Mode then
         return;
      end if;

      Flight.Data.Timestamp := Epoch + Lapse;

      Flight.Data.Origin := (others => Update_None);

      if Lat in -90.0..90.0 and Lon in -180.0..180.0 then
         Flight.Data.Position.Lon := Long_Float (Lon);
         Flight.Data.Position.Lat := Long_Float (Lat);
         Flight.Data.Ages   (Field_Position) := Cached_Time;
         Flight.Data.Origin (Field_Position) := Update_External;
      end if;

      if Alt in 0.0..12_000.0 then
         Flight.Data.Altitude := Float (Alt);
         Flight.Data.Ages   (Field_Altitude) := Cached_Time;
         Flight.Data.Origin (Field_Altitude) := Update_External;
      end if;

      if Spd in 0.0..300.0 then
         Flight.Data.Speed := Float (Spd);
         Flight.Data.Ages   (Field_Speed) := Cached_Time;
         Flight.Data.Origin (Field_Speed) := Update_External;
      end if;

      if Crs in 0.0..360.0 then
         Flight.Data.Course := Float (Crs);
         Flight.Data.Ages   (Field_Course) := Cached_Time;
         Flight.Data.Origin (Field_Course) := Update_External;
      end if;

      Flight.Complete_Data;

      if Flight.Data.Is_Valid (Field_Position) then
         if Flight.Data.Is_Valid (Field_Altitude) then
            Maps.Airspaces.Monitor.Process_Location (Flight.Data.Position, Data.Altitude);
         end if;
         Maps.Reference.Update_Distance_To_Airfields (Flight.Data.Position);
      end if;

      Flight.Cache_Data;

      Flight.Register.Update;

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

   end Gnav_Process_Timer;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Refresh_Screen is
   begin

      if Display.Refresh then

         Glex.Clear_Screen;

         Display.Main.Draw;

      end if;

   end Gnav_Refresh_Screen;
   -----------------------------------------------------------------------------



   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   First_Down_X : Float := 0.0;
   First_Down_Y : Float := 0.0;
   Pressing     : Boolean := False;
   Moving       : Boolean := False;
   Down_Time    : Interfaces.IEEE_Float_64;
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Touch (X : Interfaces.IEEE_Float_64;
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

      Display.Main.Screen_Pressed (Mouse_X, Mouse_Y);

   end Gnav_Touch;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Touch_Start (X : Interfaces.IEEE_Float_64;
                               Y : Interfaces.IEEE_Float_64;
                               T : Interfaces.IEEE_Float_64) is

      Mouse_X : Float := 0.0;
      Mouse_Y : Float := 0.0;

   begin

      Down_Time := T;

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

      First_Down_X := Mouse_X;
      First_Down_Y := Mouse_Y;

      Pressing := True;
      Moving   := False;

   end Gnav_Touch_Start;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Touch_Move (X : Interfaces.IEEE_Float_64;
                              Y : Interfaces.IEEE_Float_64;
                              T : Interfaces.IEEE_Float_64) is

      use Interfaces;

      Mouse_X : Float := 0.0;
      Mouse_Y : Float := 0.0;
      Delta_X,
      Delta_Y,
      Delta_W : Float;

   begin

      if Pressing and then T - Down_Time > 100.0 then

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

         Display.Main.Screen_Move (First_Down_X,
                                   First_Down_Y,
                                   Mouse_X - First_Down_X,
                                   Mouse_Y - First_Down_Y,
                                   not Moving);

         Moving := True;

      end if;

   end Gnav_Touch_Move;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Gnav_Touch_End (T : Interfaces.IEEE_Float_64) is
      use Interfaces;
   begin

      if not Moving and T - Down_Time < 200.0 then

         Display.Main.Screen_Pressed (First_Down_X, First_Down_Y);

      end if;

      Pressing := False;
      Moving   := False;

   end Gnav_Touch_End;
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




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Gnav_Notification_Request return Interfaces.Unsigned_8 is
   begin

      return Utility.Notifications.Dequeue_Next;

   end Gnav_Notification_Request;
   -----------------------------------------------------------------------------

begin
   Initialize_Gnav;

end Gnav;
--------------------------------------------------------------------------------
