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

-- AdaWebPack
with Interfaces;
with Web.DOM.Event_Listeners;
with Web.DOM.Events;
with Web.Strings;
use  Web.Strings;
-- Gnav
with Gnav_Info;
with Flight.Wind;
with Timing.Events;
use  Timing.Events;
with Utility.Atmosphere;
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
package body Flight.Meteo is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Request : XML_Http_Request;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Get_Request : constant Web_String := To_Web_String ("GET");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Metar_File : constant Web_String := To_Web_String ("metar.bin");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Ready_State_Change : constant Web_String := To_Web_String ("readystatechange");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Interval between requests when the next metar is behind
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Request_Period : Duration := 15.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- This must be shorter than the request period (milliseconds)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Timeout_Period : Natural := 6000;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last received update
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Update : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Request : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Metar_Time : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Next_Metar : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Interval between position checks
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reposition_Period : Duration := 26.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Closest_Station_Name : Metar_Name := "ZZZZ";

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of loaded stations
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Number_Of_Stations : Natural := 0;

   --===========================================================================
   --
   --===========================================================================
   procedure Find_Closest_Station is

      use Utility.Calendar;
      use Utility.Strings;

      D     : Float := 0.0;
      D_Min : Float := Float'Last;
      Local : access Meteo_Station_Record := null;

   begin

      for S in Station_Range loop

         if Stations (S).Loaded then

            D := Distance (Flight.Data.Position, Stations (S).Position);

            if D < D_Min then
               D_Min := D;
               Local_Station := S;
            end if;

         end if;

      end loop;

      Local := Get_Local_Station;

      if Local.Loaded then

         Flight.Wind.Set_Metar_Wind (Float (Local.Wind_Speed),
                                     Float (Local.Wind_Course),
                                     Local.Date);

         if Local.Name /= Closest_Station_Name then

            Closest_Station_Name := Local.Name;

            Utility.Atmosphere.Set_Qnh (Float (Local.Qnh), True);

            On_Local_Station_Changed.Trigger;

         end if;

      end if;

   end Find_Closest_Station;
   -----------------------------------------------------------------------------



   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Epoch : constant Times := Time_Of (1970, 1, 1, 0.0);
   --===========================================================================
   --
   --===========================================================================
   procedure Process_Meteo_Request (S : in out Stream_Reader_Type) is

      I : Natural := 0;
      M : constant Short_Short_Natural := Short_Short_Natural (Station_Range'Last);

   begin

      Utility.Log.Put_Message ("processing meteo");

      Stations := (others => No_Meteo_Station_Record);

      if not S.Is_Empty then

         Metar_Time := Epoch + Long_Lapse_Of (S.Read_Long_Float);
         Next_Metar := Metar_Time + Lapse_Of (S.Read_Float);

         Utility.Log.Put_Message ("metar time is " & Utility.Strings.Day_Lapse_Image (Metar_Time));
         Utility.Log.Put_Message ("next update expected at " & Utility.Strings.Day_Lapse_Image (Next_Metar));

         Number_Of_Stations := Natural (Short_Short_Natural'Min (S.Read_Short_Short_Natural, M));

         Utility.Log.Put_Message (Utility.Strings.Integer_Image (Number_Of_Stations) & " active stations");

         for Station of Stations loop

            I := I + 1;
            exit when I > Number_Of_Stations;

            Station.Name         := S.Read_String (4);
            Station.Date         := Metar_Time + Lapse_Of (S.Read_Float);
            Station.Position.Lat := S.Read_Long_Float;
            Station.Position.Lon := S.Read_Long_Float;
            Station.Qnh          := Integer (S.Read_Short_Short_Integer) + 1013;
            Station.Wind_Speed   := Natural (S.Read_Short_Short_Natural);
            Station.Wind_Course  := Natural (S.Read_Short_Natural);
            Station.Temperature  := Integer (S.Read_Short_Short_Integer);
            Station.Dew_Point    := Integer (S.Read_Short_Short_Integer);
            Station.Cloud_Base   := Natural (S.Read_Short_Natural);
            Station.Visibility   := Natural (S.Read_Short_Natural);
            Station.Loaded       := True;

         end loop;

         Last_Update := Cached_Time;

      end if;

      Find_Closest_Station;

   end Process_Meteo_Request;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- An object handling the load event of a given region
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Listener_Record is new Web.DOM.Event_Listeners.Event_Listener with null record;

   --===========================================================================
   -- Handle ready event changes
   --===========================================================================
   overriding procedure Handle_Event (Self  : in out Listener_Record;
                                      Event : in out Web.DOM.Events.Event'Class) is
      use Interfaces;
      use Utility.Strings;

   begin

      case Request.Get_Ready_State is

         when Opened =>

            Request.Set_Request_Header (To_Web_String ("Cache-Control"),
                                        To_Web_String ("no-cache"));

            Request.Send (Web.Strings.Empty_Web_String);

         when Done =>

            if Request.Get_Status = 200 then

               declare
                  Reader : Stream_Reader_Type;
               begin
                  Request.Get_Response  (Reader);
                  Process_Meteo_Request (Reader);
               end;

            end if;

         when others =>
            null;

      end case;

   end Handle_Event;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Event_Listener : aliased Listener_Record;



   --===========================================================================
   -- Request a traffic update to the server. Give up after 4 seconds.
   --===========================================================================
   procedure Send_Meteo_Request is
   begin

      -- Only send requests when active and allowed
      --------------------------------------------------------------------------
      if not Gnav_Info.Request_Metar then
         return;
      end if;

      -- Only send request after the next expected period
      --------------------------------------------------------------------------
      if Next_Metar = No_Time or else (Next_Metar - Cached_Time) < No_Lapse then

         Request := Constructors.New_XML_Http_Request;

         Request.Add_Event_Listener (On_Ready_State_Change, Event_Listener'Access);

         Request.Open (Method => Get_Request,
                       URL    => Metar_File,
                       Async  => True);

         Request.Set_Timeout (Timeout_Period);

         Last_Request := Cached_Time;

      end if;

   end Send_Meteo_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      -- Start requesting metar updates
      ---------------------------------------

      if Gnav_Info.Request_Metar then

         Send_Meteo_Request;

         Timing.Events.Register_Timer (Request_Period, Send_Meteo_Request'Access);

         Timing.Events.Register_Timer (Reposition_Period, Find_Closest_Station'Access);

      end if;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Indicates if the data is the most recent one
   --===========================================================================
   function Updated return Boolean is
   begin

      return Next_Metar /= No_Time and then Next_Metar - Cached_Time > No_Lapse;

   end Updated;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Station (S : Station_Range) return not null access Meteo_Station_Record is
   begin

      return Stations (S)'Access;

   end Get_Station;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Local_Station return Station_Range is
   begin

      return Local_Station;

   end Get_Local_Station;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Local_Station return not null access Meteo_Station_Record is
   begin

      return Stations (Local_Station)'Access;

   end Get_Local_Station;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the number of active stations
   --===========================================================================
   function Get_Number_Of_Stations return Natural is
   begin

      return Number_Of_Stations;

   end Get_Number_Of_Stations;
   -----------------------------------------------------------------------------

end Flight.Meteo;
--------------------------------------------------------------------------------
