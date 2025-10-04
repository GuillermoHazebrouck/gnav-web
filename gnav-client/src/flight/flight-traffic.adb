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
with Flight;
with Flight.Plan;
with Gnav_Info;
with Timing.Events;
use  Timing.Events;
with Utility.Log;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;
with Utility.Storage;
with Utility.Strings;
with Math.Vector2;
use  Math.Vector2;
with Maps;
with Maps.Terrain;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Traffic is

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
   Traffic_File : constant Web_String := To_Web_String ("traffic.bin");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Ready_State_Change : constant Web_String := To_Web_String ("readystatechange");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Request_Period : Duration := 6.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- This must be shorter than the request period
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Timeout_Period : Natural := Natural (0.8 * Request_Period) * 1000;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Update : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Track : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The maximum offset ahead: tracks shoud not be ahead of the clock.
   -- TODO: at 0:00 UTC the track ages should be compensated for 10 seconds.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Offset_Ahead : constant Lapses := Lapse_Of (-1.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Obsolence : constant Lapses := Lapse_Of (30.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Coasting  : constant Lapses := Lapse_Of (10.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Rad : constant float := Float (Math.Pi / 180.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --Monitor : array (1..10) of Boolean := (others => False);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --M : Positive := Monitor'First;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Status return Aprs_Status is
   begin

      if Gnav_Info.Request_Traffic and Enabled then

         if Last_Update = No_Time or else
            (Cached_Time - Last_Update) > Obsolence
         then
            return Aprs_Not_Receiving;
         elsif
           Last_Track = No_Time                   or else
           (Cached_Time - Last_Track) > Obsolence or else
           (Cached_Time - Last_Track) < Offset_Ahead
         then
            return Aprs_Not_Reporting;
         else
            return Aprs_Nominal;
         end if;

      else
         return Aprs_Disabled;
      end if;

   end Get_Status;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Aprs_Data return String is

      use Utility.Strings;

   begin

      if Follow and then Data.Position /= No_Position_Record then

         -- Record data and sent traffic around actual location

         return Data.Get_Igc_Image;

      elsif Plan.Home_Waypoint.Position /= No_Position_Record then

         -- Only send data around the route home location

         return 'H' & Maps.Compact_Image (Plan.Home_Waypoint.Position);

      else

         -- Only send data around the system home location

         return 'S' & Maps.Compact_Image (Gnav_Info.Home_Position);

      end if;

   end Get_Aprs_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Process_Traffic_Request (S : in out Stream_Reader_Type) is
   begin

      if not S.Is_Empty then

         declare
            use Utility.Strings;
            T        : Natural;
            R        : Position_Record;
            N        : Natural;
            Lat      : Float;
            Lon      : Float;
            Flags    : Short_Short_Natural;
            Age      : Short_Short_Integer;
            Altitude : Short_Natural;
            Speed    : Short_Short_Natural;
            Course   : Short_Short_Natural;
            Vario    : Short_Short_Integer;
            Rotation : Short_Short_Integer;
            Last     : Short_Short_Integer := Short_Short_Integer'First;
            Tailmark : String (1..2);
            Pilot    : String (1..2);
         begin

            T     := S.Read_Natural;
            R.Lat := S.Read_Long_Float;
            R.Lon := S.Read_Long_Float;

            N := Natural (S.Read_Short_Short_Natural);
            if N > Traffic_Data'Last then
               N := Traffic_Data'Last;
            end if;
            Number_Of_Tracks := N;

            for I in 1..N loop

               Flags    := S.Read_Short_Short_Natural;
               Age      := S.Read_Short_Short_Integer;
               Lat      := S.Read_Float;
               Lon      := S.Read_Float;
               Altitude := S.Read_Short_Natural;
               Speed    := S.Read_Short_Short_Natural;
               Course   := S.Read_Short_Short_Natural;
               Vario    := S.Read_Short_Short_Integer;
               Rotation := S.Read_Short_Short_Integer;
               Tailmark := S.Read_String (2);
               Pilot    := S.Read_String (2);

               Traffic_Data (I).Time_Stamp := Cached_Time;
               Set_Clock (Traffic_Data (I).Time_Stamp, Day_Lapse (T + Integer (Age)));
               Traffic_Data (I).Last_Integral := Traffic_Data (I).Time_Stamp;

               Traffic_Data (I).Id           := 1;
               Traffic_Data (I).Position.Lat := R.Lat + Long_Float (Lat);
               Traffic_Data (I).Position.Lon := R.Lon + Long_Float (Lon);
               Traffic_Data (I).Altitude     := Natural (Altitude);
               Traffic_Data (I).Speed        := Float (Speed)    * 2.0; -- km/h
               Traffic_Data (I).Course       := Float (Course)   * 1.5; -- deg
               Traffic_Data (I).Vario        := Float (Vario)    * 0.1; -- m/s
               Traffic_Data (I).Rotation     := Float (Rotation);       -- deg/s
               Traffic_Data (I).Active       := True;
               Traffic_Data (I).Coasted      := False;
               Traffic_Data (I).Tailmark     := Tailmark;
               Traffic_Data (I).Pilot        := Pilot;

               -- Keep the most recent time stap to check the system status
               -----------------------------------------------------------------
               if Last_Track = No_Time or else Age > Last then
                  Last := Age;
                  Last_Track := Traffic_Data (I).Time_Stamp;
               end if;
               -----------------------------------------------------------------

            end loop;

            for I in N + 1 .. Traffic_Data'Last loop
               Traffic_Data (I).Active := False;
            end loop;

            -- Pick the user Id if available
            --------------------------------------------------------------------
            if S.Get_Remaining_Size >= 8 then

               declare
                  User_Id  : Gnav_Info.Id_Type := Gnav_Info.No_Id;
               begin

                  Override (User_Id, S.Read_String (8));

                  if
                    User_Id /= Gnav_Info.No_Id and then
                    User_Id /= Gnav_Info.User_Id
                  then

                     Gnav_Info.User_Id := User_Id;

                     Utility.Storage.Set_Item ("ID", User_Id);

                  end if;

               end;

            end if;

         end;

         Last_Update := Cached_Time;

      else
         Number_Of_Tracks := 0;

      end if;

   end Process_Traffic_Request;
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

            -- Feed automatic postion reporting system: location + altitude
            --------------------------------------------------------------------
            Request.Set_Request_Header (To_Web_String ("APRS"),
                                        To_Web_String (To_Wide_Wide_String (Get_Aprs_Data)));

            -- Feed user Id for data logging: No_Id will request for new Id
            --------------------------------------------------------------------
            if Gnav_Info.User_Id /= Gnav_Info.No_Id then

               Request.Set_Request_Header (To_Web_String ("CODE"),
                                           To_Web_String (To_Wide_Wide_String (Gnav_Info.User_Id)));

            else

               Request.Set_Request_Header (To_Web_String ("CODE"),
                                           To_Web_String (To_Wide_Wide_String (Gnav_Info.Zz_Id)));

            end if;

            if Gnav_Info.User_Tm /= Gnav_Info.No_Tm then

               Request.Set_Request_Header (To_Web_String ("MARK"),
                                           To_Web_String (To_Wide_Wide_String (Gnav_Info.User_Tm)));

            end if;
            --------------------------------------------------------------------

            Request.Send (Web.Strings.Empty_Web_String);

         when Done =>

            if Request.Get_Status = 200 then

               declare
                  Reader : Stream_Reader_Type;
               begin
                  Request.Get_Response    (Reader);
                  Process_Traffic_Request (Reader);
               end;

            else
               Utility.Log.Put_Message ("missed traffic update");

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
   procedure Send_Traffic_Request is
   begin

      -- Only send requests when active and allowed
      if not Enabled or not Gnav_Info.Request_Traffic or Gnav_Info.Simulation_Mode then
         return;
      end if;

      -- Only send requests inside the coverage area and when moving
      if
        not Maps.Terrain.Inside_Area (Flight.Data.Position) or else
        (Gnav_Info.Aprs_Airborne_Only and then Flight.Data.Speed < 10.0)
      then
         return;
      end if;

      Request := Constructors.New_XML_Http_Request;

      Request.Add_Event_Listener (On_Ready_State_Change, Event_Listener'Access);

      Request.Open (Method => Get_Request,
                    URL    => Traffic_File,
                    Async  => True);

      Request.Set_Timeout (Timeout_Period);

   end Send_Traffic_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Save_Configuration is
   begin

      if Enabled then
         if Follow then
            Utility.Storage.Set_Item ("TRAFFIC", "F");
         else
            Utility.Storage.Set_Item ("TRAFFIC", "H");
         end if;
      else
         Utility.Storage.Set_Item ("TRAFFIC", "N");
      end if;

   end Save_Configuration;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is

      Traffic_Flag : String := Utility.Storage.Get_Item ("TRAFFIC");

   begin

      if Gnav_Info.Request_Traffic then

         Utility.Log.Put_Message ("traffic request enabled");

         Timing.Events.Register_Timer (Request_Period, Send_Traffic_Request'Access);

      else
         Utility.Log.Put_Message ("traffic request disabled");

      end if;

      Utility.Log.Put_Message ("traffic config is " & Traffic_Flag);
      Follow  := Traffic_Flag = "F";
      Enabled := Follow or else Traffic_Flag = "H";

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- > Marks the tracks older than 6 seconds as coasted.
   -- > Disables the tracks that are obsolete.
   -- > TODO: only maintain visible tracks
   --===========================================================================
   procedure Maintain_Tracks is

      Age : Lapses := No_Lapse;

   begin

      for Track of Traffic_Data loop

         if Track.Active then

            -- Integrate position up to the coasting lapse
            --------------------------------------------------------------------

            if not Track.Coasted then

               declare
                  T : Lapses := Cached_Time - Track.Last_Integral;
                  V : Vector2_Record;
                  R : Vector2_Record;
                  A : Float;
                  S : Natural;
               begin

                  if T > Coasting then
                     T := Coasting;
                  elsif T < No_Lapse then
                     T := No_Lapse;
                  end if;

                  S := Integer (Seconds (T));

                  if S > 0 then

                     V.Set    (0.0, 1.0);
                     V.Rotate (Long_Float (-Track.Course * Rad));
                     V.Scale  (Long_Float ( Track.Speed / 3600.0));

                     if Track.Rotation = 0.0 then
                        V.Scale (Long_Float (S));
                        Track.Position := Position (Track.Position, V);
                     else
                        R.Set (0.0, 0.0);
                        A := 0.0;
                        for I in 1..S loop
                           A := A + Track.Rotation * Float (I) / Float (S); --> TODO: review fading factor
                           V.Rotate (Long_Float (-A * Rad));
                           R.Add (V);
                        end loop;
                        Track.Course   := Track.Course + A;
                        Track.Position := Position (Track.Position, R);
                     end if;

                  end if;

                  Track.Last_Integral := Cached_Time;

               end;

            end if;

            -- Deactivate or coast the track based on its age
            -- NOTE: we do not keep track on the date, only on the hour.
            --------------------------------------------------------------------

            Age := Cached_Time - Track.Time_Stamp;

            if
              Age < Offset_Ahead or else
              Age > Obsolence
            then

               Track.Active := False;

            elsif Age > Coasting  then

               Track.Coasted := True;

            else

               Track.Coasted := False;

            end if;

         end if;

      end loop;

   end Maintain_Tracks;
   -----------------------------------------------------------------------------


end Flight.Traffic;
--------------------------------------------------------------------------------
