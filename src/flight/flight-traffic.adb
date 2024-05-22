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
with Timing.Events;
use  Timing.Events;
with Utility.Log;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;
with Utility.Strings;
with Math.Vector2;
use  Math.Vector2;


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
   Timeout_Period : Natural := Natural (Request_Period - 2.0) * 1000;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Update : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Track : Times := No_Time;

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




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Status return Aprs_Status is
   begin

      if Gnav_Info.Request_Traffic and Enabled then
         if Last_Update = No_Time or Last_Track = No_Time then
            return Aprs_Not_Receiving;
         else
            if (Cached_Time - Last_Update) > Obsolence then
               return Aprs_Not_Receiving;
            elsif (Cached_Time - Last_Track) > Obsolence then
               return Aprs_Not_Reporting;
            else
               return Aprs_Nominal;
            end if;
         end if;
      else
         return Aprs_Disabled;
      end if;

   end Get_Status;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Process_Traffic_Request (S : in out Stream_Reader_Type) is
   begin

      Utility.Log.Put_Message ("processing traffic" & Natural'Image (S.Get_Size));

      if not S.Is_Empty then

         declare
            use Utility.Strings;
            T        : Natural;
            R        : Position_Record;
            N        : Natural;
            Id       : Natural;
            Lat      : Float;
            Lon      : Float;
            Age      : Short_Short_Integer;
            Altitude : Short_Natural;
            Speed    : Short_Short_Natural;
            Course   : Short_Short_Natural;
            Vario    : Short_Short_Integer;
            Rotation : Short_Short_Integer;
            Last     : Short_Short_Integer := Short_Short_Integer'First;
         begin

            T     := S.Read_Natural;
            R.Lat := S.Read_Long_Float;
            R.Lon := S.Read_Long_Float;

            N := Natural (S.Read_Short_Short_Natural);
            if N > Traffic_Data'Last then
               N := Traffic_Data'Last;
            end if;

            Utility.Log.Put_Message ("#" & Natural'Image (N));

            for I in 1..N loop

               Id       := S.Read_Natural;
               Age      := S.Read_Short_Short_Integer;
               Lat      := S.Read_Float;
               Lon      := S.Read_Float;
               Altitude := S.Read_Short_Natural;
               Speed    := S.Read_Short_Short_Natural;
               Course   := S.Read_Short_Short_Natural;
               Vario    := S.Read_Short_Short_Integer;
               Rotation := S.Read_Short_Short_Integer;

               Traffic_Data (I).Time_Stamp := Cached_Time;
               Set_Clock (Traffic_Data (I).Time_Stamp, Day_Lapse (T + Integer (Age)));
               Traffic_Data (I).Last_Integral := Traffic_Data (I).Time_Stamp;

               --if (Traffic_Data (I).Time_Stamp - Cache_Time) > No_Lapse then
               --   Utility.Log.Put_Message ("warning: time track is ahead of clock");
               --end if;

               Traffic_Data (I).Id           := Id;
               Traffic_Data (I).Position.Lat := R.Lat + Long_Float (Lat);
               Traffic_Data (I).Position.Lon := R.Lon + Long_Float (Lon);
               Traffic_Data (I).Altitude     := Natural (Altitude);
               Traffic_Data (I).Speed        := Float (Speed)    * 2.0; -- km/h
               Traffic_Data (I).Course       := Float (Course)   * 1.5; -- deg
               Traffic_Data (I).Vario        := Float (Vario)    * 0.1; -- m/s
               Traffic_Data (I).Rotation     := Float (Rotation);       -- deg/s
               Traffic_Data (I).Active       := True;
               Traffic_Data (I).Coasted      := False;

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

         end;

         Last_Update := Cached_Time;

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

   begin

      case Request.Get_Ready_State is

         when Opened =>

            Request.Set_Request_Header (To_Web_String ("Cache-Control"),
                                        To_Web_String ("no-cache"));

            Request.Send (Web.Strings.Empty_Web_String);

         when Done =>

            if Request.Get_Status = 200 then

               declare
                  Reader : Stream_Reader_Type := Request.Get_Response;
               begin
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

      if not Enabled or not Gnav_Info.Request_Traffic then
         return;
      end if;

      Utility.Log.Put_Message ("request traffic");

      Request := Constructors.New_XML_Http_Request;

      Request.Add_Event_Listener (On_Ready_State_Change, Event_Listener'Access);

      Request.Open (Method => Get_Request,
                    URL    => Traffic_File,
                    Async  => True);

      Request.Set_Response_Type (Array_Buffer);

      Request.Set_Timeout (Timeout_Period);

   end Send_Traffic_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      if Gnav_Info.Request_Traffic then

         Utility.Log.Put_Message ("traffic request enabled");

       --Timing.Events.Register_Timer (1.0, Maintain_Tracks'Access);

         Timing.Events.Register_Timer (Request_Period, Send_Traffic_Request'Access);

      else
         Utility.Log.Put_Message ("traffic request disabled");

      end if;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- > Marks the tracks older than 6 seconds as coasted.
   -- > Disables the tracks that are obsolete.
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
            --------------------------------------------------------------------

            Age := Cached_Time - Track.Time_Stamp;

            if    Age > Obsolence then

               Track.Active  := False;

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
