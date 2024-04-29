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
   Request_Period : Duration := 8.0;

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
   Last_Request : Times := No_Time;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Standard Unix epoch
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Epoch : constant Times := Time_Of (1970, 1, 1, 0.0);

   --===========================================================================
   --
   --===========================================================================
   procedure Process_Traffic_Request (S : in out Stream_Reader_Type) is
   begin

      Utility.Log.Put_Message ("processing traffic" & Natural'Image (S.Get_Size));

      if not S.Is_Empty then

         declare
            use Utility.Strings;
            L        : Long_Float;
            T        : Times := No_Time;
            N        : Short_Natural;
            Id       : Traffic_Id;
            Lat      : Float;
            Lon      : Float;
            Age      : Float;
            Altitude : Short_Natural;
            Speed    : Short_Short_Natural;
            Course   : Short_Short_Natural;
            A        : Natural := 0;
         begin

            L := S.Read_Long_Float;
            T := Epoch + Long_Lapse_Of (L);
            N := S.Read_Short_Natural;

            for I in 1..N loop

               Override (Id, S.Read_String (6));

               Age      := S.Read_Float;
               Lat      := S.Read_Float;
               Lon      := S.Read_Float;
               Altitude := S.Read_Short_Natural;
               Speed    := S.Read_Short_Short_Natural;
               Course   := S.Read_Short_Short_Natural;

               for I in Traffic_Data'Range loop
                  if Traffic_Data (I).Id = Id then
                     A := I;
                     exit;
                  elsif A = 0 and not Traffic_Data (I).Active then
                     A := I;
                  end if;
               end loop;

               if A in Traffic_Data'Range then

                  Traffic_Data (A).Time_Stamp   := T + Lapse_Of (Age);
                  Traffic_Data (A).Position.Lat := Gnav_Info.Home_Position.Lat + Long_Float (Lat);
                  Traffic_Data (A).Position.Lon := Gnav_Info.Home_Position.Lon + Long_Float (Lon);
                  Traffic_Data (A).Altitude     := Float (Altitude);
                  Traffic_Data (A).Speed        := Float (Speed) * 3.6;
                  Traffic_Data (A).Course       := Float (Course);
                  Traffic_Data (A).Active       := True;
                  Traffic_Data (A).Coasted      := False;

               end if;

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

      if not Active then
         return;
      end if;

      Last_Request := Cached_Time;

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



   procedure Maintain_Tracks;

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      if Gnav_Info.Request_Traffic then

         Utility.Log.Put_Message ("traffic request enabled");

         Timing.Events.Register_Timer (1.0, Maintain_Tracks'Access);

         Timing.Events.Register_Timer (Request_Period, Send_Traffic_Request'Access);

      else
         Utility.Log.Put_Message ("traffic request disabled");

      end if;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- > Marks the tracks older than 4 seconds as coasted.
   -- > Removes the tracks that are older than 8 seconds.
   --===========================================================================
   procedure Maintain_Tracks is

      Age : Lapses := No_Lapse;

   begin

      for T in Traffic_Data'Range loop

         if Traffic_Data (T).Active then

            Age := (Cached_Time - Traffic_Data (T).Time_Stamp);

            if Age > Lapse_Of (10.0) then

               Traffic_Data (T) := No_Traffic_Record;

            elsif Age > Lapse_Of (4.0) then

               Traffic_Data (T).Coasted := True;

            end if;

         end if;

      end loop;

   end Maintain_Tracks;
   -----------------------------------------------------------------------------


end Flight.Traffic;
--------------------------------------------------------------------------------
