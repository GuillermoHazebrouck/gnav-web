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
-- AdaWebPack
with Interfaces;
with Web.DOM.Events;
with Web.DOM.Event_Listeners;
with Web.Strings;
use  Web.Strings;
-- Gnav
with Utility.Requests;
use  Utility.Requests;
with Utility.Log;
with Utility.Strings;



--//////////////////////////////////////////////////////////////////////////////
-- This package uses Utility.Requests to generate secuential requests
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Resources is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active request
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Request : XML_Http_Request;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Ongoing_Request : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The maximum number of times a request can be restarted
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Maximum_Trials : constant Natural := 5;

   --===========================================================================
   --
   --===========================================================================
   procedure Process_Next_Request;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The listener to handle a particular resource
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Request_Record is new Web.DOM.Event_Listeners.Event_Listener with record

      Name    : Web_String;

      Handler : Binary_Response_Handler;

      Timeout : Natural;

      Counter : Natural;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Resource_Range is new Positive range 1..20;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   R : Resource_Range := Resource_Range'First;

   --===========================================================================
   -- Handle ready event changes
   --===========================================================================
   overriding procedure Handle_Event (Self  : in out Request_Record;
                                      Event : in out Web.DOM.Events.Event'Class) is
      use Interfaces;

   begin

      case Request.Get_Ready_State is

         when Opened =>

            Request.Send (Web.Strings.Empty_Web_String);

            Utility.Log.Put_Message ("request sent for resource" & Resource_Range'Image (R));

         when Done =>

            if Request.Get_Status = 200 then

               Utility.Log.Put_Message ("processing resource" & Resource_Range'Image (R));

               declare
                  Reader : Stream_Reader_Type;
               begin
                  Request.Get_Response (Reader);
                  Self.Handler (Reader);
               end;

               Self.Handler := null;

            else
               Utility.Log.Put_Message ("something went wrong while fetching resource" & Resource_Range'Image (R));

               -- Give up resource after several trials

               Self.Counter := Self.Counter - 1;

               if Self.Counter = 0 then
                  Utility.Log.Put_Message ("giving up resource" & Resource_Range'Image (R));
                  Self.Handler := null;
               end if;

            end if;

            Ongoing_Request := False;

            Process_Next_Request;

         when others =>
            null;

      end case;

   end Handle_Event;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Request_Queue : array (Resource_Range) of aliased Request_Record := (others => (Name    => Empty_Web_String,
                                                                                   Handler => null,
                                                                                   Timeout => 0,
                                                                                   Counter => Maximum_Trials));

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Get_Request : constant Web_String := To_Web_String ("GET");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Ready_State_Change : constant Web_String := To_Web_String ("readystatechange");



   --===========================================================================
   --
   --===========================================================================
   procedure Process_Next_Request is
   begin

      if not Ongoing_Request then

         -- Make a full round trip and exit immediatly after first request
         -----------------------------------------------------------------------

         for I in Resource_Range loop

            if Request_Queue (R).Handler /= null then

               Utility.Log.Put_Message ("requesting resource " & Resource_Range'Image (R));

               Ongoing_Request := True;

               Request := Constructors.New_XML_Http_Request;

               Request.Add_Event_Listener (On_Ready_State_Change, Request_Queue (R)'Access);

               Request.Open (Method => Get_Request,
                             URL    => Request_Queue (R).Name,
                             Async  => True);

               Request.Set_Timeout (Request_Queue (R).Timeout);

               return;

            end if;

            if R = Resource_Range'Last then
               R := Resource_Range'First;
            else
               R := R + 1;
            end if;

         end loop;

      end if;

   end Process_Next_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Request_Binary_Resource (Name    : String;
                                      Handler : Binary_Response_Handler;
                                      Timeout : Natural := 8000) is

      N : Resource_Range := R;

   begin

      if Handler /= null and then Name /= "" then

         -- Put the request on the next void
         -------------------------------------------------

         for I in Resource_Range'Range loop

            if Request_Queue (N).Handler = null then

               Request_Queue (N).Handler := Handler;

               Request_Queue (N).Name    := To_Web_String (Utility.Strings.To_Wide_Wide_String (Name));

               Request_Queue (N).Timeout := Timeout;

               Request_Queue (N).Counter := Maximum_Trials;

               Utility.Log.Put_Message ("resource request registered" & Resource_Range'Image (N) & " (" &  Name & ")");

               exit;

            end if;

            if N = Resource_Range'Last then
               N := Resource_Range'First;
            else
               N := N + 1;
            end if;

         end loop;

         Process_Next_Request;

      end if;

   end Request_Binary_Resource;
   -----------------------------------------------------------------------------

end Utility.Resources;
--------------------------------------------------------------------------------
