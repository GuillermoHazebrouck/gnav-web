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

   --===========================================================================
   --
   --===========================================================================
   procedure Process_Next_Request;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Get_Request : constant Web_String := To_Web_String ("GET");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ready change event name
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   On_Ready_State_Change : constant Web_String := To_Web_String ("readystatechange");

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Handler : Binary_Response_Handler := null;

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

            Request.Send (Web.Strings.Empty_Web_String);

         when Done =>

            if Request.Get_Status = 200 then

               Utility.Log.Put_Message ("processing resource");

               declare
                  Reader : Stream_Reader_Type := Request.Get_Response;
               begin
                  Active_Handler (Reader);
               end;

            else
               Utility.Log.Put_Message ("something went wrong while fetching resource");

               declare
                  Dummy : Stream_Reader_Type := No_Stream_Reader;
               begin

                  -- TODO: check this, it seeps to cause an exception
                  -- Active_Handler (Dummy);
                  null;

               end;

            end if;

            Active_Handler := null;

            Process_Next_Request;

         when others =>
            null;

      end case;

   end Handle_Event;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- NOTE: this does not seem to be working
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Event_Listener : aliased Listener_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   R : Natural := 1;

   Init : Boolean := False;
   --===========================================================================
   --
   --===========================================================================
   procedure Process_Next_Request is
   begin

      if Active_Handler = null then

         -- Make a full round trip and exit immediatly after first request
         -----------------------------------------------------------------------

         for I in Request_Queue'Range loop

            if R not in Request_Queue'Range then
               R := Request_Queue'First;
            end if;

            if Request_Queue (R).Handler /= null then

               Utility.Log.Put_Message ("requesting resource " & Natural'Image (R));

               Active_Handler := Request_Queue (R).Handler;

               Request := Constructors.New_XML_Http_Request;

               Request.Add_Event_Listener (On_Ready_State_Change, Event_Listener'Access);

               Request.Open (Method => Get_Request,
                             URL    => Request_Queue (R).Name,
                             Async  => True);

               Request.Set_Response_Type (Array_Buffer);

               Request.Set_Timeout (Request_Queue (R).Timeout);

               Request_Queue (R).Handler := null;

               Request_Queue (R).Timeout := 0;

               Request_Queue (R).Name    := Empty_Web_String;

               exit;

            end if;

            R := R + 1;

         end loop;

      end if;

   end Process_Next_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Request_Binary_Resource (Name    : String;
                                      Handler : Binary_Response_Handler;
                                      Timeout : Natural := 0) is

      N : Natural := R;

   begin

      if Handler /= null and then Name /= "" then

         -- Put the request on the next void
         -------------------------------------------------

         for I in Request_Queue'Range loop

            if N not in Request_Queue'Range then
               N := Request_Queue'First;
            end if;

            if Request_Queue (N).Handler = null then

               Request_Queue (N).Handler := Handler;

               Request_Queue (N).Name    := To_Web_String (Utility.Strings.To_Wide_Wide_String (Name));

               Request_Queue (N).Timeout := Timeout;

               Utility.Log.Put_Message ("resource request registered " & Natural'Image (N) & " (" &  Name & ")");

               exit;

            end if;

            N := N + 1;

         end loop;

         Process_Next_Request;

      end if;

   end Request_Binary_Resource;
   -----------------------------------------------------------------------------

end Utility.Resources;
--------------------------------------------------------------------------------
