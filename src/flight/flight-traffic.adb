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
with Gnav_Info;
with Timing.Events;
use  Timing.Events;
with Utility.Log;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Traffic is

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
   procedure Send_Traffic_Request;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Request a traffic update every 4 seconds
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Traffic_Request : access Timer_Record := Register_Timer (6.0, Send_Traffic_Request'Access);




   --===========================================================================
   --
   --===========================================================================
   procedure Process_Traffic_Request (S : in out Stream_Reader_Type) is
   begin

      Utility.Log.Put_Message ("processing traffic");
      Utility.Log.Put_Message ("size" & Natural'Image (S.Get_Size));

      if not S.Is_Empty then

         Last_Update := Cached_Time;

      end if;

      Traffic_Request.Resume;

   end Process_Traffic_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Request a traffic update to the server. Give up after 4 seconds.
   --===========================================================================
   procedure Send_Traffic_Request is
   begin

      if not Gnav_Info.Request_Traffic then
         return;
      end if;

      Last_Request := Cached_Time;

      Utility.Resources.Request_Binary_Resource (Name    => "traffic.bin",
                                                 Handler => Process_Traffic_Request'Access,
                                                 Timeout => 4000);

      Traffic_Request.Pause;

   end Send_Traffic_Request;
   -----------------------------------------------------------------------------



   procedure Maintain_Tracks;

   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      Timing.Events.Register_Timer (1.0, Maintain_Tracks'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- > Marks the tracks older than 4 seconds as coasted.
   -- > Removes the tracks that are older than 8 seconds.
   --===========================================================================
   procedure Maintain_Tracks is

      Age : Lapses := No_Lapse;

   begin

      for T in Traffic_Range loop

         if Traffic_Data (T).Active then

            Age := (Cached_Time - Traffic_Data (T).Time_Stamp);

            if Age > Lapse_Of (8.0) then

               Traffic_Data (T) := No_Traffic_Record;

            elsif Age > Lapse_Of (4.0) then

               Traffic_Data (T).Coasted := True;

            end if;

         end if;

      end loop;

   end Maintain_Tracks;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Keep_Track (Element : Traffic_Record) is
   begin

      for T in Traffic_Range loop

         if Traffic_Data (T).Id = Element.Id then

            Traffic_Data (T)            := Element;

            Traffic_Data (T).Active     := True;

            Traffic_Data (T).Time_Stamp := Cached_Time;

            Traffic_Data (T).Coasted    := False;

            return;

         end if;

      end loop;

      for T in Traffic_Range loop

         if not Traffic_Data (T).Active then

            Traffic_Data (T)            := Element;

            Traffic_Data (T).Active     := True;

            Traffic_Data (T).Time_Stamp := Cached_Time;

            Traffic_Data (T).Coasted    := False;

            return;

         end if;

      end loop;

   end Keep_Track;
   -----------------------------------------------------------------------------

end Flight.Traffic;
--------------------------------------------------------------------------------
