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
with Interfaces;
use  Interfaces;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Notifications is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Notify : array (Notification_Kinds) of Boolean;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Add_Notification (Kind : Notification_Kinds) is
   begin

      Notify (Kind) := True;

   end Add_Notification;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Dequeue_Next return Interfaces.Unsigned_8 is
   begin

      for Kind in Notification_Kinds loop

         if Notify (Kind) then

            Notify (Kind) := False;

            case Kind is
               when Notify_Sector => return 1;
               when Notify_Sink   => return 2;
               when Notify_Range  => return 3;
            end case;

         end if;

      end loop;

      return 0;

   end Dequeue_Next;
   -----------------------------------------------------------------------------

end Utility.Notifications;
--------------------------------------------------------------------------------
