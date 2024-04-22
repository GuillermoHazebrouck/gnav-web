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
with Utility.Calendar;
use  Utility.Calendar;
-- AdaWebPack
with WASM.Console;
with Web.Strings;
-- Gnav
with Timing.Events;
with Utility.Strings;




--//////////////////////////////////////////////////////////////////////////////
-- NOTE: implementation missing
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Log is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Debug : constant Boolean := True;

   --===========================================================================
   --
   --===========================================================================
   procedure Flush_Log is
   begin

      null;

   end Flush_Log;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      -- Flush the file every 5 seconds
      ------------------------------------
      Timing.Events.Register_Timer (5.0, Flush_Log'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Close is
   begin

      null;

   end Close;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Puts the message in the log file
   --===========================================================================
   procedure Put_Message (Message : String) is

      use Web.Strings;

   begin

      if Debug then

         WASM.Console.Log (To_Web_String (Utility.Strings.To_Wide_Wide_String (Message)));

      end if;

   end Put_Message;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Puts the message in the log file
   --===========================================================================
   procedure Put_Message (E : Exception_Occurrence; Message : String) is
   begin

      null;

   end Put_Message;
   -----------------------------------------------------------------------------

end Utility.Log;
--------------------------------------------------------------------------------
