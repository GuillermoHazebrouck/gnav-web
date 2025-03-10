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
with Web.Strings;
use  Web.Strings;
-- Gnav
with Utility.Requests;
use  Utility.Requests;


--//////////////////////////////////////////////////////////////////////////////
-- This package uses Utility.Requests to generate secuential requests
--//////////////////////////////////////////////////////////////////////////////
package Utility.Resources is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A handler tailored to access an specific resource
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Binary_Response_Handler is access procedure (Reader : in out Stream_Reader_Type);

   --===========================================================================
   -- Registers an asynchrounous request in a FIFO queue. The request is sent
   -- as soon as all previous request have been handled.
   --===========================================================================
   procedure Request_Binary_Resource (Name : String; Handler : Binary_Response_Handler; Timeout : Natural := 8000);

end Utility.Resources;
--------------------------------------------------------------------------------
