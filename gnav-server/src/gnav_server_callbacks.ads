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
-- AdaWebServer
with Aws.Response;
with Aws.Status;


--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package Gnav_Server_Callbacks is

   use Aws;

   --===========================================================================
   --
   --===========================================================================
   function Handle_Request (Request : Status.Data) return Response.Data;

   --===========================================================================
   --
   --===========================================================================
   function Set_Password (File : String) return String;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Server stats (with thread synchoronization)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected Server_Stats is

      function Get_Number_Of_Wasm_Requests return Natural;
      function Get_Number_Of_Aprs_Requests return Natural;

      procedure Increase_Number_Of_Wasm_Requests;
      procedure Increase_Number_Of_Aprs_Requests;

      procedure Reset;

   private

      Number_Of_Wasm_Requests : Natural := 0;
      Number_Of_Aprs_Requests : Natural := 0;

   end Server_Stats;
   ----------------------------------------------------------------------------

end Gnav_Server_Callbacks;
--------------------------------------------------------------------------------
