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
with Ada.Calendar;
use  Ada.Calendar;
-- Local
with Utility;
with Utility.Ids;
use  Utility.Ids;
with Utility.Maps;
use  Utility.Maps;

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package Users is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type User_Record is tagged record

      Id        : Id_Type;

      Mark      : Track_Marks;

      Timestamp : Ada.Calendar.Time;

      Count     : Natural;

      Ogn_Id    : Natural;

      Time      : Natural;

      Position  : Position_Record;

      Altitude  : Float;

      Course    : Float;

      Speed     : Float;

   end record;

   --===========================================================================
   --
   --===========================================================================
   procedure Load_Data (This : in out User_Record; Track_Data : String);

   --===========================================================================
   --
   --===========================================================================
   function Valid_Position (This : User_Record) return Boolean;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_User : constant User_Record := (Id        => No_Id,
                                      Mark      => No_Mark,
                                      Timestamp => Ada.Calendar.Clock - 4000.0,
                                      Count     => 0,
                                      Ogn_Id    => 0,
                                      Time      => 0,
                                      Position  => No_Position_Record,
                                      Altitude  => 0.0,
                                      Course    => 0.0,
                                      Speed     => 0.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type User_Stack_Array is array (1..50) of User_Record;
   type User_Id_Array    is array (1..50) of Id_Type;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected Users_Stack is

      --========================================================================
      -- Adds or updates the user to the list and removes old users
      --========================================================================
      procedure Process_User (User : User_Record; Track_Data : String);

      --========================================================================
      -- Adds the ID to the list of registered users (for logs)
      --========================================================================
      procedure Add_To_Register (Id : Id_Type);

   private

      --========================================================================
      -- Indicates if the user is in the local register (for logs)
      --========================================================================
      function Registered (Id : Id_Type) return Boolean;

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      --
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Users : User_Stack_Array := (others =>  No_User);

      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      --
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Register : User_Id_Array := (others => No_Id);

   end Users_Stack;
   -----------------------------------------------------------------------------

end Users;
--------------------------------------------------------------------------------
