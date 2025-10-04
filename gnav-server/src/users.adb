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
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Text_IO;
-- Gnav
with Utility;
with Utility.Ids;
with Utility.Log;
use  Utility.Log;

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package body Users is

   --===========================================================================
   --
   --===========================================================================
   procedure Load_Data (This : in out User_Record; Track_Data : String) is

      use Ada.Calendar;

   begin

      This.Timestamp := Clock;

      -- Parse downlinked IGC track data
      -----------------------------------------------------------------------
      if
        Track_Data'First  =  1 and then
        Track_Data'Length > 40 and then
        Track_Data (1) = 'B'
      then
         This.Time :=             Natural'Value (Track_Data (2..3)) * 3600;
         This.Time := This.Time + Natural'Value (Track_Data (4..5)) * 60;
         This.Time := This.Time + Natural'Value (Track_Data (6..7));

         -- DDMMmmm
         This.Position.Lat := Long_Float'Value (Track_Data (8..9));
         This.Position.Lat := This.Position.Lat + Long_Float'Value (Track_Data (10..11)) / 60.0;
         This.Position.Lat := This.Position.Lat + Long_Float'Value (Track_Data (12..14)) / 60000.0;
         if Track_Data (15) = 'S' then
            This.Position.Lat := -This.Position.Lat;
         end if;

         -- DDDMMmmm
         This.Position.Lon := Long_Float'Value (Track_Data (16..18));
         This.Position.Lon := This.Position.Lon + Long_Float'Value (Track_Data (19..20)) / 60.0;
         This.Position.Lon := This.Position.Lon + Long_Float'Value (Track_Data (21..23)) / 600000.0;
         if Track_Data (24) = 'W' then
            This.Position.Lon := -This.Position.Lon;
         end if;

         This.Altitude := Float'Value (Track_Data (31..35));
         This.Speed    := Float'Value (Track_Data (36..38));
         This.Course   := Float'Value (Track_Data (39..41));

      elsif
        Track_Data'First  =  1 and then
        Track_Data'Length > 17 and then
        Track_Data (1) = 'H'
      then
         This.Position.Lat := Long_Float'Value (Track_Data (02..03));
         This.Position.Lat := This.Position.Lat + Long_Float'Value (Track_Data (04..05)) / 60.0;
         This.Position.Lat := This.Position.Lat + Long_Float'Value (Track_Data (06..08)) / 60000.0;
         if Track_Data (09) = 'S' then
            This.Position.Lat := -This.Position.Lat;
         end if;

         This.Position.Lon := Long_Float'Value (Track_Data (10..12));
         This.Position.Lon := This.Position.Lon + Long_Float'Value (Track_Data (13..14)) / 60.0;
         This.Position.Lon := This.Position.Lon + Long_Float'Value (Track_Data (15..17)) / 60000.0;
         if Track_Data (18) = 'W' then
            This.Position.Lon := -This.Position.Lon;
         end if;

         This.Altitude := 0.0;
         This.Speed    := 0.0;
         This.Course   := 0.0;

      else
         This.Position := No_Position_Record;
         This.Altitude := 0.0;
         This.Speed    := 0.0;
         This.Course   := 0.0;
         Utility.Log.Log_Trace ("bad request for traffic <" & Track_Data & ">");

      end if;

   exception
      when others =>
         This.Position := No_Position_Record;
         This.Altitude := 0.0;
         This.Speed    := 0.0;
         This.Course   := 0.0;
         Utility.Log.Log_Trace ("bad request for traffic <" & Track_Data & ">");

   end Load_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Valid_Position (This : User_Record) return Boolean is
   begin

      return This.Position /= No_Position_Record;

   end Valid_Position;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected body Users_Stack is

      --========================================================================
      -- Adds the user to the list and removes old users
      --========================================================================
      procedure Process_User (User : User_Record; Track_Data : String) is
      begin

         -- Log the user and store the data
         ------------------------------------------------
         if Registered (User.Id) then

            Log_Track_Data (User.Id, Track_Data);

         end if;

         -- Update as existing user
         ------------------------------------------------
         for Other_User of Users loop

            if Other_User.Id = User.Id then
               Other_User := User;
               Other_User.Count := Other_User.Count + 1;
               return;
            end if;

         end loop;

         -- Remove old users (inactive for 10 minutes)
         ------------------------------------------------
         for Other_User of Users loop

            if Other_User.Id /= No_Id and then Clock - User.Timestamp > 600.0 then

               Log_Trace ("user " & Other_User.Id & " lost contact at " & Ada.Calendar.Formatting.Image (Other_User.Timestamp) & " C=" & Natural'Image (Other_User.Count));

               Other_User.Id    := No_Id;
               Other_User.Count := 0;

            end if;

         end loop;

         -- Add on a vacant place
         ------------------------------------------------
         for Other_User of Users loop

            if Other_User.Id = No_Id then

               Other_User := User;
               Other_User.Count := 0;

               Log_Trace ("user " & Other_User.Id & " logged");

               return;

            end if;

         end loop;

         Log_Trace ("user " & User.Id & " not registered, stack full");

      end Process_User;
      --------------------------------------------------------------------------




      --========================================================================
      -- Adds the ID to the list of registered users
      --========================================================================
      procedure Add_To_Register (Id : Id_Type) is
      begin

         for R of Register loop

            if R = No_Id or R = Id then

               R := Id;

               return;

            end if;

         end loop;

      end Add_To_Register;
      --------------------------------------------------------------------------




      --========================================================================
      -- Indicates if the user is a valid user
      --========================================================================
      function Registered (Id : Id_Type) return Boolean is
      begin

         if Id = No_Id or else Id = Zz_Id then

            return False;

         end if;

         for R of Register loop

            if R = Id then

               return True;

            elsif R = No_Id then

               return False;

            end if;

         end loop;

         return False;

      end Registered;
      --------------------------------------------------------------------------

   end Users_Stack;
   -----------------------------------------------------------------------------

end Users;
--------------------------------------------------------------------------------
