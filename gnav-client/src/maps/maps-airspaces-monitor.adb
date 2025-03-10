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
with Ada.Streams;
use  Ada.Streams;
with Interfaces;
-- Gnav
with Glex.Fonts;
use  Glex.Fonts;
with Utility.Atmosphere;
use  Utility.Atmosphere;
with Utility.Log;
with Utility.Notifications;
use  Utility.Notifications;
with Utility.Calendar;
use  Utility.Calendar;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Airspaces.Monitor is

   Font_1 : Font_Style_Record := (Width     => 0.008,
                                  Height    => 0.026,
                                  Space     => 0.006,
                                  Rendering => Glex.Fonts.Font_Glow,
                                  Thickness => Glex.Fonts.Font_Bold);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Notification_String is String (1..30);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Notification_Record is record

      Clock  : Times;
      Text   : Notification_String;
      Index  : Natural;
      Active : Boolean;

   end record;

   type Notification_Range is range 1..3;
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Notifications : array (Notification_Range) of Notification_Record :=
     (others => (Text   => (others => ' '),
                 Index  => 0,
                 Active => False,
                 Clock  => No_Time));

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   N : Notification_Range := 1;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Location : Position_Record := No_Position_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Altitude : Float := 0.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Notification_Duration : constant Lapses := Lapse_Of (30.0);

   --===========================================================================
   --
   --===========================================================================
   procedure Process_Location (Location : Position_Record; Altitude : Float) is
   begin

      if Allow_Notifications then

         if Last_Location = No_Position_Record then

            Last_Location := Location;
            Last_Altitude := Altitude;

            -- Synchronize location
            ----------------------------------------
            for I in 1 .. Number_Of_Airspaces loop

               if
                 Airspaces (I).Active and then
                 Airspaces (I).Notify /= Notify_None
               then

                  if
                    Airspaces (I).Lower_Limit < Altitude and then
                    Airspaces (I).Upper_Limit > Altitude
                  then
                     Airspaces (I).Inside := Airspaces (I).Contains (Location);
                  else
                     Airspaces (I).Inside := False;
                  end if;

               end if;

            end loop;

            return;

         end if;

         for I in Notification_Range loop

            if
              Notifications (I).Active and then
              Cached_Time - Notifications (I).Clock > Notification_Duration
            then
               Notifications (I).Active := False;
            end if;

         end loop;

         if
           Location /= No_Position_Record             and then
           (Distance (Last_Location, Location) > 0.250 or else
            abs (Altitude - Last_Altitude)     > 15.0)
         then

            Last_Location := Location;
            Last_Altitude := Altitude;

            declare
               Inside,
               When_In,
               When_Out : Boolean := False;
            begin

               for I in 1 .. Number_Of_Airspaces loop

                  if
                    Airspaces (I).Active and then
                    Airspaces (I).Notify /= Notify_None
                  then

                     if
                       Airspaces (I).Lower_Limit < Altitude and then
                       Airspaces (I).Upper_Limit > Altitude
                     then
                        Inside := Airspaces (I).Contains (Location);
                     else
                        Inside := False;
                     end if;

                     When_In  := Airspaces (I).Notify = Notify_In  or Airspaces (I).Notify = Notify_In_Out;
                     When_Out := Airspaces (I).Notify = Notify_Out or Airspaces (I).Notify = Notify_In_Out;

                     if
                       When_In and then
                       not Airspaces (I).Inside and then Inside
                     then

                        if N = Notification_Range'Last then
                           N := 1;
                        else
                           N := N + 1;
                        end if;

                        Notifications (N).Clock := Cached_Time;
                        Utility.Strings.Override (Notifications (N).Text, "ENTERED " & Airspaces (I).Name);
                        Notifications (N).Active := True;

                        Add_Notification (Notify_Sector);

                        Utility.Log.Put_Message (Notifications (N).Text);

                     elsif
                       When_Out and then
                       Airspaces (I).Inside and then not Inside
                     then

                        if N = Notification_Range'Last then
                           N := 1;
                        else
                           N := N + 1;
                        end if;

                        Notifications (N).Clock := Cached_Time;
                        Utility.Strings.Override (Notifications (N).Text, "LEFT " & Airspaces (I).Name);
                        Notifications (N).Active := True;

                        Add_Notification (Notify_Sector);

                        Utility.Log.Put_Message (Notifications (N).Text);

                     end if;

                     Airspaces (I).Inside := Inside;

                  end if;

               end loop;

            end;

         end if;

      end if;

   end Process_Location;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Draws the active notifications
   --===========================================================================
   procedure Draw_Notifications is

      Y : Float := 0.98;

   begin

      if Allow_Notifications then

         for I in Notification_Range loop

            if Notifications (I).Active then

               Glex.Fonts.Draw (Trim (Notifications (I).Text),
                                X         => 0.5,
                                Y         => Y,
                                Style     => Font_1,
                                Color     => Line_Red,
                                Alignment => Alignment_TC);

               Y := Y - 0.046;

            end if;

         end loop;

      end if;

   end Draw_Notifications;
   -----------------------------------------------------------------------------



end Maps.Airspaces.Monitor;
--------------------------------------------------------------------------------
