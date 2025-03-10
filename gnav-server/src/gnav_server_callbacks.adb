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
with Ada.Exceptions;
with Ada.Text_IO;
-- AdaWebServer
with Aws.Headers;
-- Gnav
with Utility;
with Utility.Ids;
with Utility.Log;
use  Utility.Log;
with Meteo;
with Traffic;

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package body Gnav_Server_Callbacks is

   use Ada;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type User_Record is limited record

      Id    : Utility.Ids.Id_Type;

      Time  : Ada.Calendar.Time;

      Count : Natural;

   end record;

   Users : array (1..50) of User_Record := (others =>  (Id    => Utility.Ids.No_Id,
                                                        Time  => Ada.Calendar.Clock - 4000.0,
                                                        Count => 0));

   --===========================================================================
   -- TODO: open file only once
   --===========================================================================
   procedure Log_User_Data (Id : Utility.Ids.Id_Type; Track_Data : String) is
   begin

      -- Log the track data (this is optional for the client)
      --------------------------------------------------------------------------
      if Track_Data'Length > 0 and then Track_Data (Track_Data'First) = 'B' then

         declare
            use Ada.Text_IO;
            use Ada.Calendar;
            Today    : String := Ada.Calendar.Formatting.Image (Clock);
            Hour     : String := Today (12..Today'Last);
            Log_Path : String := "files/users/" & Id;
            Log_Name : String := Log_Path & "/" & Today (1..10) & ".igc";
            File_Id  : File_Type;
         begin

            if not Ada.Directories.Exists (Log_Path) then
               Ada.Directories.Create_Directory (Log_Path);
            end if;

            if not Ada.Directories.Exists (Log_Name) then

               Create (File_Id, Out_File, Log_Name);

               -- Write the IGC header
               --------------------------------------------------------
               Put_Line (File_Id, "AGNVV2A");
               Put_Line (File_Id, "HFDTE" & Today (3..4) & Today (6..7) & Today (9..10)); -- YYMMDD
               Put_Line (File_Id, "HFDTM100GPSDATUM:WGS-1984");
               Put_Line (File_Id, "HFFTYFRTYPE:G-NAV");
               Put_Line (File_Id, "I023638GSP3941HDT"); -- TODO: change HDT

            else
               Open (File_Id, Append_File, Log_Name);

            end if;

            -- Write the data in IGC format
            ------------------------------------------------------------
            Put_Line (File_Id, Track_Data);

            Close (File_Id);

         end;

      end if;

   end Log_User_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Adds the user to the list and removes old users
   --===========================================================================
   procedure Log_User (Id : Utility.Ids.Id_Type; Track_Data : String) is
   begin

      Log_User_Data (Id, Track_Data);

      -- Update as existing user
      ------------------------------------------------
      for User of Users loop

         if User.Id = Id then
            User.Time  := Clock;
            User.Count := User.Count + 1;
            return;
         end if;

      end loop;

      -- Remove old users (inactive for 10 minutes)
      ------------------------------------------------
      for User of Users loop

         if User.Id /= Utility.Ids.No_Id and then Clock - User.Time > 600.0 then

            Log_Trace ("user " & User.Id & " lost contact at " & Ada.Calendar.Formatting.Image (User.Time) & " C=" & Natural'Image (User.Count));

            User.Id    := Utility.Ids.No_Id;
            User.Count := 0;

         end if;

      end loop;

      -- Add on a vacant place
      ------------------------------------------------
      for User of Users loop

         if User.Id = Utility.Ids.No_Id then

            User.Id    := Id;
            User.Time  := Clock;
            User.Count := 0;

            Log_Trace ("user " & User.Id & " logged");

            return;

         end if;

      end loop;

      Log_Trace ("user " & Id & " not registered, stack full");

   end Log_User;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Handle_Request (Request : Status.Data) return Response.Data is

      Answer  : Response.Data;
      Content : String := Aws.Status.URI (Request);

   begin

      --------------------------------------------------------------------------
      -- XML file
      --------------------------------------------------------------------------
      if Content = "/" or Content = "/index.html" then

         Answer := Response.File (Content_Type => "text/html",
                                  Filename     => "files/index.html");

         Log_Trace ("resources requested");

      --------------------------------------------------------------------------
      -- Icons
      --------------------------------------------------------------------------
      elsif Content = "/favicon.ico" then

         Answer := Response.File (Content_Type => "image/vnd.microsoft.icon",
                                  Filename     => "files/icons/favicon.ico");

      elsif Content = "/icon_16x16.png" then

         Answer := Response.File (Content_Type => "image/png",
                                  Filename     => "files/icons/icon_16x16.png");

      elsif Content = "/icon_32x32.png" then

         Answer := Response.File (Content_Type => "image/png",
                                  Filename     => "files/icons/icon_32x32.png");

      elsif Content = "/icon_192x192.png" then

         Answer := Response.File (Content_Type => "image/png",
                                  Filename     => "files/icons/icon_192x192.png");

      elsif Content = "/icon_512x512.png" then

         Answer := Response.File (Content_Type => "image/png",
                                  Filename     => "files/icons/icon_512x512.png");

      --------------------------------------------------------------------------
      -- Main module
      --------------------------------------------------------------------------
      elsif Content = "/main.wasm" then

         Answer := Response.File (Content_Type => "application/wasm",
                                  Filename     => "files/main.wasm");

      --------------------------------------------------------------------------
      -- Ada webpack tools
      --------------------------------------------------------------------------
      elsif Content = "/adawebpack.mjs" then

         Answer := Response.File (Content_Type => "text/javascript",
                                  Filename     => "files/adawebpack.mjs");

      --------------------------------------------------------------------------
      -- Manifest
      --------------------------------------------------------------------------
      elsif Content = "/manifest.json" then

         Answer := Response.File (Content_Type => "text/javascript",
                                  Filename     => "files/manifest.json");

      --------------------------------------------------------------------------
      -- Service worker
      --------------------------------------------------------------------------
      elsif Content = "/gnav_sw.js" then

         Answer := Response.File (Content_Type => "text/javascript",
                                  Filename     => "files/gnav_sw.js");

      --------------------------------------------------------------------------
      -- Maps
      --------------------------------------------------------------------------
      elsif Content = "/terrain_1.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/terrain_1.bin");

      elsif Content = "/terrain_2.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/terrain_2.bin");

      elsif Content = "/terrain_3.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/terrain_3.bin");

      elsif Content = "/terrain_4.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/terrain_4.bin");

      elsif Content = "/terrain_5.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/terrain_5.bin");

      elsif Content = "/airspaces.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/airspaces.bin");

      elsif Content = "/layers.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/layers.bin");

      elsif Content = "/reference.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/reference.bin");

      --------------------------------------------------------------------------
      -- List of useful radio frequencies
      --------------------------------------------------------------------------

      elsif Content = "/radio.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/radio.bin");

      --------------------------------------------------------------------------
      -- Aircraft list
      --------------------------------------------------------------------------

      elsif Content = "/aircraft.bin" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/data/aircraft.bin");

      --------------------------------------------------------------------------
      -- METAR messages
      --------------------------------------------------------------------------

      elsif Content = "/metar.bin" then

         Answer := Response.Build (Content_Type => "application/octet-stream",
                                   Message_Body => Meteo.Get_Stack.Get_Stations);

      --------------------------------------------------------------------------
      -- Traffic (G-NAV APRS)
      --------------------------------------------------------------------------
      elsif Content = "/traffic.bin" then

         declare
            use Utility.Ids;
            User_Id    : Id_Type := No_Id;
            Track_Data : String  := Aws.Headers.Get_Values (Aws.Status.Header (Request), "APRS");
         begin

            Utility.Override (User_Id, Aws.Headers.Get_Values (Aws.Status.Header (Request), "SQUAWK"));

            -- "Zulu" requests are treated as new users, we need a new Id.
            --------------------------------------------------------------------
            if User_Id = Zz_Id then

               User_Id := Get_New_Id;

               Answer := Response.Build (Content_Type => "application/octet-stream",
                                         Message_Body => Traffic.Get_Stack.Get_Tracks (User_Id, Track_Data, Send_Id => True));

            -- When there is a valid Id, it means the request is legitimate
            --------------------------------------------------------------------
            elsif User_Id /= No_Id then

               -- Respond without the Id as it is already known
               -----------------------------------------------------------------
               Answer := Response.Build (Content_Type => "application/octet-stream",
                                         Message_Body => Traffic.Get_Stack.Get_Tracks (User_Id, Track_Data, Send_Id => False));

               Log_User (User_Id, Track_Data);

            end if;

         exception
            when E : others =>
               Log_Error ("error while processing traffic", E);
         end;

      --------------------------------------------------------------------------
      -- Unknow requests
      --------------------------------------------------------------------------
      else
         Log_Trace ("warning: invalid request for '" & Content & "'");

      end if;

      return Answer;

   end Handle_Request;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Set_Password (File : String) return String is
   begin
      Log_Trace ("asking for password for " & File);
      return "foobar";
   end Set_Password;
   -----------------------------------------------------------------------------

end Gnav_Server_Callbacks;
--------------------------------------------------------------------------------
