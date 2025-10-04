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
with Users;
use  Users;

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package body Gnav_Server_Callbacks is

   use Ada;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Server stats
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected body Server_Stats is

      function Get_Number_Of_Wasm_Requests return Natural is
      begin
         return Number_Of_Wasm_Requests;
      end;

      function Get_Number_Of_Aprs_Requests return Natural is
      begin
         return Number_Of_Aprs_Requests;
      end;

      procedure Reset is
      begin
         Number_Of_Wasm_Requests := 0;
         Number_Of_Aprs_Requests := 0;
      end;

      procedure Increase_Number_Of_Wasm_Requests is
      begin
         Number_Of_Wasm_Requests := Number_Of_Wasm_Requests + 1;
      end;

      procedure Increase_Number_Of_Aprs_Requests is
      begin
         Number_Of_Aprs_Requests := Number_Of_Aprs_Requests + 1;
      end;

   end Server_Stats;
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

         Server_Stats.Increase_Number_Of_Wasm_Requests;

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
      -- Sounds
      --------------------------------------------------------------------------
      elsif Content = "/bweep.wav" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/sounds/bweep.wav");

      elsif Content = "/alert.wav" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/sounds/alert.wav");

      elsif Content = "/atent.wav" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/sounds/atent.wav");

      elsif Content = "/score.wav" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/sounds/score.wav");

      elsif Content = "/messg.wav" then

         Answer := Response.File (Content_Type => "application/octet-stream",
                                  Filename     => "files/sounds/messg.wav");

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
            User       : User_Record;
            Track_Data : String := Aws.Headers.Get_Values (Aws.Status.Header (Request), "APRS");
         begin

            Utility.Override (User.Id,   Aws.Headers.Get_Values (Aws.Status.Header (Request), "CODE"));

            Utility.Override (User.Mark, Aws.Headers.Get_Values (Aws.Status.Header (Request), "MARK"));

            User.Load_Data (Track_Data);

            if User.Valid_Position then

               -- "Zulu" requests are treated as new users, we need a new Id.
               --------------------------------------------------------------------
               if User.Id = Zz_Id then

                  User.Id := Get_New_Id;

                  Answer := Response.Build (Content_Type => "application/octet-stream",
                                            Message_Body => Traffic.Get_Stack.Get_Tracks (User, Send_Id => True));

                  Server_Stats.Increase_Number_Of_Aprs_Requests;

               -- When there is a valid Id, it means the request is legitimate
               --------------------------------------------------------------------
               elsif User.Id /= No_Id then

                  -- Respond without the Id as it is already known
                  --------------------------------------------------------------------
                  Answer := Response.Build (Content_Type => "application/octet-stream",
                                            Message_Body => Traffic.Get_Stack.Get_Tracks (User, Send_Id => False));

                  -- Incoporate the user to the list of active users and tracks
                  --------------------------------------------------------------------
                  Users_Stack.Process_User (User, Track_Data);

                  Traffic.Get_Stack.Link_User (User);

                  Server_Stats.Increase_Number_Of_Aprs_Requests;

               end if;

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
