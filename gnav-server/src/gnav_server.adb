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
with Ada.Directories;
with Ada.Text_IO;
-- AdaWebServer
with Aws.Config;
with Aws.Net.Ssl.Certificate;
with Aws.Server;
-- Local
with Gnav_Server_Callbacks;
with Gnav_Admin_Callbacks;
with Meteo;
with Traffic;
with Traffic.Ogn;
with Utility;
with Utility.Log;
use  Utility.Log;


--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
procedure Gnav_Server is

   use Ada;
   use Aws;
   use Text_Io;
   use Utility;

   File_Id     : File_Type;
   File_Name   : constant String := "gnav.ini";
   Argument    : String_Buffer (500);

   Port_Number : Natural := 4433;
   Web_Server  : Server.Http;
   Ssl_Config  : Net.Ssl.Config;
   Version     : constant String := "1L";
   Admin_Mode  : Boolean := False;

begin

   Log_Trace ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   Log_Trace ("x          G-NAV SERVER             x");
   Log_Trace ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   Log_Trace ("Version: " & Version);

   -- Load configuration
   -----------------------------------------------------------------------------
   if Ada.Directories.Exists (File_Name) then

      Log_Trace ("Reading server configuration...");

      Open (File_Id, In_File, File_Name);

      while not End_Of_File (File_Id) loop

         Argument.Load (Get_Line (File_Id));

         declare
            Key : String := Argument.Read_Next ('=');
            Val : String := Argument.Read_Next ('=');
         begin

            if Key = "PORT" then

               Port_Number := Natural'Value (Val);

            elsif Key = "ADMIN" then

               Admin_Mode := True;

            end if;

         end;

      end loop;

      Close (File_Id);

   end if;

   Log_Trace ("port:" & Natural'Image (Port_Number));

   -- Run G-NAV server
   -----------------------------------------------------------------------------
   Net.SSL.Certificate.Set_Password_Callback (Gnav_Server_Callbacks.Set_Password'Access);

   Net.SSL.Initialize
     (Ssl_Config,
      Certificate_Filename => "gnav-server.crt",
      Key_Filename         => "gnav-server.key",
      Security_Mode        => Net.Ssl.TLSv1_2);

   Server.Set_Ssl_Config (Web_Server,
                          Ssl_Config);

   if Admin_Mode then

      Server.Start (Web_Server,
                    Name             => "G-NAV ADMIN SERVER " & Version,
                    Max_Connection   => 20,
                    Security         => True,
                    Port             => Port_Number,
                    Session          => False,
                    Callback         => Gnav_Admin_Callbacks.Handle_Request'Access);

      Server.Wait (Server.Q_Key_Pressed);
      Server.Shutdown (Web_Server);

   else

      -- Run the server in the background
      --------------------------------------------------------------------------
      Server.Start (Web_Server,
                    Name             => "G-NAV SERVER " & Version,
                    Max_Connection   => 50,
                    Security         => True,
                    Port             => Port_Number,
                    Session          => False,
                    Callback         => Gnav_Server_Callbacks.Handle_Request'Access);

      -- Run the meteo data fetcher in the background
      --------------------------------------------------------------------------
      Meteo.Start_Updating;

      -- Run the tracker data fetcher in the background
      --------------------------------------------------------------------------
      Traffic.Ogn.Start_Updating;

      loop
         delay 5.0;
      end loop;

   end if;

end Gnav_Server;
--------------------------------------------------------------------------------
