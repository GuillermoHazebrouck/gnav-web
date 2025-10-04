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
with Ada.Characters.Handling;
-- AdaWebServer
with Aws.Headers;
-- Gnav
with Utility;
with Utility.Ids;
with Utility.Log;
use  Utility.Log;
with Traffic;

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package body Gnav_Admin_Callbacks is

   use Ada;

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

         Answer := Response.File (Content_Type => "image/x-icon",
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

      elsif Content = "/gnav_logo.png" then

         Answer := Response.File (Content_Type => "image/png",
                                  Filename     => "files/gnav_logo.png");

      --------------------------------------------------------------------------
      -- Screenshot
      --------------------------------------------------------------------------
      elsif Content = "/screenshot.jpg" then

         Answer := Response.File (Content_Type => "image/jpg",
                                  Filename     => "files/screenshot.jpg");

      --------------------------------------------------------------------------
      -- Search request
      --------------------------------------------------------------------------
      elsif Content = "/recording.igc" then

         declare
            use Ada.Directories;
            Squawk : String := Aws.Status.Parameter (Request, "squawk");
            Server : String := Aws.Status.Parameter (Request, "server");
            Name   : String := Aws.Status.Parameter (Request, "name");
            Path   : String := "../" & Server & "/files/users/" & Squawk & "/" & Name;
         begin

            if Exists (Path) then

               Answer := Response.File (Content_Type => "text/igc",
                                        Filename     => Path);

            end if;

         end;

      --------------------------------------------------------------------------
      -- Search request
      -- The Squawk can be:
      -- > 8 character user ID
      -- > 6 character local device name (a local registration like OO-YZP)
      --------------------------------------------------------------------------
      elsif Content = "/search.html" then

         declare
            use Ada.Directories;
            use Ada.Characters.Handling;
            Squawk : String := To_Upper (Aws.Status.Parameter (Request, "squawk"));
            Server : String := To_Upper (Aws.Status.Parameter (Request, "server"));
            Path   : String := "../" & Server & "/files/users/" & Squawk;
         begin

            Log_Trace ("recording request for " & Squawk & " at " & Server);

            if (Squawk'Length = 6 or Squawk'Length = 8) and Server'Length = 4 then

               -- Send the list of files using hyperlinks
               -----------------------------------------------------------------
               if Exists (Path) then

                  declare
                     use Ada.Text_Io;
                     Item    : Directory_Entry_Type;
                     Search  : Search_Type;
                     File_Id : File_Type;
                     Empty   : Boolean := True;
                  begin

                     Create   (File_Id, Out_File, Path & "/resume.html");

                     Put_Line (File_Id, "<!DOCTYPE html>");
                     Put_Line (File_Id, "<html>");
                     Put_Line (File_Id, "<head>");
                     Put_Line (File_Id, "</head>");

                     Put_Line (File_Id, "<body>");
                     Put_Line (File_Id, "<p>Recorded data for " & Squawk & " at server " & Server & ":</p>");

                     Start_Search (Search    => Search,
                                   Directory => Path,
                                   Pattern   => "*.igc");

                     while More_Entries (Search) loop

                        Get_Next_Entry (Search, Item);

                        if Kind (Item) = Ordinary_File then

                           declare
                              Name : String := Simple_Name (Item);
                              Link : String := "/recording.igc"
                                & "?server=" & Server
                                & "&squawk=" & Squawk
                                & "&name="   & name;
                           begin

                              Put_Line (File_Id, "<p><a href=""" & Link & """>" & Name & "</a></p>");

                           end;

                           Empty := False;

                        end if;

                        exit when not More_Entries (Search);

                     end loop;

                     if Empty then

                        Put_Line (File_Id, "<p>There are still no entries for code " & Squawk & " in server " & Server & ". <a href=""/search.html"">Go back to search</a>.</p>");

                     end if;

                     Put_Line (File_Id, "</body>");
                     Put_Line (File_Id, "</html>");

                     Close (File_Id);

                  exception
                     when E : others =>
                        Close (File_Id);
                        Utility.Log.Log_Error ("while building recording request", E);
                  end;

                  Answer := Response.File (Content_Type => "text/html",
                                           Filename     => Path & "/resume.html");

               else

                  Answer := Response.File (Content_Type => "text/html",
                                           Filename     => "files/search.html");

               end if;

            else

               -- Send the submission request
               -----------------------------------------------------------------
               Answer := Response.File (Content_Type => "text/html",
                                        Filename     => "files/search.html");

            end if;

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

end Gnav_Admin_Callbacks;
--------------------------------------------------------------------------------
