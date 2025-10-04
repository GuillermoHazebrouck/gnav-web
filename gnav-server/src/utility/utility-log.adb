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
with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Directories;

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package body Utility.Log is

   Trace_Date : String (1..10) := (others => ' ');
   Trace_File : File_Type;

   --===========================================================================
   --
   --===========================================================================
   procedure Log_Trace (Message : String) is

      Now : String := Ada.Calendar.Formatting.Image (Clock);

   begin

      if not Log_On_Files then
         Ada.Text_IO.Put_Line (Message);
         return;
      end if;

      if Trace_Date /= Now (1..10) then

         if Is_Open (Trace_File) then
            Close (Trace_File);
         end if;

         Trace_Date := Now (1..10);

         declare
            Path  : String := "traces/";
            Name  : String := Path & "/" & Trace_Date & ".dat";
         begin

            if not Ada.Directories.Exists (Name) then

               Create (Trace_File, Out_File, Name);

            else
               Open (Trace_File, Append_File, Name);

            end if;

         end;

      end if;

      if Is_Open (Trace_File) then

         Put_Line (Trace_File, Now (12..19) & ": " & Message);
         Flush    (Trace_File);

      end if;

   end Log_Trace;
   -----------------------------------------------------------------------------



   Error_Date : String (1..10) := (others => ' ');
   Error_File : File_Type;

   --===========================================================================
   --
   --===========================================================================
   procedure Log_Error (Message : String) is

      Now : String := Ada.Calendar.Formatting.Image (Clock);

   begin

      if not Log_On_Files then
         Ada.Text_IO.Put_Line ("error: " & Message);
         return;
      end if;

      if Error_Date /= Now (1..10) then

         if Is_Open (Error_File) then
            Close (Error_File);
         end if;

         Error_Date := Now (1..10);

         declare
            Path  : String := "errors/";
            Name  : String := Path & "/" & Error_Date & ".dat";
         begin

            if not Ada.Directories.Exists (Name) then

               Create (Error_File, Out_File, Name);

            else
               Open (Error_File, Append_File, Name);

            end if;

         end;

      end if;

      if Is_Open (Error_File) then

         Put_Line (Trace_File, Now (12..19) & ": " & Message);
         Flush    (Error_File);

      end if;

   end Log_Error;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Log_Error (Message : String; E : Exception_Occurrence) is

      Now : String := Ada.Calendar.Formatting.Image (Clock);

   begin

      if not Log_On_Files then
         Ada.Text_IO.Put_Line ("error: " & Message);
         return;
      end if;

      if Error_Date /= Now (1..10) then

         if Is_Open (Error_File) then
            Close (Error_File);
         end if;

         Error_Date := Now (1..10);

         declare
            Path  : String := "errors/";
            Name  : String := Path & "/" & Error_Date & ".dat";
         begin

            if not Ada.Directories.Exists (Name) then

               Create (Error_File, Out_File, Name);

            else
               Open (Error_File, Append_File, Name);

            end if;

         end;

      end if;

      if Is_Open (Error_File) then

         Put_Line (Error_File, Now (12..19) & ": " & Message);
         Put_Line (Error_File, "E: " & Ada.Exceptions.Exception_Message (E));
         Flush    (Error_File);

      end if;

   end Log_Error;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- TODO: open file only once
   --===========================================================================
   procedure Log_Track_Data (Id : String; Track_Data : String) is
   begin

      -- Log the track data (this is optional for the client)
      --------------------------------------------------------------------------
      if
        Track_Data'Length > 0 and then
        Track_Data (Track_Data'First) = 'B' and then
        Track_Data (Track_Data'First+1..Track_Data'First+6) /= "000000"
      then

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
               Put_Line (File_Id, "HFGIDGLIDERID:" & Id);
               Put_Line (File_Id, "HFPLTPILOTINCHARGE:");
               Put_Line (File_Id, "I023638GSP3941HDT"); -- TODO: change HDT

            else
               Open (File_Id, Append_File, Log_Name);

            end if;

            -- Write the data in IGC format
            -- TODO: do not load the data if it is older than the last
            --       packet.
            ------------------------------------------------------------
            Put_Line (File_Id, Track_Data);

            Close (File_Id);

         end;

      end if;

   end Log_Track_Data;
   -----------------------------------------------------------------------------

end Utility.Log;
--------------------------------------------------------------------------------
