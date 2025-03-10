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

end Utility.Log;
--------------------------------------------------------------------------------
