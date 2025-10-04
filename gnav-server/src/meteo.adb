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
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
use  Ada.Calendar.Time_Zones;
with Ada.Calendar.Conversions;
use  Ada.Calendar.Conversions;
with Ada.Directories;
with Ada.Text_IO;
-- Aws
with Aws.Client;
with Aws.Response;
-- Gnav
with Utility;
with Utility.Log;
use  Utility.Log;
with Utility.Streams;
use  Utility.Streams;
with Utility.Types;
use  Utility.Types;

--//////////////////////////////////////////////////////////////////////////////
-- This special program is able to parse a collection of METAR messages to
-- generate a meteo stations message.
--//////////////////////////////////////////////////////////////////////////////
package body Meteo is

   --===========================================================================
   -- Returns the track stack
   --===========================================================================
   function Get_Stack return not null access Meteo_Station_Stack is
   begin

      return Stack'Access;

   end Get_Stack;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected body Meteo_Station_Stack is

      --========================================================================
      --
      --========================================================================
      procedure Setup_Stations is

         use Ada.Text_IO;
         use Utility;

         File_Id   : File_Type;
         File_Name : constant String := "gnav.ini";
         Argument  : String_Buffer (500);
         S         : Station_Range := Station_Range'First;

      begin

         -- Load configuration
         -----------------------------------------------------------------------
         if Ada.Directories.Exists (File_Name) then

            Log_Trace ("reading meteo stations...");

            Open (File_Id, In_File, File_Name);

            while not End_Of_File (File_Id) loop

               Argument.Load (Get_Line (File_Id));

               declare
                  Key   : String := Argument.Read_Next ('=');
                  Arg_1 : String := Argument.Read_Next ('|');
                  Arg_2 : String := Argument.Read_Next ('|');
                  Arg_3 : String := Argument.Read_Next ('|');
                  Arg_4 : String := Argument.Read_Next ('|');
                  Arg_5 : String := Argument.Read_Next (' ');
               begin

                  if Key = "METEO_STATION" then

                     Stations (S).Loaded := True;

                     Utility.Override (Stations (S).Name, Arg_1);

                     Stations (S).Position  := Utility.Maps.Value  (Arg_2);
                     Stations (S).Elevation := Natural'Value       (Arg_3);
                     Stations (S).Interval  := Duration'Value      (Arg_4) * 60.0;
                     Stations (S).Url       := To_Unbounded_String (Arg_5);

                     if S = Station_Range'Last then
                        return;
                     else
                        S := S + 1;
                     end if;

                  end if;

               end;

            end loop;

            Close (File_Id);

         end if;

      end Setup_Stations;
      --------------------------------------------------------------------------




      --========================================================================
      -- (See specification file)
      --========================================================================
      procedure Update_Stations is

         Now     : Time    := Clock;
         Updated : Boolean := False;

      begin

         for Station of Stations loop

            if Station.Loaded and then Station.Next_Update < Now then

               declare
                  Data    : Aws.Response.Data := Aws.Client.Get (URL => To_String (Station.Url));
                  Message : String            := Aws.Response.Message_Body (Data);
               begin

                  if Message'Length > 4 then

                     Station.Parse_Message (Message);
                     Updated := True;

                  end if;

               exception
                  when E : others =>
                     Utility.Log.Log_Error ("while fetching metar message", E);
               end;

            end if;

         end loop;

         if Updated then

            declare
               Holder : aliased Stream_Buffer_Type;
               Source : not null access  Stream_Buffer_Type := Holder'Access;
               Time_Stamp   : Long_Float;
               Metar_Time   : Long_Float;
               Metar_Offset : Float;
               Next_Update  : Float := 3600.0;
               Count        : Short_Short_Natural := 0;
            begin
               -- Reset buffer
               -----------------------------------------------------------------
               Source.Buffer := Buffer'Unrestricted_Access;
               Source.Cursor := Buffer'First;

               -- Write the active stations
               -----------------------------------------------------------------
               Time_Stamp := Long_Float (To_Unix_Time (Now));

               Utility.Log.Log_Trace ("timestamp =" & Long_Float'Image (Time_Stamp));

               Long_Float'Write (Source, Time_Stamp);

               for Station of Stations loop

                  if Station.Active then

                     Count := Count + 1;

                     Next_Update := Float'Min (Next_Update,
                                               Float'Max (0.0, Float (Station.Next_Update - Now)));

                  end if;

               end loop;

               Utility.Log.Log_Trace ("next update is in " & Utility.Float_Image (Next_Update / 60.0, 0) & " min");

               Float'Write (Source, Next_Update);

               Short_Short_Natural'Write (Source, Count);

               for Station of Stations loop

                  if Station.Active then

                     Metar_Name'Write (Source, Station.Name);                                       -- 4 bytes

                     Metar_Time := Long_Float (To_Unix_Time (Station.Date));

                     Metar_Offset := Float (Metar_Time - Time_Stamp);

                     Utility.Log.Log_Trace (Station.Name & " time offset " & Utility.Float_Image (Metar_Offset / 60.0, 0) & " min");

                     Float'Write (Source, Metar_Offset);                                            -- 4 bytes

                     Long_Float'Write (Source, Station.Position.Lat);                               -- 8 bytes

                     Long_Float'Write (Source, Station.Position.Lon);                               -- 8 bytes

                     Short_Short_Integer'Write (Source, Short_Short_Integer (Station.Qnh - 1013));  -- 1 byte

                     Utility.Log.Log_Trace (Station.Name & " QNH " & Short_Short_Integer'Image (Short_Short_Integer (Station.Qnh - 1013)));

                     Short_Short_Natural'Write (Source, Short_Short_Natural (Station.Wind_Speed));  -- 1 byte

                     Short_Natural'Write (Source, Short_Natural (Station.Wind_Course));             -- 2 bytes

                     Short_Short_Integer'Write (Source, Short_Short_Integer (Station.Temperature)); -- 1 bytes

                     Short_Short_Integer'Write (Source, Short_Short_Integer (Station.Dew_Point));   -- 1 bytes

                     Short_Natural'Write (Source, Short_Natural (Station.Cloud_Base));              -- 2 bytes

                     Short_Natural'Write (Source, Short_Natural (Station.Visibility));              -- 2 bytes

                  end if;

               end loop;

               Cursor := Source.Cursor;

               Utility.Log.Log_Trace ("meteo message size is" & Stream_Element_Offset'Image (Cursor) & " bytes");

            end;

         end if;

      exception
         when E : others =>
            Cursor := 0;
            Utility.Log.Log_Error ("while updating stations", E);

      end Update_Stations;
      --------------------------------------------------------------------------




      --========================================================================
      -- Returns a stream containing all meteo stations
      --========================================================================
      function Get_Stations return Stream_Element_Array is
      begin

         return Buffer (Buffer'First.. Cursor);

      end Get_Stations;
      --------------------------------------------------------------------------

   end Meteo_Station_Stack;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Parse_Message (This : in out Meteo_Station_Record; Message : String) is

      use Utility;

      Reader : String_Buffer (Message'Length);
      Fog    : Boolean    := False;
      Name   : Metar_Name := (others => ' ');

   begin

      Utility.Log.Log_Trace ("parsing message");

      Reader.Load (Message);

      declare
         Message_Type : String := Reader.Read_Next (' ', Multiple => True);
      begin
         if Message_Type /= "METAR" then
            Utility.Log.Log_Trace ("warning: wrong metar message received (" & Message_Type & ")");
            return;
         end if;
      end;

      Override (Name, Reader.Read_Next (' ', Multiple => True));

      if This.Name /= Name then
         Utility.Log.Log_Trace ("warning: wrong station received (" & Name & ")");
         Utility.Log.Log_Trace (Message);
         return;
      end if;

      This.Active        := False;
      This.Wind_Speed    := 0;
      This.Wind_Course   := 0;

      This.Cloud_Base    := 0; -- (0 => no clouds, 1 => Fog, > 1 => Base)
      This.Precipitation := 0;
      This.Visibility    := 9999; -- (9999 => unlimited)

      loop

         declare
            Data : String  := Reader.Read_Next (' ', Multiple => True);
            F    : Natural := Data'First;
            L    : Natural := Data'Last;
         begin

            exit when Data'Length = 0;

            if
              Data'Length = 7 and then
              Data (L) = 'Z'
            then
               -- Time
               declare
                  Now     : Time  := Clock;
                  Today   : Day_Number := Day (Now);
                  Day     : Day_Number := Day_Number'Value (Data (F  ..F+1));
                  Hour    : Float := 3600.0 * Float'Value  (Data (F+2..F+3));
                  Minutes : Float :=   60.0 * Float'Value  (Data (F+4..F+5));
                  Lapse   : Float := Hour + Minutes;
               begin

                  if Day = Today then

                     if Lapse in 0.0..86_400.0 then

                        This.Date := Ada.Calendar.Time_Of (Year  (Now),
                                                           Month (Now),
                                                           Day,
                                                           Day_Duration (Lapse)) + Duration (60 * UTC_Time_Offset);
                     else
                        Log_Trace ("meteo station " & This.Name & ": incorrect metar time " & Data);

                        return;

                     end if;

                  else
                     -- Skip metars from different days, they can block the updates
                     return;

                  end if;

               end;

            elsif
              Data'Length = 5 and then
              Data (F) = 'Q'  and then
              Is_Numerical (Data (F+1..L))
            then
               -- QNH
               This.Qnh := Natural'Value (Data (F+1..F+4));

            elsif
              Data'Length = 4 and then
              Is_Numerical (Data)
            then
               -- Visibility
               This.Visibility := Natural'Value (Data);

            elsif
              Data'Length > 2 and then
              Data (F..F+1) = "VV" and then
              Is_Numerical (Data (F+2..L))
            then
               -- Vertical visibility
               null;

            elsif
              Data'Length > 4  and then
              Data (F+2) = '/' and then
              Is_Numerical (Data (F..F+1))
            then
               -- Temperature and dew point
               This.Temperature := Natural'Value (Data (F  ..F+1));

               if Data (F+3) = 'M' then
                  This.Dew_Point := -Natural'Value (Data (F+4..F+5));
               else
                  This.Dew_Point :=  Natural'Value (Data (F+3..F+4));
               end if;

            elsif
              Data'Length > 5  and then
              Data (F  ) = 'M' and then
              Data (F+3) = '/' and then
              Is_Numerical (Data (F+1..F+2))
            then
               -- Negative temperature and dew point
               This.Temperature := -Natural'Value (Data (F+1..F+2));

               if Data (F+4) = 'M' then
                  This.Dew_Point := -Natural'Value (Data (F+5..F+6));
               else
                  This.Dew_Point :=  Natural'Value (Data (F+4..F+5));
               end if;

            elsif
              Data'Length = 7  and then
              Data (L-1..L) = "KT"
            then
               -- Wind (convert to km/h)
               if Is_Numerical (Data (F..F+2)) then
                  This.Wind_Course := Natural'Value (Data (F..F+2));
               end if;
               This.Wind_Speed := Natural (Float'Value (Data (F+3..F+4)) * 1.852);

            elsif
              Data'Length = 8 and then
              Data (L-2..L) = "MPS"
            then
               -- Wind (convert to km/h)
               if Is_Numerical (Data (F..F+2)) then
                  This.Wind_Course := Natural'Value (Data (F..F+2));
               end if;
               This.Wind_Speed := Natural (Float'Value (Data (F+3..F+4)) * 3.6);

            elsif
              Data'Length > 5 and then
             (Data (F..F+2) = "FEW" or else
              Data (F..F+2) = "SCT" or else
              Data (F..F+2) = "BKN" or else
              Data (F..F+2) = "OVC")
            then
               -- Cloud layer: take minimum as cloud base (converted from hFT to m)
               declare
                  H : Natural := Natural (Float'Value (Data (F+3..F+5)) * 30.48);
               begin
                  H := H + This.Elevation;
                  if This.Cloud_Base = 0 or else H < This.Cloud_Base then
                     This.Cloud_Base := H;
                  end if;
               end;

            elsif
              Data = "FG" or else Data = "BR"
            then
               Fog := True;

            elsif
              Data = "SH" or else
              Data = "RA" or else
              Data = "DZ"
            then
               This.Precipitation := 1; -- rain, showers or drizzle

            elsif
              Data = "TS"
            then
               This.Precipitation := 2; -- tunder storm

            elsif
              Data = "SN"
            then
               This.Precipitation := 3; -- snow

            end if;

         end;

      end loop;

      if Fog then
         This.Cloud_Base := 1;
      end if;

      This.Next_Update := This.Date + This.Interval;

      This.Active      := True; --> set to false when there is an error

      -- Consistency check, block garbage
      ------------------------------------------
      if This.Temperature not in -20..50 then
         This.Active := False;
      end if;

      if This.Dew_Point not in -20..50 then
         This.Active := False;
      end if;

      if This.Wind_Speed not in 0..150 then
         This.Active := False;
      end if;

      if This.Wind_Course not in 0..360 then
         This.Active := False;
      end if;

      if This.Qnh not in 890..1135 then
         This.Active := False;
      end if;

      if This.Visibility not in 0..9999 then
         This.Active := False;
      end if;

      if This.Cloud_Base not in 0..20000 then
         This.Active := False;
      end if;

      if This.Active then
         Log_Trace ("METEO STATION " & This.Name);
         Log_Trace ("Location    = " & Image (This.Position));
         Log_Trace ("Elevation   = " & Natural'Image (This.Elevation));
         Log_Trace ("Time        = " & Ada.Calendar.Formatting.Image (This.Date,        Time_Zone => UTC_Time_Offset));
         Log_Trace ("Next        = " & Ada.Calendar.Formatting.Image (This.Next_Update, Time_Zone => UTC_Time_Offset));
         Log_Trace ("QNH         = " & Natural'Image (This.Qnh));
         Log_Trace ("Wind speed  = " & Natural'Image (This.Wind_Speed));
         Log_Trace ("Wind course = " & Natural'Image (This.Wind_Course));
         Log_Trace ("Temperature = " & Integer'Image (This.Temperature));
         Log_Trace ("Dew point   = " & Integer'Image (This.Dew_Point));
         Log_Trace ("Visibility  = " & Natural'Image (This.Visibility));
         Log_Trace ("Cloud base  = " & Natural'Image (This.Cloud_Base));
         Log_Trace ("Message     = " & Message (Message'First..Message'Last-1));
      else
         Log_Trace ("meteo station " & This.Name & " inactive");
      end if;

   exception
      when E : others =>
         This.Active := False;
         Utility.Log.Log_Error ("at meteo station " & This.Name & " while parsing metar message '" & Message & "'", E);

   end Parse_Message;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   task body Maintain_Stations is
   begin

      accept Start;

      loop
         Stack.Update_Stations;
         delay 5.0;
      end loop;

   end Maintain_Stations;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Start_Updating is
   begin

      Stack.Setup_Stations;

      Maintain_Stations.Start;

   end Start_Updating;
   -----------------------------------------------------------------------------


end Meteo;
--------------------------------------------------------------------------------
