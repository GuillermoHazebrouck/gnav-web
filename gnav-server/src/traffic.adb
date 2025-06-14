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
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
-- Gnav
with Utility;
with Utility.Streams;
use  Utility.Streams;
with Utility.Log;
use  Utility.Log;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Traffic is

   --===========================================================================
   -- Configures the tracker based on the initialization file
   --===========================================================================
   procedure Setup_Tracker is
      use Ada.Text_Io;
      use Utility;
      File_Id   : File_Type;
      File_Name : constant String := "gnav.ini";
      Argument  : String_Buffer (500);
      Url_Set   : Boolean := False;
   begin
      
      if Ada.Directories.Exists (File_Name) then
         
         Log_Trace ("reading tracker configuration...");

         Open (File_Id, In_File, File_Name); 
      
         while not End_Of_File (File_Id) loop	
           
            Argument.Load (Get_Line (File_Id));

            declare
               Key : String := Argument.Read_Next ('=');
               Val : String := Argument.Read_Next ('=');
            begin

               if    Key = "LATITUDE" then

                  Reference.Lat := Long_Float'Value (Val);

               elsif Key = "LONGITUDE" then

                  Reference.Lon := Long_Float'Value (Val);

               elsif Key = "TRAFFIC_RANGE" then

                  Traffic_Range := abs Float'Value (Val);

               elsif Key = "OGN_RANGE" then

                  Ogn_Range := abs Long_Float'Value (Val);

               elsif Key = "OGN_URL" then

                  Override (Ogn_Url, Val);

                  Url_Set := True;
                  
               elsif Key = "OGN_PORT" then

                  Ogn_Port := Natural'Value (Val);

               elsif Key = "OGN_GLIDERS_ONLY" then

                  Gliders_Only := True;
                  
                  Log_Trace ("serving gliders only");

               elsif Key = "LOCAL_DEVICE" then

                  if Val'Length > 7 and then Val (Val'First + 6) = ',' then
                     
                     for Device of Local_Devices loop
                     
                        if Device.Id = 0 then
                        
                           Device.Id := Parse_Hex_Id (Val (Val'First .. Val'First + 5));
                           
                           Utility.Override (Device.Name, Val (Val'First + 7 .. Val'Last));
                        
                           Log_Trace ("registered local tracking device " & Val & " Id=" & Natural'Image (Device.Id));

                           exit;
                           
                        end if;
                     
                     end loop;
                     
                  else
                     
                     Log_Trace ("wrong format for local tracking device " & Val);

                  end if;
                  
               elsif Key = "LOG_LATTENCY" then

                  Log_Lattency := Float'Value (Val);
                  
                  if Log_Lattency > 10.0 then
                     Log_Lattency := 10.0;
                  elsif Log_Lattency < 1.0 then
                     Log_Lattency := 1.0;
                  end if;
                  
               end if;

            end;

         end loop;
      
         Close (File_Id);
         
      end if;
      
      if not Url_Set then
         Override (Ogn_Url, "aprs.glidernet.org");
      end if;
         
      declare
         V : Point_Record;
         R : Position_Record;
      begin
         
         V.Set (Ogn_Range, Ogn_Range);         
         R := Utility.Maps.Position (Reference, V);
         
         Limits.Lat := R.Lat - Reference.Lat;
         Limits.Lon := R.Lon - Reference.Lon;
         
      end;
      
   end Setup_Tracker;
   -----------------------------------------------------------------------------
     
   
   
   
   --===========================================================================
   --
   --===========================================================================
   function To_String (This : Track_Record) return String is
   begin
      
      return "track: " &
        Natural'Image (This.Id)                 & "/ T =" &
        Short_Short_Integer'Image (This.Age)    & "/ L =" &
        Float'Image (This.Latitude)             & "/ L =" &
        Float'Image (This.Longitude)            & "/ A =" &
        Short_Natural'Image (This.Altitude)     & "/ V =" &
        Short_Short_Integer'Image (This.Vario)  & "/ S =" &
        Short_Short_Natural'Image (This.Speed)  & "/ C =" &
        Short_Short_Natural'Image (This.Course) & "/ R =" &
        Short_Short_Integer'Image (This.Rotation);
              
   end To_String;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Returns an IGC image of the data
   --===========================================================================
   function Get_Igc_Image (This : Track_Record) return String is
      
      use Utility;

      --===========================================================================
      -- DDDMMmmm<E/W>
      --===========================================================================
      function Compact_Lon_String (Value : Long_Float) return String is

         Degrees : Long_Float := Long_Float'Floor (abs Value);
         Minutes : Long_Float := 60.0 * (abs Value - Degrees);

         D : String := "000";
         M : String := "00.000";

      begin

         Override (D, Integer_Image (Integer (Long_Float'Floor (Degrees))), '0', True);
         Override (M, Float_Image   (Float   (Minutes), 3), '0', True);

         if Value < 0.0 then
            return D & M (1..2) & M (4..6) & 'W';
         else
            return D & M (1..2) & M (4..6) & 'E';
         end if;

      end Compact_Lon_String;
      -----------------------------------------------------------------------------



      --===========================================================================
      -- DDMMmmm<N/S>
      --===========================================================================
      function Compact_Lat_String (Value : Long_Float) return String is

         Degrees : Long_Float := Long_Float'Floor (abs Value);
         Minutes : Long_Float := 60.0 * (abs Value - Degrees);

         D : String := "00";
         M : String := "00.000";

      begin

         Override (D, Integer_Image (Integer (Long_Float'Floor (Degrees))), '0', True);
         Override (M, Float_Image   (Float   (Minutes), 3), '0', True);

         if Value < 0.0 then
            return D & M (1..2) & M (4..6) & 'S';
         else
            return D & M (1..2) & M (4..6) & 'N';
         end if;

      end Compact_Lat_String;
      -----------------------------------------------------------------------------
      
      Timestamp : String := Ada.Calendar.Formatting.Image (This.Timestamp);
      Hours     : String := Timestamp (12..13);
      Minutes   : String := Timestamp (15..16);
      Seconds   : String := Timestamp (18..19);
      
      Position  : String := Compact_Lat_String (This.Position.Lat) &
                            Compact_Lon_String (This.Position.Lon);
      
      Altitude  : String := "00000"; -- [m]
      Course    : String := "000";   -- [deg]
      Speed     : String := "000";   -- [km/h]

   begin

      Override (Altitude, Float_Image (Float (This.Altitude),     0), '0', True);
      Override (Course,   Float_Image (Float (This.Speed)  * 2.0, 0), '0', True);
      Override (Speed,    Float_Image (Float (This.Course) * 1.5, 0), '0', True);

      return 'B' & Hours & Minutes & Seconds & Position & 'A' & Altitude & Altitude & Course & Speed;

   end Get_Igc_Image;
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

   end Log_Track_Data;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Stack return not null access Track_Stack is
   begin
      
      return Stack'Access;
      
   end Get_Stack;
   -----------------------------------------------------------------------------

   
   
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected body Track_Stack is
                               
      --========================================================================
      -- (See type declaration)
      --========================================================================      
      procedure Clean_Tracks is
         Current_Time : Time := Clock;
      begin
         
         -- Remove very old tracks
         -----------------------------------------------------------------------
         for Track of Tracks loop
            if Track.Id /= 0 and then (Current_Time - Track.Timestamp) > 120.0 then 
               Track.Id := 0;
            end if;
         end loop;
       
      end Clean_Tracks;
      --------------------------------------------------------------------------
      
      
      
      
      --========================================================================
      -- (See type declaration)
      --========================================================================      
      function Count_Tracks return Natural is
         Track_Count  : Natural := 0;
      begin

         -- Count remaining tracks
         -----------------------------------------------------------------------       
         for Track of Tracks loop
            if Track.Id /= 0 then 
               Track_Count := Track_Count + 1;
            end if;
         end loop;
         
         return Track_Count;
         
      end Count_Tracks;
      --------------------------------------------------------------------------
      
      
      
      
      --========================================================================
      -- (See type declaration)
      --========================================================================
      procedure Add_Track (New_Track : Track_Record) is

         T    : Float;
         User : Id_Type;
         
      begin
         
         -- Update if already present
         -----------------------------------------------------------------------
         for Track of Tracks loop
         
            if Track.Id = New_Track.Id then
            
               -- TODO: > check that the updates make sence for the time lapse 
               --         between updates (huge jumps have been observed in OGN)
               --       > check that this track is not duplicated
               --         (split tracks with different IDs have also been observed)

               T := Float (New_Track.Timestamp - Track.Timestamp);
            
               if T <= 0.0 then
               
                  -- Duplicated within the same stream
                  --------------------------------------------------------------
                  return;
               
               end if;
               
               -- If this is a local device, write to log file
               --------------------------------------------------------------
               --if T >= Log_Lattency and then New_Track.Speed > 0 then

               for Device of Local_Devices loop

                  exit when Device.Id = 0;
                     
                  if Device.Id = Track.Id then
                     
                     Log_Track_Data (Device.Name, New_Track.Get_Igc_Image);
                        
                     exit;
               
                  end if;
                        
               end loop;

               --end if;
                
               -- Update track data
               --------------------------------------------------------------
               
               User := Track.User;
            
               Track := New_Track;
               
               Track.User := User;
               
               return;
            
            end if;
         
         end loop;
      
         -- Add as new one
         -----------------------------------------------------------------------
         for Track of Tracks loop
         
            if Track.Id = 0 then
            
               Track := New_Track;
            
               return;
            
            end if;
         
         end loop;
         
         -- Replace with track further from the reference or older than 20s
         -----------------------------------------------------------------------
         declare
            use Utility.Maps;
            D : Float := Distance (Reference, New_Track.Position);
         begin
         
            for Track of Tracks loop
         
               if 
                 Distance (Reference, Track.Position) > D or else
                 Float (New_Track.Timestamp - Track.Timestamp) > 20.0
               then
               
                  User := Track.User;
            
                  Track := New_Track;
               
                  Track.User := User;
                  
                  return;
            
               end if;
         
            end loop;
                         
         end;
    
      end Add_Track;
      --------------------------------------------------------------------------
      
      
      
      
      --========================================================================
      -- (See type declaration)
      --========================================================================
      procedure Add_User (User_Id  : Id_Type;
                          Time     : Natural;
                          Position : Position_Record; 
                          Altitude : Float;
                          Speed    : Float;
                          Course   : Float) is
      begin
         
         -- TODO: merge G-NAV track to OGN tracks;
         
         null;
                    
      end Add_User;
      --------------------------------------------------------------------------
      
      
      
      
      --========================================================================
      -- (See type declaration)
      --========================================================================
      function Get_Tracks (User_Id    : Id_Type;
                           Track_Data : String; 
                           Send_Id    : Boolean := False) return Stream_Element_Array is
         
         use Utility;
         
         T        : Integer;
         Count    : Short_Short_Natural := 0;
         Include  : array (Tracks'Range) of Boolean := (others => False);
         Ages     : array (Tracks'Range) of Short_Short_Integer;
         I        : Natural := Tracks'First;
         Position : Maps.Position_Record := Reference;
         Altitude : Float := 0.0;
         Course   : Float := 0.0;
         Speed    : Float := 0.0;
         Time     : Natural := 0;
         
         Buffer   : Stream_Element_Array (1..10_000);
         Holder   : aliased Stream_Buffer_Type;
         Source   : not null access Stream_Buffer_Type := Holder'Access;
         
         Utc_Offset   : Duration := 60.0 * Duration  (Ada.Calendar.Time_Zones.Utc_Time_Offset);                  
         Message_Time : Natural  := Natural (Seconds (Ada.Calendar.Clock - Utc_Offset));
         
      begin
         
         -- Configure stream buffer for online tracks
         -----------------------------------------------------------------------
         Source.Buffer := Buffer'Unrestricted_Access;
         Source.Cursor := Buffer'First;
         
         -- Parse downlinked IGC track data
         -----------------------------------------------------------------------      
         if
           Track_Data'First  =  1 and then
           Track_Data'Length > 40 and then
           Track_Data (1) = 'B'
         then
            Time := Time + Natural'Value (Track_Data (2..3)) * 3600;
            Time := Time + Natural'Value (Track_Data (4..5)) * 60;
            Time := Time + Natural'Value (Track_Data (6..7));
            
            -- DDMMmmm
            Position.Lat := Long_Float'Value (Track_Data (8..9));
            Position.Lat := Position.Lat + Long_Float'Value (Track_Data (10..11)) / 60.0;
            Position.Lat := Position.Lat + Long_Float'Value (Track_Data (12..14)) / 60000.0;
            if Track_Data (15) = 'S' then
               Position.Lat := -Position.Lat;
            end if;
            
            -- DDDMMmmm
            Position.Lon := Long_Float'Value (Track_Data (16..18));
            Position.Lon := Position.Lon + Long_Float'Value (Track_Data (19..20)) / 60.0;
            Position.Lon := Position.Lon + Long_Float'Value (Track_Data (21..23)) / 600000.0;
            if Track_Data (24) = 'W' then
               Position.Lon := -Position.Lon;
            end if;

            Altitude := Float'Value (Track_Data (31..35));
            Speed    := Float'Value (Track_Data (36..38));
            Course   := Float'Value (Track_Data (39..41));
            
            -- Correlate this track with one in the stack
            --------------------------------------------------------------------
            --Add_User (User_Id, Time, Position, Altitude, Speed, Course);            
                    
         elsif
           Track_Data'First  =  1 and then
           Track_Data'Length > 17 and then
           Track_Data (1) = 'H'
         then
            Position.Lat := Long_Float'Value (Track_Data (02..03));
            Position.Lat := Position.Lat + Long_Float'Value (Track_Data (04..05)) / 60.0;
            Position.Lat := Position.Lat + Long_Float'Value (Track_Data (06..08)) / 60000.0;
            if Track_Data (09) = 'S' then
               Position.Lat := -Position.Lat;
            end if;
            
            Position.Lon := Long_Float'Value (Track_Data (10..12));
            Position.Lon := Position.Lon + Long_Float'Value (Track_Data (13..14)) / 60.0;
            Position.Lon := Position.Lon + Long_Float'Value (Track_Data (15..17)) / 60000.0;
            if Track_Data (18) = 'W' then
               Position.Lon := -Position.Lon;
            end if;

         else
            Utility.Log.Log_Trace ("bad request for traffic <" & Track_Data & ">");
            
         end if;
               
         -- Filter old and/or distant tracks
         -----------------------------------------------------------------------      
         for Track of Tracks loop
         
            if Track.Id /= 0 then
            
               T := Track.Time - Message_Time;
                  
               if (abs T in 0..120) and then Distance (Track.Position, Position) < Traffic_Range then
                  Ages    (I) := Short_Short_Integer (T);
                  Include (I) := True;
                  Count       := Count + 1;
               end if;
            
            end if;
         
            exit when Count = 50;
         
            I := I + 1;
         
         end loop;
         
         -- Start writing
         -----------------------------------------------------------------------

         -- Timestamp
         Natural'Write (Source, Message_Time);
      
         -- Reference
         Long_Float'Write (Source, Reference.Lat);
         Long_Float'Write (Source, Reference.Lon);
      
         -- Count
         Short_Short_Natural'Write (Source, Count); --  4 bytes
      
         if Count > 0 then
         
            I := Tracks'First;
         
            for Track of Tracks loop
            
               if Include (I) then
               
                  Natural'Write (Source, Track.Id);
                  Short_Short_Integer'Write (Source, Ages (I));
                  Float'Write (Source, Track.Latitude);
                  Float'Write (Source, Track.Longitude);
                  Short_Natural'Write (Source, Track.Altitude);
                  Short_Short_Natural'Write (Source, Track.Speed);
                  Short_Short_Natural'Write (Source, Track.Course);
                  Short_Short_Integer'Write (Source, Track.Vario);
                  Short_Short_Integer'Write (Source, Track.Rotation);
               
               end if;
            
               I := I + 1;
            
            end loop;
         
         end if;
         
         if Send_Id and then User_Id /= No_Id then
            
            String'Write (Source, User_Id);
            
         end if;
         
         return Buffer (Buffer'First.. Source.Cursor);
         
      end Get_Tracks;
      --------------------------------------------------------------------------
         
      
   
   
      --========================================================================
      -- (See type declaration)
      --========================================================================
      procedure Write_Tracks (File_Name : String) is

         use Ada.Streams.Stream_IO;

         T       : Integer;
         File_Id : File_Type;
         Stream  : Ada.Streams.Stream_IO.Stream_Access;
         Count   : Short_Short_Natural := 0;
         
         Utc_Offset   : Duration := 60.0 * Duration  (Ada.Calendar.Time_Zones.Utc_Time_Offset); 
         Message_Time : Natural  := Natural (Seconds (Ada.Calendar.Clock - Utc_Offset));
         
      begin
      
         Count := 0;
      
         -- Recompute ages
         -----------------------------------------------------------------------      
         for Track of Tracks loop
            
            if Track.Id /= 0 then
            
               T := Track.Time - Message_Time;
                  
               if not (abs T in 0..120) then
                  Track.Id := 0;
               else               
                  Count     := Count + 1;  
                  Track.Age := Short_Short_Integer (T);
               end if;
                        
            end if;
         
         end loop;
          
         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => File_Name);

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         -- Timestamp
         Natural'Write (Stream, Message_Time);
         
         -- Reference
         Long_Float'Write (Stream, Reference.Lat);
         Long_Float'Write (Stream, Reference.Lon);
         
         -- Count
         Short_Short_Natural'Write (Stream, Count); --  4 bytes
      
         if Count > 0 then
         
            for Track of Tracks loop
            
               if Track.Id /= 0 then
               
                  Natural'Write (Stream, Track.Id);
                  Short_Short_Integer'Write (Stream, Track.Age);
                  Float'Write (Stream, Track.Latitude);
                  Float'Write (Stream, Track.Longitude);
                  Short_Natural'Write (Stream, Track.Altitude);
                  Short_Short_Natural'Write (Stream, Track.Speed);
                  Short_Short_Natural'Write (Stream, Track.Course);
                  Short_Short_Integer'Write (Stream, Track.Vario);
                  Short_Short_Integer'Write (Stream, Track.Rotation);
               
               end if;
            
            end loop;
         
         end if;
         
         Close (File_Id);
      
      end Write_Tracks;
      --------------------------------------------------------------------------
      
   end Track_Stack;
   -----------------------------------------------------------------------------
   
end Traffic;
--------------------------------------------------------------------------------
