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

   Test_Mode : Boolean := False;
   
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

               elsif Key = "OGN_CEILING" then

                  Ogn_Ceiling := abs Integer'Value (Val);

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

               elsif Key = "OGN_GENERAL_AVIATION_ONLY" then

                  Small_Only := True;
                  
                  Log_Trace ("serving gliders only");

               elsif Key = "LOCAL_DEVICE" then

                  if Val'Length >= 13 then
                     
                     for Device of Local_Devices loop
                     
                        if Device.Id = 0 then
                           
                           -- Flarm ID
                           Device.Id := Parse_Hex_Id (Val (Val'First .. Val'First + 5));
                           
                           -- Registration
                           Utility.Override (Device.Name, Val (Val'First + 7 .. Val'First + 12));
                                  
                           -- Competition marks (optional)
                           if Val'Length >= 16 then                              
                              Utility.Override (Device.Mark, Val (Val'First + 14 .. Val'First + 15));                                  
                           end if;
                              
                           Log_Trace ("registered local tracking device " & Device.Name & "(" & Device.Mark & ") Id=" & Natural'Image (Device.Id));

                           exit;
                           
                        end if;
                     
                     end loop;
                     
                  else
                     
                     Log_Trace ("wrong format for local tracking device " & Val);

                  end if;
                  
               elsif Key = "LOCAL_USER" then

                  if Val'Length = 8 then
                     
                     Users_Stack.Add_To_Register (Val);
                     
                     Log_Trace ("registered local user " & Val);

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
      
      for I in 1..Ada.Command_Line.Argument_Count loop
         if Ada.Command_Line.Argument (I) = "TEST" then
            Test_Mode := True;
            exit;
         end if;
      end loop;
      
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
   -- (See specification file)
   --===========================================================================
   procedure Update_From (This : in out Track_Record; Other : Track_Record) is
   begin
      
      -- Workspace variables
      This.Timestamp := Other.Timestamp;
      This.Time      := Other.Time;
      This.Position  := Other.Position;
      -- Message
      This.Id        := Other.Id;
      This.Age       := Other.Age;
      This.Latitude  := Other.Latitude;
      This.Longitude := Other.Longitude;
      This.Altitude  := Other.Altitude;
      This.Vario     := Other.Vario;
      This.Speed     := Other.Speed;
      This.Course    := Other.Course;
      This.Rotation  := Other.Rotation;
      
   end Update_From;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Updates the external fields
   --===========================================================================
   procedure Update_From (This : in out Track_Record; User : User_Record) is
   begin
      
      This.Timestamp := User.Timestamp;
      This.Time      := User.Time;
      This.User      := User;                    
      This.Position  := User.Position;
      This.Latitude  := Float (User.Position.Lat - Reference.Lat);
      This.Longitude := Float (User.Position.Lon - Reference.Lon);
      This.Altitude  := Short_Natural (User.Altitude);
      This.Speed     := Short_Short_Natural (User.Speed  / 2.0);
      This.Course    := Short_Short_Natural (User.Course / 1.5);
      This.Rotation  := 0;
      This.Vario     := 0;
      
   end Update_From;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Tailmark (This : Track_Record) return Track_Marks is
   begin
      
      if This.Device /= null then
         
         return This.Device.Mark;
         
      elsif Test_Mode then
         
         return "XX";
         
      else
         return No_Mark;
         
      end if;
      
   end Get_Tailmark;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Returns the track flags based on the data
   --===========================================================================
   function Get_Bit_Flags (This : Track_Record) return Short_Short_Natural is
   begin
      
      case This.Source is
         when Source_Ogn =>
            return 2;
         when Source_Gnav =>
            return 1;
         when Source_None => 
            return 0;
      end case;
      
   end Get_Bit_Flags; 
   -----------------------------------------------------------------------------   
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Merged_User (This : Track_Record; User : User_Record) return Boolean is
      
      T : Integer := abs (This.Time - User.Time);
      
   begin
      
      if This.Source = Source_None then
         
         return False;
      
      elsif This.Source = Source_Gnav and then This.User.Id = User.Id then
         
         -- The user created this track
         return True;
         
      elsif This.User.Id /= User.Id then
         
         -- To stablish first association the user must be:
         --  > moving at more than 40km/h
         --  > within 600m to the last known position of the track 
         --  > both reports no more than 12s appart
         return User.Speed > 40.0 and then T < 12 and then Distance (This.Position, User.Position) < 0.6;
         
      elsif This.User.Id = User.Id then
         
         -- Maintain close position
         --  > within 1km to the last known position of the track 
         --  > both reports no more than 20s appart
         return T < 20 and then Distance (This.Position, User.Position) < 1.0;
         
      else
         return False;
         
      end if;
      
   end Merged_User;
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
         Now : Time := Clock;
      begin
         
         -- Remove tracks older than 120s
         -----------------------------------------------------------------------
         for Track of Tracks loop
            if Track.Source /= Source_None and then (Now - Track.Timestamp) > 120.0 then 
               Track := No_Track;
            end if;
         end loop;
         
         -- For tracks older than 60s, remove the user link
         -----------------------------------------------------------------------
         for Track of Tracks loop       
            if Track.User.Id /= No_Id and then Now - Track.Timestamp > 60.0 then               
               Track.User := No_User;                  
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
            
            if Track.Source /= Source_None then 
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

         T : Float;

      begin
         
         -- Update if already present
         -----------------------------------------------------------------------
         for Track of Tracks loop
         
            if Track.Source = Source_Ogn and then Track.Id = New_Track.Id then
            
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
               
               -- Couple a local device
               -----------------------------------------------------------------
               
               if not Track.Checked then
                  
                  Track.Checked := True;
                  
                  for Device of Local_Devices loop

                     exit when Device.Id = 0;
                     
                     if Device.Id = Track.Id then
                     
                        Track.Device := Device'Access;
                     
                        exit;
               
                     end if;
                        
                  end loop;
                  
               end if;
               
               -- If this is a local device, write to log file and pick label
               --------------------------------------------------------------
               
               if Track.Device /= null then
                  
                  Log_Track_Data (Track.Device.Name, New_Track.Get_Igc_Image);
                  
               end if;

               -- Update track data
               --------------------------------------------------------------
               
               Track.Update_From (New_Track);
               
               return;
            
            end if;
         
         end loop;
         
         -- Try merging to an existent user track 
         -- (the G-NAV user track becomes an OGN track)
         -----------------------------------------------------------------------
         for Track of Tracks loop
         
            if Track.Source = Source_Gnav and then New_Track.Merged_User (Track.User) then
            
               Track.Id := New_Track.Id;
               
               Track.Source := Source_Ogn;
               
               Track.Update_From (New_Track);
               
               return;
            
            end if;
         
         end loop;
         
         -- Add as new one
         -----------------------------------------------------------------------
         for Track of Tracks loop
         
            if Track.Source = Source_None then
            
               Track := New_Track;
            
               return;
            
            end if;
         
         end loop;
         
         -- Replace with any track further from the reference or older than 20s
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
               
                  Track := New_Track;
                  
                  return;
            
               end if;
         
            end loop;
                         
         end;
    
      end Add_Track;
      --------------------------------------------------------------------------
      
      
      
      
      --========================================================================
      -- (See type declaration)
      --========================================================================
      procedure Link_User (User : Users.User_Record) is
 
         Linked : Boolean := False;
         
      begin

         if User.Id /= No_Id then
              
            -- Try merging user track to OGN track
            -------------------------------------------------------
            for Track of Tracks loop
                   
               if Linked then 
                  
                  if Track.User.Id = User.Id then
                  
                     Track.User := No_User;
                     
                  end if;
                  
               elsif Track.Source = Source_Ogn and then Track.Merged_User (User) then
                     
                  Linked := True;
                  
                  Track.User := User;
                  
               end if;
               
            end loop;
         
            -- Try updating the user track
            -------------------------------------------------------
            if not Linked then
                           
               for Track of Tracks loop
                    
                  if Linked then
                  
                     if Track.User.Id = User.Id then
                  
                        Track.User := No_User;
                     
                     end if;
                  
                  elsif Track.Source = Source_Gnav and then Track.User.Id = User.Id then
                     
                     Track.Update_From (User);
                  
                     Linked := True;
                  
                  end if;
               
               end loop;
               
            end if;
            
            -- Create a new user track (only when moving)
            ----------------------------------------------
            if not Linked and then (Test_Mode or User.Speed > 40.0) then
         
               for Track of Tracks loop
         
                  if Track.Source = Source_None then
                     
                     Track := No_Track;
                     
                     Track.Source := Source_Gnav;
                     
                     Track.Update_From (User);
                  
                     exit;
                  
                  end if;
         
               end loop;
            
            end if;
            
         end if;
         
      end Link_User;
      --------------------------------------------------------------------------
      
      
      
      
      --========================================================================
      -- (See type declaration)
      --========================================================================
      function Get_Tracks (User    : Users.User_Record;
                           Send_Id : Boolean := False) return Stream_Element_Array is
         
         use Utility;
         
         T        : Integer;
         Count    : Short_Short_Natural := 0;
         Include  : array (Tracks'Range) of Boolean := (others => False);
         Ages     : array (Tracks'Range) of Short_Short_Integer;
         I        : Natural := Tracks'First;
         
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
                        
         -- Filter own, old and/or distant tracks
         -----------------------------------------------------------------------      
         for Track of Tracks loop
         
            if Track.Source /= Source_None and then Track.User.Id /= User.Id then
            
               T := Track.Time - Message_Time;
                  
               if (abs T in 0..60) and then Distance (Track.Position, User.Position) < Traffic_Range then
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
               
                  Short_Short_Natural'Write (Source, Track.Get_Bit_Flags);
                  Short_Short_Integer'Write (Source, Ages (I));
                  Float'Write (Source, Track.Latitude);
                  Float'Write (Source, Track.Longitude);
                  Short_Natural'Write (Source, Track.Altitude);
                  Short_Short_Natural'Write (Source, Track.Speed);
                  Short_Short_Natural'Write (Source, Track.Course);
                  Short_Short_Integer'Write (Source, Track.Vario);
                  Short_Short_Integer'Write (Source, Track.Rotation);
                  Track_Marks'Write (Source, Track.Get_Tailmark);
                  Track_Marks'Write (Source, Track.User.Mark);
                  
               end if;
            
               I := I + 1;
            
            end loop;
         
         end if;
         
         if Send_Id and then User.Id /= No_Id then
            
            String'Write (Source, User.Id);
            
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
            
               if Track.Source /= Source_None then
               
                  Natural'Write (Stream, Track.Id);
                  Short_Short_Integer'Write (Stream, Track.Age);
                  Float'Write (Stream, Track.Latitude);
                  Float'Write (Stream, Track.Longitude);
                  Short_Natural'Write (Stream, Track.Altitude);
                  Short_Short_Natural'Write (Stream, Track.Speed);
                  Short_Short_Natural'Write (Stream, Track.Course);
                  Short_Short_Integer'Write (Stream, Track.Vario);
                  Short_Short_Integer'Write (Stream, Track.Rotation);
                  Track_Marks'Write (Stream, Track.Get_Tailmark);
                  
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
