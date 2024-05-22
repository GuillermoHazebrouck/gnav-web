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
with Ada.Command_Line;
-- Gnat
with GNAT.Sockets;
use  GNAT.Sockets;
-- Standard
with Ada.Exceptions;
with Ada.Calendar;
use  Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
-- Gnav
with Utility;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Traffic is
        
   Reference   : Position_Record := (50.7, 3.8);
   
   Limits      : Position_Record := (1.0, 2.0);
   
   Radius      : Long_Float      := 150.0;
   
   UTC_Offset  : Duration        := +2.0 * 3600.0;
         
   --===========================================================================
   -- Configures the tracker based on command line arguments
   --===========================================================================
   procedure Setup_Tracker is
      use Utility;
   begin
      
      for I in 1..Ada.Command_Line.Argument_Count loop

         declare
            Argument : String_Buffer (100);
         begin

            Argument.Load (Ada.Command_Line.Argument (I));

            declare
               Key : String := Argument.Read_Next ('=');
               Val : String := Argument.Read_Next ('=');
            begin

               if    Key = "LATITUDE" then

                  Reference.Lat := Long_Float (Integer'Value (Val));

               elsif Key = "LONGITUDE" then

                  Reference.Lon := Long_Float (Integer'Value (Val));

               elsif Key = "RANGE" then

                  Radius := abs Long_Float (Integer'Value (Val));

               elsif Key = "UTC" then

                  UTC_Offset := Duration'Value (Val);

               end if;

            end;

         end;

      end loop;
      
      declare
         V : Point_Record;
         R : Position_Record;
      begin
         
         V.Set (Radius, Radius);         
         R := Utility.Maps.Position (Reference, V);
         
         Limits.Lat := R.Lat - Reference.Lat;
         Limits.Lon := R.Lon - Reference.Lon;
         
      end;
      
   end Setup_Tracker;
   -----------------------------------------------------------------------------
   
   
   
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 2 bytes natural
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Short_Natural is range 0..2 ** 16 - 1;
   for Short_Natural'Size use Short_Integer'Size;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 1 byte natural
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Short_Short_Natural is range 0..2 ** 8 - 1;
   for Short_Short_Natural'Size use Short_Short_Integer'Size;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The track as represented in the binary file. The id is only for
   -- local use, so there are 14 bytes in total
   -- Tracks can live maximum 127 seconds from block timestamp
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Track_Record is tagged record
      
      Timestamp : Time;
      Time      : Natural;
      
      -- MESSAGE FIELDS                 -- Bytes / Units / Accuracy
      Id        : Natural := 0;         -- 4
      Age       : Short_Short_Integer;  -- 1       [s]     1
      Latitude  : Float;                -- 4       [deg]   ?
      Longitude : Float;                -- 4       [deg]   ?
      Altitude  : Short_Natural;        -- 2       [m]     1
      Vario     : Short_Short_Integer;  -- 1       [dm/s]  1
      Speed     : Short_Short_Natural;  -- 1       [m/s]   2
      Course    : Short_Short_Natural;  -- 1       [deg]   1.5
      Rotation  : Short_Short_Integer;  -- 1       [deg/s] 1
      
   end record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The list of active tracks
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Tracks : array (1..50) of Track_Record;
         
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of tracks sent trough the stream
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Track_Count : Short_Short_Natural := 0;
   
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
   --
   --===========================================================================
   function Get_Integer (Value : String) return Integer is
      
      B : Natural := 0;
      E : Natural := 1;
      R : Integer := 0;
      
   begin
      
      if Value'Length > 10 then
         return 0;
      end if;
      
      for C of reverse Value loop
         
         E := 10 ** B;
         
         case C is
            when '0' => null;
            when '1' => R := R + 1 * E;
            when '2' => R := R + 2 * E;
            when '3' => R := R + 3 * E;
            when '4' => R := R + 4 * E;
            when '5' => R := R + 5 * E;
            when '6' => R := R + 6 * E;
            when '7' => R := R + 7 * E;
            when '8' => R := R + 8 * E;
            when '9' => R := R + 9 * E;
            when '+' => 
               return R;
            when '-' => 
               return -R;
            when others =>
               -- Invalid
               return 0;
         end case;
         
         B := B + 1;         
                  
      end loop;
      
      return R;
      
   end Get_Integer;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   function Get_Natural (Value : String) return Natural is
      
      B : Natural := 0;
      E : Natural := 1;
      R : Natural := 0;
      
   begin
      
      if Value'Length > 10 then
         return 0;
      end if;
      
      for C of reverse Value loop
         
         E := 10 ** B;
         
         case C is
            when '0' => null;
            when '1' => R := R + 1 * E;
            when '2' => R := R + 2 * E;
            when '3' => R := R + 3 * E;
            when '4' => R := R + 4 * E;
            when '5' => R := R + 5 * E;
            when '6' => R := R + 6 * E;
            when '7' => R := R + 7 * E;
            when '8' => R := R + 8 * E;
            when '9' => R := R + 9 * E;
            when others =>
               -- Invalid
               return 0;
         end case;
         
         B := B + 1;         
                  
      end loop;
      
      return R;
      
   end Get_Natural;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   function Get_Time (Value : String) return Natural is
      H,
      M,
      S : Natural;
   begin
      
      H := Get_Natural (Value (Value'First+0..Value'First+1));
      M := Get_Natural (Value (Value'First+2..Value'First+3));
      S := Get_Natural (Value (Value'First+4..Value'First+5));
      
      return 3600 * H + 60 * M + S;
                            
   end Get_Time;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Parses the latitude values DDMM.MM<N/S>
   --===========================================================================
   function Get_Lat (Value : String) return Long_Float is
      D : Natural;
      M : Float;
      R : Long_Float;
   begin
         
      D := Get_Natural (Value (Value'First+0..Value'First+1));
      M := Float'Value (Value (Value'First+2..Value'First+6));
      
      R := Long_Float (D) + Long_Float (M / 60.0);
      
      if Value (Value'First+7) = 'S' then
         R := -R;
      end if;
      
      --Ada.Text_IO.Put_Line (Value & "->" & Lat_Image ((R,0.0)));
      
      return R;
      
   end Get_Lat;
   -----------------------------------------------------------------------------
      
   
   
   
   --===========================================================================
   -- Parses the longitude values DDDMM.MM<E/W>
   --===========================================================================
   function Get_Lon (Value : String) return Long_Float is
      D : Natural;
      M : Float;
      R : Long_Float;
   begin
         
      D := Get_Natural (Value (Value'First+0..Value'First+2));
      M := Float'Value (Value (Value'First+3..Value'First+7));
      
      R := Long_Float (D) + Long_Float (M / 60.0);
      
      if Value (Value'First+7) = 'W' then
         R := -R;
      end if;
      
      --Ada.Text_IO.Put_Line (Value & "->" & Lon_Image ((0.0,R)));
      
      return R;
      
   end Get_Lon;
   -----------------------------------------------------------------------------
   
   
   
      
   --===========================================================================
   -- Transforms the 8 hexa characters into a 4 bytes decimal
   --===========================================================================
   function Parse_Hex_Id (Value : String) return Natural is
      
      B : Natural := 0;
      E : Natural := 1;
      R : Natural := 0;
      
   begin
      
      if Value'Length > 8 then
         return 0;
      end if;
      
      for C of reverse Value loop

         E := 16 ** B;
         
         case C is
            when '0' => null;
            when '1' => R := R + 1  * E;
            when '2' => R := R + 2  * E;
            when '3' => R := R + 3  * E;
            when '4' => R := R + 4  * E;
            when '5' => R := R + 5  * E;
            when '6' => R := R + 6  * E;
            when '7' => R := R + 7  * E;
            when '8' => R := R + 8  * E;
            when '9' => R := R + 9  * E;
            when 'A' => R := R + 10 * E;
            when 'B' => R := R + 11 * E;
            when 'C' => R := R + 12 * E;
            when 'D' => R := R + 13 * E;
            when 'E' => R := R + 14 * E; 
            when 'F' => R := R + 15 * E;
            when others =>
               -- Invalid
               return 0;
         end case;
         
         B := B + 1;
         
      end loop;
      
      return R;
      
   end Parse_Hex_Id;
   -----------------------------------------------------------------------------
   
   
   
   
   
   Cached_Time : Natural := Natural (Seconds (Ada.Calendar.Clock));
   
   
   
   --===========================================================================
   -- Adds the track to the array
   --===========================================================================
   procedure Add_Track (New_Track : Track_Record) is
   begin
            
      -- Update if already present
      -------------------------------------
      for Track of Tracks loop
         
         if Track.Id = New_Track.Id then
            
            Track := New_Track;
            
            return;
            
         end if;
         
      end loop;
      
      -- Add as new one
      -------------------------------------
      for Track of Tracks loop
         
         if Track.Id = 0 then
            
            Track := New_Track;
            
            return;
            
         end if;
         
      end loop;
      
      Ada.Text_IO.Put_Line ("WARNING: the track stack is full");
      
   end Add_Track;
   -----------------------------------------------------------------------------
       
   
   
   
   --===========================================================================
   -- Writes the tracks to the output file
   --===========================================================================
   procedure Write_Tracks is

      use Ada.Streams.Stream_IO;

      T       : Integer;
      File_Id : File_Type;
      Stream  : Ada.Streams.Stream_IO.Stream_Access;

   begin
      
      Track_Count := 0;
      
      -- Recompute ages
      --------------------------------------------------------------------------      
      for Track of Tracks loop
            
         if Track.Id /= 0 then
            
            T := Track.Time - Cached_Time;
                  
            if not (abs T in 0..120) then
               Track.Id := 0;
            else               
               Track_Count := Track_Count + 1;  
               Track.Age   := Short_Short_Integer (T);
            end if;
                        
         end if;
         
      end loop;
           
      -- Write
      --------------------------------------------------------------------------      
      if Track_Count > 0 then

         --Ada.Text_IO.Put_Line ("#" & Short_Short_Natural'Image (Track_Count));
            
         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "traffic.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         -- Timestamp
         Natural'Write (Stream, Cached_Time);
         
         -- Reference
         Long_Float'Write (Stream, Reference.Lat);
         Long_Float'Write (Stream, Reference.Lon);
         
         -- Count
         Short_Short_Natural'Write (Stream, Track_Count); --  4 bytes
         
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
         
         Close (File_Id);
         
      end if;
         
   end Write_Tracks;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Converts an APR message into a compact G-NAV binary representation
   --===========================================================================
   procedure Parse_Apr_Message (Message : String) is
   begin
      
      if Message (Message'First) = '#' then
         Ada.Text_IO.Put_Line (Message);
         return;         
      end if;
      
      for I in Message'Range loop
         
         if Message (I) = ':' and I < Message'Last then
            
            if
              Message'Last   >= I + 44 and then
              Message (I +  1)  = '/'  and then              
              Message (I + 26) /= '&'
            then
               
               -- This is a beacon
               
               declare
                  Track : Track_Record;                  
                  T     : Integer;
                  P     : Position_Record;
                  S,
                  C,
                  A,
                  D     : Natural := 0;
                  K     : String (1..2);
               begin
                  
                  -- Reference timestamp (system only)
                  --------------------------------------------------------------                  
                  Track.Timestamp := Clock;
                  
                  -- Aircraft type: only gliders in G-NAV
                  --------------------------------------------------------------
                     
                  K (1) := Message (I + 17);
                  K (2) := Message (I + 27);
                  
                  if K /= "/'" then
                     return;
                  end if;
                    
                  -- Creation time and age (s): maximum 30 seconds old
                  --------------------------------------------------------------
                  
                  Track.Time := Get_Time (Message (I +  2 .. I +  7));
                  
                  T := Track.Time - Cached_Time;   
                  
                  if not (abs T in 0..30) then
                     return;
                  end if;
                  
                  Track.Age := Short_Short_Integer (T);
                      
                  -- Position: only inside the limits
                  --------------------------------------------------------------
                               
                  P.Lat := Get_Lat (Message (I +  9 .. I + 16)) - Reference.Lat;
                  P.Lon := Get_Lon (Message (I + 18 .. I + 26)) - Reference.Lon;
     
                  if
                    abs P.Lat > Limits.Lat or else
                    abs P.Lon > Limits.Lon 
                  then
                     return;
                  end if;
                  
                  Track.Latitude  := Float (P.Lat);
                  Track.Longitude := Float (P.Lon);
                          
                  -- Course (1.5 deg)
                  --------------------------------------------------------------
                  
                  C := Get_Natural (Message (I + 28 .. I + 30));
                  
                  if C in 0..360 then
                     Track.Course := Short_Short_Natural (Float (C) / 1.5);
                  else
                     -- Invalid course, can't be represented correctly
                     return;
                  end if;
                  
                  -- Speed (LSB = 2 km/h)
                  --------------------------------------------------------------
                  
                  S := Natural (Float (Get_Natural (Message (I + 32 .. I + 34))) * 1.852);
                  
                  if S in 25..500 then -- TODO: start from 25 km/h
                     Track.Speed := Short_Short_Natural (Float (S) / 2.0);
                  else
                     -- Invalid speed
                     return;
                  end if;
               
                  -- Altitude (m)
                  --------------------------------------------------------------
                  
                  A := Natural (Float (Get_Natural (Message (I + 38 .. I + 43))) * 0.3048);
                       
                  if A in 0..10_000 then
                     Track.Altitude := Short_Natural (A);
                  else
                     -- Invalid altitude
                     return;
                  end if;
                         
                  -- Id from the OGN extension
                  --------------------------------------------------------------
               
                  for J in I + 44 .. Message'Last - 10 loop
                  
                     if 
                       Message (J)   = 'i' and then
                       Message (J+1) = 'd'
                     then
                     
                        Track.Id := Parse_Hex_Id (Message (J+2..J+9));
                     
                        exit;
                     
                     end if;
               
                  end loop;
                          
                  -- Vario from the OGN extension (LSB => 0.1m/s)
                  --------------------------------------------------------------
                  
                  Track.Vario := 0;
                  
                  for J in I + 44 .. Message'Last - 2 loop
                  
                     if 
                       Message (J)   = 'f' and then
                       Message (J+1) = 'p' and then
                       Message (J+2) = 'm'
                     then
                     
                        for K in reverse J-10..J loop
                  
                           if Message (K) = ' ' then
                              
                              declare                                 
                                 Vario : Integer := Integer (Float'Value (Message (K+1..J-1)) * 0.0508);
                              begin
                                 
                                 if    Vario < -127 then
                                    Vario := -127;
                                 elsif Vario > 127 then
                                    Vario :=  127;                                   
                                 end if;
                                 
                                 Track.Vario := Short_Short_Integer (Vario);
                     
                              end;
                              
                              exit;
                     
                           end if;
                           
                        end loop;
                     
                     end if;
               
                  end loop;
                   
                  -- Rotation from the OGN extension
                  --------------------------------------------------------------
                  
                  Track.Rotation := 0;
                  
                  for J in I + 44 .. Message'Last - 2 loop
                  
                     if 
                       Message (J)   = 'r' and then
                       Message (J+1) = 'o' and then
                       Message (J+2) = 't'
                     then
                     
                        for K in reverse J-10..J loop
                  
                           if Message (K) = ' ' then
                              
                              declare                                 
                                 Rotation : Integer := Integer (Float'Value (Message (K+1..J-1)) * 3.0);
                              begin
                                 
                                 if    Rotation < -127 then
                                    Rotation := -127;
                                 elsif Rotation > 127 then
                                    Rotation :=  127;                                   
                                 end if;
                                 
                                 Track.Rotation := Short_Short_Integer (Rotation);
                     
                              end;
                              
                              exit;
                     
                           end if;
                           
                        end loop;
                     
                     end if;
               
                  end loop;
               
                  Ada.Text_IO.Put_Line (Track.To_String);
               
                  Add_Track (Track);
                                    
               end;
               
            end if;
            
            exit;
            
         end if;
         
      end loop;
       
   exception
      when E : others =>
         
         Ada.Text_IO.Put_Line ("ERROR: while parsing message >" & Message);
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         
   end Parse_Apr_Message;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Reads the aircraft data from the data file 'data/aircraft.dat'
   --===========================================================================
   procedure Start_Tracker is
      
      Address   : Sock_Addr_Type;
      Socket_Id : Socket_Type;
      Data      : Stream_Element_Array (1..10_000);
      Step      : Duration := 0.250;
      Timer     : Duration := 0.000;
      
      -- Storage for incomplete part of the message that could not be parsed
      Buffer    : String (1..2000);
      K         : Natural := 0;
      
      --========================================================================
      -- Send a message to the APR server
      --========================================================================
      procedure Send_Message (Message : String) is

         Buffer : Stream_Element_Array (1..Message'Length); 
         for Buffer'Address use Message'Address;
         Last   : Stream_Element_Offset := Buffer'First;
         
      begin
         
         while Last /= Buffer'Last loop            
            Send_Socket (Socket_Id, Buffer (Last..Buffer'Last), Last);
         end loop;
         
      end Send_Message;
      --------------------------------------------------------------------------
                    
      --========================================================================
      -- Digest the incoming data
      --========================================================================
      procedure Process_Message (Message : String) is
         
         I : Natural := Message'First;
                  
      begin
         
         Cached_Time := Natural (Seconds (Ada.Calendar.Clock - UTC_Offset));
         
         -- Prepend pending part of previous message
         
         for J in Message'Range loop
            
            if Message (J) = ASCII.LF and I < J then
               
               if K > 0 then
                  Parse_Apr_Message (Buffer (1..K) & Message (I..J-1));
                  K := 0;
               else                  
                  Parse_Apr_Message (Message (I..J-1));
               end if;
               
               I := J + 1;
               
            end if;
            
         end loop;
         
         if I < Message'Last then
            
            if Message'Last - I < Buffer'Length then
            
               K := Buffer'First;
               for J in I .. Message'Last loop               
                  Buffer (K) := Message (J);
                  K := K + 1;
               end loop;
                        
            else
               K := 0;                        
               Ada.Text_IO.Put_Line ("WARINING: missing message part >" & Message (I..Message'Last));
               
            end if;
            
         end if;
          
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("error while processing message");

      end Process_Message;
      --------------------------------------------------------------------------
      
   begin
      
      Ada.Text_IO.Put_Line ("Connecting to OGN server full feed stream...");
      
      <<Restart_Connection>>
      
      Address.Addr := Addresses (Get_Host_By_Name ("aprs.glidernet.org"), 1);
      Address.Port := 14580; -- (use 10152 for full feed)
              
      Create_Socket  (Socket_Id, Address.Family, Socket_Stream);
      Connect_Socket (Socket_Id, Address);
      
      Ada.Text_IO.Put_Line ("OGN server connected: " & Image (Address));
                
      Send_Message ("user GNAV-RX pass -1 vers GNAV 1.0 filter r/51/4/150 t/tu" & ASCII.LF);
      
      loop
         
         -- Read incoming messages
         -----------------------------------------------------------------------
         
         declare            
            Last : Stream_Element_Offset;
            From : Sock_Addr_Type;
         begin
            
            Receive_Socket (Socket_Id, Data, Last, From);
            
            if Last in Data'Range then

               declare
                  Messages : String (1.. Positive (Last)); 
                  for Messages'Address use Data'Address;
               begin                  
                  Process_Message (Messages);
               end;

            end if;
            
            Write_Tracks;
                     
            -- Renew keep alive every 10 seconds
            -----------------------------------------------------------------------
            Timer := Timer + Step;         
            if Timer > 20.0 then
               Send_Message ("# keepalive" & ASCII.LF);
               Ada.Text_IO.Put_Line ("**poll**");
               Timer := 0.0;
            end if;
            
            delay Step;
           
         exception    
            when E : others =>
               Ada.Text_IO.Put_Line ("ERROR: while reading from socket");
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
               goto Restart_Connection;
         end;
 
      end loop;
      
      --Parse_Apr_Message (Message => "FLRDDDEAD>APRS,qAS,EDER:/223000h5101.86N/00404.21E'342/049/A=005524 id0ADDDEAD -454fpm -1.1rot 8.8dB 0e +51.2kHz gps4x5");
      
   end Start_Tracker;
   -----------------------------------------------------------------------------
   
end Traffic;
--------------------------------------------------------------------------------
