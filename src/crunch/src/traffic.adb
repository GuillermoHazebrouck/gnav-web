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
                                        -- Bytes / Units / Accuracy
      Id        : Natural;              -- 4
      Age       : Short_Short_Integer;  -- 1 [s]    1
      Latitude  : Float;                -- 4 [deg]  ?
      Longitude : Float;                -- 4 [deg]  ?
      Altitude  : Short_Natural;        -- 2 [m]    1
      Vario     : Short_Short_Integer;  -- 1 [dm/s] 1
      Speed     : Short_Short_Natural;  -- 1 [m/s]  1
      Course    : Short_Short_Natural;  -- 1 [deg]  1.5
      
   end record;
   
   function To_String (This : Track_Record) return String is
   begin
      
      return "track: " &
        Natural'Image (This.Id)                 & "/" &
        Short_Short_Integer'Image (This.Age)    & "/" &
        Float'Image (This.Latitude)             & "/" &
        Float'Image (This.Longitude)            & "/" &
        Short_Natural'Image (This.Altitude)     & "/" &
        Short_Short_Integer'Image (This.Vario)  & "/" &
        Short_Short_Natural'Image (This.Speed)  & "/" &
        Short_Short_Natural'Image (This.Course);
              
   end To_String;
   
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
      
      Ada.Text_IO.Put_Line (Value & "->" & Lat_Image ((R,0.0)));
      
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
      
      Ada.Text_IO.Put_Line (Value & "->" & Lon_Image ((0.0,R)));
      
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
   
   
   
   Cached_Time : Natural := Natural (Ada.Calendar.Seconds (Ada.Calendar.Clock));
   
   --===========================================================================
   -- Converts an APR message into a compact G-NAV binary representation
   --===========================================================================
   procedure Parse_Apr_Message (Message : String) is
      Track : Track_Record;
   begin
      
      for I in Message'Range loop
         
         if Message (I) = ':' and I < Message'Last then
            
            if 
              Message (I + 1) = '/'    and then
              Message'Last   >= I + 44 and then
              Message (Message'First + 26) /= '&'
            then
               
               -- This is a beacon
               
               declare
                  T : Integer;
                  P : Position_Record;
                  S,
                  C,
                  A,
                  D : Natural := 0;                  
               begin
                  
                  T     := Get_Time    (Message (I +  2 .. I +  7)) - Cached_Time;
                  P.Lat := Get_Lat     (Message (I +  9 .. I + 16)) - Reference.Lat;
                  P.Lon := Get_Lon     (Message (I + 18 .. I + 26)) - Reference.Lon;
                  S     := Get_Natural (Message (I + 28 .. I + 30));
                  C     := Get_Natural (Message (I + 32 .. I + 34));
                  A     := Get_Natural (Message (I + 38 .. I + 43));
                  
                  Ada.Text_IO.Put_Line ("offset=" & Image (P));
                           
                  if not (abs T in 1..120) then
                     -- The track differs more than 2 minutes -> discard  
                     Ada.Text_IO.Put_Line ("too old:" & Integer'Image (T));
                     return;
                  end if;
                          
                  if
                    abs P.Lat > Limits.Lat or else
                    abs P.Lon > Limits.Lon 
                  then
                     -- The track is out of the zone of interest -> discard
                     Ada.Text_IO.Put_Line ("out of bounds " & Image (P));
                     return;
                  end if;
                          
                  Track.Age       := Short_Short_Integer (T);                       
                  Track.Latitude  := Float (P.Lat);
                  Track.Longitude := Float (P.Lon);
                  
                  if S in 0..918 then
                     Track.Speed := Short_Short_Natural (Float (S) / 3.6);
                  else
                     Track.Speed := 255; --> read as invalid
                  end if;
                       
                  if C in 0..360 then
                     Track.Course := Short_Short_Natural (Float (C) / 1.5);
                  else
                     Track.Course := 255; --> read as invalid
                  end if;
                             
                  if A in 0..45_000 then
                     Track.Altitude := Short_Natural (A);
                  else
                     Track.Altitude := 65_535; --> read as invalid
                  end if;
                                       
               end;
               
               -- Recover the id
               
               for J in I + 44 .. Message'Last - 10 loop
                  
                  if 
                    Message (J)   = 'i' and then
                    Message (J+1) = 'd'
                  then
                     
                     Track.Id := Parse_Hex_Id (Message (J+2..J+9));
                     
                     exit;
                     
                  end if;
               
               end loop;
               
               Ada.Text_IO.Put_Line (Track.To_String);
               
               -- Search for the flight id in the stack and update it
      
            end if;
            
            exit;
            
         end if;
         
      end loop;
      
   end Parse_Apr_Message;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Reads the aircraft data from the data file 'data/aircraft.dat'
   --===========================================================================
   procedure Start_Tracker is
   begin
      
      Parse_Apr_Message (Message => "FLRDDDEAD>APRS,qAS,EDER:/223000h5101.86N/00404.21E'342/049/A=005524 id0ADDDEAD -454fpm -1.1rot 8.8dB 0e +51.2kHz gps4x5");
      
   end Start_Tracker;
   -----------------------------------------------------------------------------
   
end Traffic;
--------------------------------------------------------------------------------
