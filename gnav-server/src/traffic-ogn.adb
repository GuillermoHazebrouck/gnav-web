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
with Ada.Command_Line;
-- Gnat
with Gnat.Sockets;
use  Gnat.Sockets;
-- Standard
with Ada.Calendar;
use  Ada.Calendar;
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
package body Traffic.Ogn is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The time at which the last message was received
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reception_Time : Natural := Natural (Seconds (Ada.Calendar.Clock));

   --===========================================================================
   -- Gets the OGN filter based on the start-up options
   --===========================================================================
   function Get_Filter return String is

      P : Position_Record := Reference;
      R : Float;

   begin

      P.Lat := Long_Float'Rounding (Reference.Lat);
      P.Lon := Long_Float'Rounding (Reference.Lon);

      -- Compute a new radius that covers the original region

      R := Float (Ogn_Range) + Distance (P, Reference);

      return "r/" &
        Utility.Float_Image (Float (P.Lat), 0) & "/" &
        Utility.Float_Image (Float (P.Lon), 0) & "/" &
        Utility.Float_Image (Float (R),     0);

   end Get_Filter;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Returns an integer from a string that could contain leading zeroes
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
   -- Converts an OGN message into a compact G-NAV binary representation
   --===========================================================================
   procedure Parse_Ogn_Message (Message : String) is
   begin

      -- Filter comments
      --------------------------------------------------------------------------
      if Message (Message'First) = '#' then
         --Ada.Text_IO.Put_Line (Message);
         return;
      end if;

      -- Extract fields
      --------------------------------------------------------------------------
      for I in Message'Range loop

         if Message (I) = ':' and I < Message'Last then

            if
              Message'Last   >= I + 44 and then
              Message (I +  1)  = '/'  and then
              Message (I + 26) /= '&'
            then

               -- This is a beacon
               -----------------------------------------------------------------
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

                  -- Aircraft type
                  --------------------------------------------------------------

                  K (1) := Message (I + 17);
                  K (2) := Message (I + 27);

                  -- Gliders-only mode
                  --------------------------
                  if K /= "/'" and Gliders_Only then
                     return;
                  end if;

                  -- Only general aviation
                  --------------------------
                  if K = "/^" then
                     return;
                  end if;

                  -- Creation time and age (s): maximum 30 seconds old
                  --------------------------------------------------------------

                  Track.Time := Get_Time (Message (I +  2 .. I +  7));

                  T := Track.Time - Reception_Time;

                --Utility.Log.Log_Trace ("track received " & Natural'Image (T));

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

                  Track.Position.Lat := Reference.Lat + P.Lat;
                  Track.Position.Lon := Reference.Lon + P.Lon;

                  -- Course (LSB = 1.5 deg)
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

                  -- Altitude (LSB = 1m)
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

                        Track.Id := Utility.Parse_Hex_Id (Message (J+4..J+9));

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

                  --Ada.Text_IO.Put_Line (Track.To_String);

                  Stack.Add_Track (Track);

               end;

            end if;

            exit;

         end if;

      end loop;

   exception
      when E : others =>

         Log_Error ("while parsing message >" & Message, E);

   end Parse_Ogn_Message;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Starts a tracker by connecting to the Open Glider Network
   --===========================================================================
   procedure Update_Tracks is

      Address        : Sock_Addr_Type;
      Socket_Id      : Socket_Type;
      Data           : Stream_Element_Array (1..10_000);
      Step           : Duration := 0.250;
      Timer          : Duration := 0.000;
      Last_Reception : Time;
      Filter         : String := Get_Filter;
      Buffer         : String (1..2000);
      K              : Natural := 0;
      Url            : String  := Utility.Trim (Ogn_Url);

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

         Utc_Offset : Duration := 60.0 * Duration  (Ada.Calendar.Time_Zones.Utc_Time_Offset);

      begin

         Last_Reception := Ada.Calendar.Clock;

         Reception_Time := Natural (Seconds (Last_Reception - Utc_Offset));

         -- Prepend pending part of previous message

         for J in Message'Range loop

            if Message (J) = ASCII.LF and I < J then

               if K > 0 then
                  Parse_Ogn_Message (Buffer (1..K) & Message (I..J-1));
                  K := 0;
               else
                  Parse_Ogn_Message (Message (I..J-1));
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
               Log_Error ("missing message part >" & Message (I..Message'Last));

            end if;

         end if;

      exception
         when E : others =>
            Log_Error ("error while processing message", E);

      end Process_Message;
      --------------------------------------------------------------------------

   begin

      Log_Trace ("connecting to APRS server " & Url & " with filter: " & Filter);

      Address.Addr := Addresses (Get_Host_By_Name (Url), 1);
      Address.Port := 14580; -- (use 10152 for full feed)

      Create_Socket     (Socket_Id, Address.Family, Socket_Stream);
      Set_Socket_Option (Socket_Id, Socket_Level, (Receive_Timeout, 60.0));
      Connect_Socket    (Socket_Id, Address);

      Log_Trace ("APRS server connected at " & Image (Address));

      Send_Message ("user GNAV-RX pass -1 vers GNAV 1.0 filter " & Filter & " t/tu" & ASCII.LF);

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

            elsif Last = Data'First - 1 then

               Log_Trace ("connection terminated by the server, attempting reconnection");
               return;

            else
               Ada.Text_IO.Put_Line (Stream_Element_Offset'Image (Last));

            end if;

            -- Renew keep alive every 60 seconds
            --------------------------------------------------------------------
            Timer := Timer + Step;

            if Timer > 60.0 then

               Send_Message ("# keepalive" & ASCII.LF);

               Stack.Clean_Tracks;

               -- Trace number of tracks
               -----------------------------------------------------------------
               declare
                  Track_Count : Natural := Stack.Count_Tracks;
               begin

                  if Track_Count = Stack_Size then
                     Log_Trace ("number of tracks:" & Natural'Image (Track_Count) & " (full stack)");
                  else
                     Log_Trace ("number of tracks:" & Natural'Image (Track_Count));
                  end if;

               end;

               Timer := 0.0;

            end if;

            -- Reset the connection after 1 minute of no reception
            --------------------------------------------------------------------
            if (Ada.Calendar.Clock - Last_Reception) > 60.0 then
               Log_Trace ("connection silence, attempting reconnection");
               return;
            end if;

            delay Step;

         exception
            when E : others =>
               Log_Error ("while reading from socket", E);
               Log_Trace ("attempting reconnection");
               return;
         end;

      end loop;

   exception
      when E : others =>
         Log_Error ("while reading tracks from OGN", E);

   end Update_Tracks;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   task body Maintain_Tracks is
   begin

      accept Start;

      loop
         Update_Tracks;
         delay 5.0;
      end loop;

   end Maintain_Tracks;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Start_Updating is
   begin

      Traffic.Setup_Tracker;

      Maintain_Tracks.Start;

   end Start_Updating;
   -----------------------------------------------------------------------------

end Traffic.Ogn;
--------------------------------------------------------------------------------
