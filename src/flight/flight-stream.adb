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
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams;
use  Ada.Streams;
with Ada.Text_IO;
use  Ada.Text_IO;
with Gnat.Sockets;
use  Gnat.Sockets;
-- Gnav
with Flight.Parsing;
use  Flight.Parsing;
with Flight.Traffic;
with Maps.Loader;
with Timing.Events;
with Utility.Log;
with Utility.Serial;
use  Utility.Serial;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Stream is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active data source kind
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Source : Source_Kind := Source_None;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active protocol
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Protocol : Protocol_Kind := Protocol_Nmea;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active port
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Active_Port : Dynamic_String := Empty_String;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the incoming messges must be recorded on the replay file
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Record_Messages : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A serial port
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Channel_Record is record

      Name : Dynamic_String;

      Port : Dynamic_String;

      Kind : Source_Kind;

      Prot : Protocol_Kind;

   end record;

   No_Channel : constant Channel_Record := (Name => Empty_String,
                                            Port => Empty_String,
                                            Kind => Source_None,
                                            Prot => Protocol_Nmea);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Available stream channels
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Channels : array (Channel_Range) of Channel_Record := (others => No_Channel);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The active channel
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Channel : Channel_Range := Channel_Range'First;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The server socket configuration
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Socket_Id   : Socket_Type    := No_Socket;
   Address     : Sock_Addr_Type := No_Sock_Addr;
   From        : Sock_Addr_Type := No_Sock_Addr;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The serial port and reconnection timer
   -- NOTE: the serial reconnection is tried every 4 seconds
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Serial_Id        : Serial_Port;
   Serial_Reconnect : access Timing.Events.Timer_Record;
   Serial_Ublox     : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 2KiB static storage for the data stream (used for serial or UDP
   -- acquisition)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Data          : Stream_Element_Array (1..2000);
   Buffer        : aliased Stream_Buffer_Type;
   Store         : not null access Stream_Buffer_Type := Buffer'Access;
   Last          : Stream_Element_Offset;
   Stream_Active : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The replay/recording file
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Replay_File      : Ada.Text_IO.File_Type;
   Replay_Index     : Replay_Range := Replay_Range'First;
   Replay_Active    : Boolean := False;
   Replay_Data      : not null access Replay_Record := Replay_Register (Replay_Index)'access;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- General
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Default_Position : Position_Record := No_Position_Record;
   Default_Map      : Dynamic_String  := Empty_String;
   Data_Rate        : Natural := 0;


   --===========================================================================
   -- Configures a channel based on the data string
   --===========================================================================
   procedure Parse_Channel (Index : Channel_Range; Data : String) is

      Reader : Utility.Strings.String_Buffer (100);

   begin

      Reader.Load (Data);

      -- Name
      ------------------------------
      Channels (Index).Name := +Reader.Read_Next (':');

      -- Protocol
      ------------------------------
      declare
         Value : String := Reader.Read_Next (':');
      begin

         if Value = "GNAV" then
            Channels (Index).Prot := Protocol_Gnav;
         else
            Channels (Index).Prot := Protocol_Nmea;
         end if;

      end;

      -- Source
      ------------------------------
      declare
         Value : String := Reader.Read_Next (':');
      begin

         if Value = "SERIAL" then
            Channels (Index).Kind := Source_Serial;
         elsif Value = "SOCKET" then
            Channels (Index).Kind := Source_Socket;
         else
            Channels (Index).Kind := Source_None;
         end if;

      end;

      -- Port
      ------------------------------
      Channels (Index).Port := +Reader.Read_Next (':');

   end Parse_Channel;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Source_Kind return Source_Kind is
   begin

      return Active_Source;

   end Get_Source_Kind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Protocol_Kind return Protocol_Kind is
   begin

      return Active_Protocol;

   end Get_Protocol_Kind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Protocol_Image (Value : Protocol_Kind) return String is
   begin

      case Value is

         when Protocol_Nmea =>

            return "NMEA";

         when Protocol_Gnav =>

            return "GNAV";

      end case;

   end Get_Protocol_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the active channel
   --===========================================================================
   function Get_Channel_Index return Channel_Range is
   begin

      return Channel;

   end Get_Channel_Index;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the name of the Ith available channel
   --===========================================================================
   function Get_Channel_Name (Index : Channel_Range) return String is
   begin

      return -Channels (Index).Name;

   end Get_Channel_Name;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Port return String is
   begin

      return -Active_Port;

   end Get_Port;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the protocol kind from a string representation
   --===========================================================================
   function Get_Protocol_Value (Value : String) return Protocol_Kind is
   begin

      for P in Protocol_Kind loop

         if Get_Protocol_Image (P) = Value then

            return P;

         end if;

      end loop;

      return Protocol_Gnav;

   end Get_Protocol_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Calculate_Rate is
   begin

      Data_Rate := Natural (Float (Message_Counter) / 5.0);

      Message_Counter := 0;

   end Calculate_Rate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Rate return Natural is
   begin

      return Data_Rate;

   end Get_Rate;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns a new file named called FLIGHT_NN.DAT, where NN is a counter from
   -- 00 to 99. If all files exists, it returns FLIGHT_XX.DAT. This is to avoid
   -- saturating the system with recording files.
   --===========================================================================
   function Get_Recording_File_Name (Index : Replay_Range) return Dynamic_String is

      use Utility.Strings;

      Count : String (1..2) := (others => '0');

   begin

      Override (Count, Trim (Replay_Range'Image (Index)), '0', True);

      return +(Utility.Base_Directory & "replay/FLIGHT_" & Count & ".DAT");

   end Get_Recording_File_Name;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Replay return Boolean is
   begin

      return Active_Source = Source_File;

   end Is_Replay;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Replay_Index return Replay_Range is
   begin

      return Replay_Index;

   end Get_Replay_Index;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Replay_Info (Index : Replay_Range) return Replay_Record is
   begin

      return Replay_Register (Index);

   end Get_Replay_Info;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Valid (Index : Replay_Range) return Boolean is
   begin

      return Replay_Register (Index) /= No_Replay_Record;

   end Is_Valid;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function To_Metadata (This : Replay_Record) return Replay_Info is

      Info : Replay_Info;

   begin

      Override (Info,
                Date_Image  (This.Start)             & ";" &
                Span_Image  (Seconds (This.Start))   & ";" &
                Span_Image  (This.Span)              & ";" &
                (-This.Map)                          & ";" &
                Get_Protocol_Image (This.Protocol)   & ";" &
                Integer_Image (This.Distance)        & ";" &
                Integer_Image (This.Altitude));

      return Info;

   exception
      when E : others =>
         Utility.Log.Put_Message (E, "error while generating replay metadata");
         return Info;

   end To_Metadata;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure From_Metadata (This : in out Replay_Record; Data : String) is

      Reader : Utility.Strings.String_Buffer (Data'Length);

   begin

      Reader.Load (Data);

      declare
         Date : String := Reader.Read_Next (';');
         Hour : String := Reader.Read_Next (';');
      begin
         This.Start    :=  Date_Value (Date, Hour);
      end;

      This.Span     :=  Span_Value (Reader.Read_Next (';'));
      This.Map      := +Reader.Read_Next (';');
      This.Protocol :=  Get_Protocol_Value (Reader.Read_Next (';'));
      This.Distance :=  Natural'Value (Reader.Read_Next (';'));
      This.Altitude :=  Natural'Value (Reader.Read_Next (';'));

      Utility.Log.Put_Message ("read metadata: " & This.To_Metadata);

   exception
      when E: others =>
         Utility.Log.Put_Message (E, "error while reading replay metadata");

   end From_Metadata;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Update (This : in out Replay_Record) is
   begin

      This.Span := Get_Utc_Time - This.Start;

      --if Flight.Data.Is_Valid (Field_Distance) then
      This.Distance := This.Distance + Integer (Flight.Data.Distance);
      --end if;

      if Flight.Data.Is_Valid (Field_Altitude) then
         This.Altitude := Integer'Max (Integer (Flight.Data.Altitude), This.Altitude);
      end if;

   end Update;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Flushes the recording file
   --===========================================================================
   procedure Flush_Recording is
      use type Dynamic_String;
   begin

      if Active_Source /= Source_File then

         if Is_Open (Replay_File) then

            Replay_Data.Update;

            Flush (Replay_File);

         elsif Flight.Data.Is_Recent (Field_Position) then -- Time_Synchronized and then

            -- Start replay after time is synchronized and there is a fix
            ---------------------------------------------------------------
            for Index in Replay_Range loop

               if Replay_Register (Index).Name = Empty_String then

                  Utility.Log.Put_Message ("starting recording");

                  Replay_Register (Index).Name  := Get_Recording_File_Name (Index);

                  Replay_Register (Index).Start := Get_Utc_Time;

                  Replay_Register (Index).Map   := +Maps.Get_Dataset_Name;

                  Replay_Data := Replay_Register (Index)'access;

                  Create   (Replay_File, Out_File, -Replay_Register (Index).Name);

                  Put_Line (Replay_File, "#METADATA=" & Replay_Register (Index).To_Metadata); -- reserved

                  Put_Line (Replay_File, "#");

                  Record_Messages := True;

                  return;

               end if;

            end loop;

         end if;

      end if;

   end Flush_Recording;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setups the replay file for replay data acqusition
   --===========================================================================
   procedure Setup_Replay is
   begin

      Utility.Log.Put_Message ("setting up replay data acqusition from file " & (-Replay_Data.Name));

      Active_Source   := Source_File;

      Active_Protocol := Replay_Data.Protocol;

      Active_Port     := Replay_Data.Name;

      Record_Messages := False;

      Maps.Loader.Load_Dataset (-Replay_Data.Map);

      if Ada.Directories.Exists (-Replay_Data.Name) then

         Open (Replay_File, In_File, -Replay_Data.Name);

         Stream_Active := True;

      else

         Flight.Data.Position := Default_Position;

         Utility.Log.Put_Message ("warning: the provided replay file dos not exist");

      end if;

   exception
      when E: others =>

         Utility.Log.Put_Message (E, "error while setting up replay");

         if Is_Open (Replay_File) then

            Close (Replay_File);

         end if;

         Flight.Data.Position := Default_Position;

   end Setup_Replay;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setup the socket port for data acquisition
   --===========================================================================
   procedure Setup_Socket (Port     : Dynamic_String := Active_Port;
                           Protocol : Protocol_Kind  := Active_Protocol) is
   begin

      Utility.Log.Put_Message ("setting up UDP socket for data aquisition");

      Active_Source   := Source_Socket;

      Active_Port     := Port;

      Active_Protocol := Protocol;

      Utility.Log.Put_Message ("target port: " & (-Active_Port));

      -- Load the default map (if different than actual)
      -----------------------------------------------------------------
      Maps.Loader.Load_Dataset (-Default_Map);

      Flight.Data.Position := Default_Position;

      -- Setup streaming
      -----------------------------------------------------------------

      Address.Addr  := Any_Inet_Addr;

      if Length (Active_Port) = 0 then
         Address.Port := 4000;
      else
         Address.Port := Port_Type'Value (-Active_Port);
      end if;

      Utility.Log.Put_Message ("address: " & Image (Address.Addr));
      Utility.Log.Put_Message ("port   :"  & Port_Type'Image (Address.Port));

      Create_Socket     (Socket_Id, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Socket_Id, Socket_Level, (Reuse_Address, True));
      Set_Socket_Option (Socket_Id, Socket_Level, (Receive_Timeout, Timeout => 0.001));
      Bind_Socket       (Socket_Id, Address);

      Stream_Active := True;

   end Setup_Socket;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Connects to the serial port for data acquisition
   --===========================================================================
   procedure Setup_Serial (Port     : Dynamic_String := Active_Port;
                           Protocol : Protocol_Kind  := Active_Protocol) is
   begin

      Utility.Log.Put_Message ("setting up serial port for data aquisition");

      Active_Source   := Source_Serial;

      Active_Port     := Port;

      Active_Protocol := Protocol;

      Utility.Log.Put_Message ("target port: " & (-Active_Port));

      -- Load the default map (if different than actual)
      -----------------------------------------------------------------
      Maps.Loader.Load_Dataset (-Default_Map);

      Flight.Data.Position := Default_Position;

      -- Setup streaming
      -----------------------------------------------------------------

      begin

         Open (Port => Serial_Id,
               Name => Port_Name (-Active_Port));

      exception
         when E : Serial_Error =>
            Utility.Log.Put_Message (E, "could not connect to serial device");
            return;
      end;

      Utility.Log.Put_Message ("serial open");

      Set (Port      => Serial_Id,
           Rate      => B9600,
           Bits      => CS8,
           Stop_Bits => One,
           Parity    => None,
           Block     => True,
           Local     => True,
           Flow      => None,
           Timeout   => 10.0);

      Utility.Log.Put_Message ("serial configured");

      Serial_Reconnect.Stop;

      Stream_Active := True;

      if Serial_Ublox then

         -- Configure U-BLOX for airborne mode < 1g
         -- (see UBX-CFG-NAV5 message [16#06#,16#24#])
         ----------------------------------------------------

         Write (Serial_Id,
                (16#B5#,16#62#,16#06#,16#24#,16#24#,16#00#,
                 16#01#,16#00#,16#06#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,
                 16#00#,16#00#,16#00#,16#00#,16#55#,16#B4#));

         Utility.Log.Put_Message ("UBLOX airborne configuration sent");

      end if;

   exception
      when E : Serial_Error =>
         Utility.Log.Put_Message (E, "error while connecting to serial device");
         return;
   end Setup_Serial;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Attempts to reconnect the serial
   --===========================================================================
   procedure Reconnect_Serial is
   begin

      if Active_Source = Source_Serial then

         Setup_Serial;

      end if;

   end Reconnect_Serial;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setups the stream buffer and data recording file
   --===========================================================================
   procedure Setup_Interface is
   begin

      Utility.Log.Put_Message ("setting up interface and recording...");

      Active_Source   := Source_Interface;

      Active_Protocol := Protocol_Gnav; -- (for recording only)

      Active_Port     := Empty_String;

      -- Load the default map
      -----------------------------------------------------------------
      Maps.Loader.Load_Dataset (-Default_Map);

   end Setup_Interface;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is

      use Ada.Text_IO;
      use type Dynamic_String;

      Reader          : Utility.Strings.String_Buffer (100);
      Selected_Source : Source_Kind := Source_None;
      File_Name       : String := Utility.Base_Directory & "setup.dat";
      File            : File_Type;

   begin

      -- Load the replay register using the metadata
      --------------------------------------------------------------------------

      Utility.Log.Put_Message ("loading replay register");

      for I in Replay_Range loop

         declare
            Replay_File_Name : Dynamic_String := Get_Recording_File_Name (I);
            Replay_File_Id   : File_Type;
         begin

            Utility.Log.Put_Message ("trying " & (-Replay_File_Name));

            if Ada.Directories.Exists (-Replay_File_Name) then

               Replay_Register (I).Name := Replay_File_Name;

               Open (Replay_File_Id, In_File, -Replay_File_Name);

               if not End_Of_File (Replay_File_Id) then

                  Reader.Load (Get_Line (Replay_File_Id));

                  declare
                     Key   : String := Reader.Read_Next ('=');
                     Value : String := Reader.Read_Next ('=');
                  begin

                     if Key = "#METADATA" then

                        Replay_Register (I).From_Metadata (Value);

                        Utility.Log.Put_Message (Replay_Range'Image (I) & ">" & Trim (Value));

                     end if;

                  end;

               end if;

               Close (Replay_File_Id);

            end if;

         end;

      end loop;

      -- Load the default selected stream source from the startup file
      --------------------------------------------------------------------------
      if Ada.Directories.Exists (File_Name) then

         Open (File, In_File, File_Name);

         while not End_Of_File (File) loop

            Reader.Load (Get_Line (File));

            if Reader.Current /= '#' then

               declare
                  Key   : String := Reader.Read_Next ('=');
                  Value : String := Reader.Read_Next ('=');
               begin

                  if Key = "LOCATION" then

                     Default_Position := Maps.Value (Value);

                     Flight.Data.Position := Default_Position;

                  elsif Key = "MAP" then

                     Default_Map := +Value;

                  elsif Key = "CHANNEL1" then

                     Parse_Channel (1, Value);

                  elsif Key = "CHANNEL2" then

                     Parse_Channel (2, Value);

                  elsif Key = "CHANNEL3" then

                     Parse_Channel (3, Value);

                  elsif Key = "CHANNEL4" then

                     Parse_Channel (4, Value);

                  elsif Key = "CHANNEL5" then

                     Parse_Channel (5, Value);

                  elsif Key = "UBLOX_AIRBORNE" then

                     Serial_Ublox := True;

                  end if;

               end;

            end if;

         end loop;

         Close (File);

      end if;

      -- Setup serial reconnection
      --------------------------------------------------------------------------
      Serial_Reconnect := Timing.Events.Register_Timer (Timer    => 4.0,
                                                        Callback => Reconnect_Serial'Access);

      -- Setup the buffer and data collection
      --------------------------------------------------------------------------
      Store.Buffer := Data'Unrestricted_Access;

      Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                    Callback => Collect_Data'Access);

      -- Setup the replay data flushing
      --------------------------------------------------------------------------
      Timing.Events.Register_Timer (Timer    => 5.0,
                                    Callback => Flush_Recording'Access);

      -- Start the traffic stack managment
      --------------------------------------------------------------------------
      Flight.Traffic.Init;

      -- Setup a timer to obtain the average data rate
      --------------------------------------------------------------------------
      Timing.Events.Register_Timer (Timer    => 5.0,
                                    Callback => Calculate_Rate'Access);

      -- Activate channel 1 by default
      --------------------------------------------------------------------------
      Configure_Channel (Channel);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Finalize is
   begin

      Utility.Log.Put_Message ("closing the data streams");

      Stream_Active := False;

      if Is_Open (Replay_File) then

         if Record_Messages then

            Put_Line (Replay_File, "#END");
            Reset    (Replay_File);
            Put_Line ("#METADATA=" & Replay_Data.To_Metadata);

         end if;

         Close (Replay_File);

      end if;

      case Active_Source is

         when Source_File =>

            null;

         when Source_Socket =>

            if Socket_Id /= No_Socket then

               Close_Socket (Socket_Id);

            end if;

         when Source_Serial =>

            Close (Serial_Id);

         when Source_Interface =>

            null;

         when Source_None =>

            null;

      end case;

      Data_Rate       := 0;
      Message_Counter := 0;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Resets the data
   --===========================================================================
   procedure Reset_Data is
   begin

      Flight.History  := (others => No_Flight_Data);
      Flight.Data     := No_Flight_Data;
      Flight.Current  := History_Range'First;
      Flight.Previous := No_Flight_Data;
      Flight.Resync_Data_And_Time;

      On_Data_Reset.Trigger;

   end Reset_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Configure_Channel (Index : Channel_Range) is
   begin

      Channel := Index;

      Finalize;

      Reset_Data;

      case Channels (Index).Kind is

         when Source_Serial =>

            Setup_Serial (Channels (Index).Port,
                          Channels (Index).Prot);

         when Source_Socket =>

            Setup_Socket (Channels (Index).Port,
                          Channels (Index).Prot);

         when others =>

            null;

      end case ;

   end Configure_Channel;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Configure_Replay (Index : Replay_Range) is
   begin

      Finalize;

      Reset_Data;

      Replay_Data  := Replay_Register (Index)'access;

      Replay_Index := Index;

      Setup_Replay;

   end Configure_Replay;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Is_Active return Boolean is
   begin

      return Stream_Active;

   end Is_Active;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Replay_Mode return Boolean is
   begin

      return Active_Source = Source_File and Is_Open (Replay_File);

   end Replay_Mode;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Recording return Boolean is
   begin

      return Record_Messages;

   end Recording;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Decodes a generic data message
   --===========================================================================
   procedure Process_Message (Messages : String) is
   begin

      Flight.Data.Timestamp := Cached_Time;

      case Active_Protocol is

         when Protocol_Gnav =>

            Parse_Gnav_Message (Messages);

         when Protocol_Nmea =>

            Parse_Nmea_Message (Messages);

      end case;

      if Record_Messages then

         Put_Line (Replay_File, Messages);

      end if;

   end Process_Message; pragma Inline (Process_Message);
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Proceses the next valid line on the replay file
   --===========================================================================
   function Read_Replay_Step return Boolean is

      Line : String := Get_Line (Replay_File);

   begin

      if End_Of_File (Replay_File) then

         -- Reset the flight history and stop caching until receiving new data
         -----------------------------------------------------------------------

         Reset (Replay_File);

         Flight.Clear_History;

         Replay_Active := False;

         On_Data_Reset.Trigger;

         Utility.Log.Put_Message ("replay reset");

      end if;

      if Line'Length = 0 then

         return True;

      else

         if Line (1) = '#' then

            return False;

         else

            Process_Message (Line);

            Replay_Active := True;

            return True;

         end if;

      end if;

   end Read_Replay_Step;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Collect_Data is

      use Utility.Strings;

   begin

      Cached_Time := Clock;

      for F in Data_Field_Kind loop

         Flight.Data.Origin (F) := Origin_None;

      end loop;

      case Active_Source is

         when Source_File =>

            -- Case A: Replay data from file
            --------------------------------------------------------------------
            if Is_Open (Replay_File) then

               Utility.Log.Put_Message ("reading step");

               while Read_Replay_Step loop

                  null;

               end loop;

            end if;

            -- Only cache when the replay is active
            ---------------------------------------
            if Replay_Active then

               Flight.Cache_Data;

            end if;

         when Source_Socket =>

            -- Case B: Online socket data (from a simulator or middleware)
            --------------------------------------------------------------------

            loop

               Last := 0;

               Store.Move_Cursor (Data'First);

               begin
                  Receive_Socket (Socket_Id, Data, Last, From);
               exception
                  when E : Socket_Error =>
                     null;
                  when E : others =>
                     Utility.Log.Put_Message (E, "while reading from socket");
               end;

               if Last in Data'Range then

                  declare
                     Messages : String (1.. Positive (Last));
                  begin
                     String'Read (Store, Messages);
                     Process_Message (Messages);
                  end;

               else

                  Utility.Log.Put_Message ("warning: socket input is too large");

                  exit;

               end if;

            end loop;

            -- Load the data on the cache
            -------------------------------------
            Flight.Cache_Data;

         when Source_Serial =>

            -- Case C: serial streaming
            --------------------------------------------------------------------

            Last := 0;

            Store.Move_Cursor (Data'First);

            begin
               Read (Serial_Id, Data, Last);
            exception
               when E : Constraint_Error =>
                  Utility.Log.Put_Message (E, "error while reading from serial");
               when E : others =>
                  Serial_Reconnect.Resume;
            end;

            if Last in Data'Range then

               declare
                  Messages : String (1.. Positive (Last));
               begin
                  String'Read (Store, Messages);
                  Process_Message (Messages);
               end;

            end if;

            -- Load the data on the cache
            -------------------------------------
            Flight.Cache_Data;

         when Source_Interface =>

            null;

         when Source_None =>

            null;

      end case;

      -- Indicate end of recording step
      -------------------------------------
      if Record_Messages then

         Put_Line (Replay_File, "#");

      end if;

   exception

      when E : others =>

         Utility.Log.Put_Message ("error while aquiring data: "& Ada.Exceptions.Exception_Message (E));

   end Collect_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Data (Lat, Lon : Long_Float; Alt, Spd, Brg : Float) is
   begin

      if Active_Source = Source_Interface then

         Cached_Time := Clock;

         for F in Data_Field_Kind loop

            Flight.Data.Origin (F) := Origin_None;

         end loop;

         Flight.Data.Timestamp := Cached_Time;

         Flight.Data.Origin (Field_Position) := Origin_External;

         Flight.Data.Ages   (Field_Position) := Cached_Time;

         Flight.Data.Position.Lat := Lat;

         Flight.Data.Position.Lon := Lon;

         --

         Flight.Data.Origin (Field_Altitude) := Origin_External;

         Flight.Data.Ages   (Field_Altitude) := Cached_Time;

         Flight.Data.Altitude := Alt;

         --

         Flight.Data.Origin (Field_Speed) := Origin_External;

         Flight.Data.Ages   (Field_Speed) := Cached_Time;

         Flight.Data.Speed := Spd;

         --

         Flight.Data.Origin (Field_Course) := Origin_External;

         Flight.Data.Ages   (Field_Course) := Cached_Time;

         Flight.Data.Course := Brg;

         -- Indicate end of recording step
         -------------------------------------
         if Record_Messages then

            -- TODO: write G-NAV message

            Put_Line (Replay_File, "#");

         end if;

      end if;

   end Set_Data;
   -----------------------------------------------------------------------------

end Flight.Stream;
--------------------------------------------------------------------------------
