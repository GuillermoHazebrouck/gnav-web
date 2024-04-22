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
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
-- This packages is where the raw flight data is collected.
--
-- SETUP
--------------------------------------------
-- There are four data sources:
--
-- FILE      -> used to replay recorded data in the "replay" folder
-- UDP       -> used primarily to connect a flight simulator or an on-board slave device
-- SERIAL    -> used to link a primary data acquisition system via serial port (UART).
-- INTERFACE -> used to load data from a different source using Set_Data.
--
-- Since version V02A it is possible to change the stream source during
-- opperations. This is done via the "Configure_..." procedures.
--
-- The source is intially set by "Init" procedure using the "setup.dat" file
-- located in the base directory. The configuration for each stream source is:
--
-- FILE   -> FILE_STREAM=<name of file located in the replay/ directory>
-- UDP    -> UDP_STREAM=<port number>
-- SERIAL -> SERIAL_STREAM=<serial port name (default is "/dev/ttyACM0")>
--
-- DATA FORMATS
--------------------------------------------
-- The data can be on two formats:
-- GNAV       -> comma separated set of named variables, like "X=10.0,Y=5.2,A=200,..."
--               This format is very suitable for linking a flight simulator.
-- NMEA/FLARM -> NMEA and FLARM standard
--
-- For non-replay streaming, this must be specified in the setup file.
--
-- DATA RECORDING
--------------------------------------------
-- The recording is automatically initiated from the moment there is a valid fix
-- and only when the stream sources are UDP or Serial.
-- The recording files are named replay/RECNN.DAT, where NN goes from 00 to 99,
-- this to prevent overloading the system memory. If 100 have been recorded, the
-- last replay is saved as RECXX.DAT and overriden.
--
--//////////////////////////////////////////////////////////////////////////////
package Flight.Stream is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kinds of data aquisition protocols
   -- NOTE: NMEA protocol includes FLARM
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Protocol_Kind is (Protocol_Gnav,
                          Protocol_Nmea);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kinds of data sources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Source_Kind is (Source_None,
                        Source_File,
                        Source_Socket,
                        Source_Serial,
                        Source_Interface);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kinds of data sources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Channel_Range is new Positive range 1..5;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Replay_Range is new Natural range 0..99;

   --===========================================================================
   -- Opens the data stream as specified on the command line arguments
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Closes all data streams
   --===========================================================================
   procedure Finalize;

   --===========================================================================
   -- Indicates if the data streaming is running
   --===========================================================================
   function Is_Active return Boolean;

   --===========================================================================
   -- Returns the data rate (average number of packets received per minute)
   --===========================================================================
   function Get_Rate return Natural;

   --===========================================================================
   -- Returns the active stream kind
   --===========================================================================
   function Get_Source_Kind return Source_Kind;

   --===========================================================================
   -- Returns the active protocol kind
   --===========================================================================
   function Get_Protocol_Kind return Protocol_Kind;

   --===========================================================================
   -- Returns the string representation for a given protocol kind
   --===========================================================================
   function Get_Protocol_Image (Value : Protocol_Kind) return String;

   --===========================================================================
   -- Returns the active port name
   --===========================================================================
   function Get_Port return String;

   --===========================================================================
   -- Returns the active channel
   --===========================================================================
   function Get_Channel_Index return Channel_Range;

   --===========================================================================
   -- Returns the name of the Ith available channel
   --===========================================================================
   function Get_Channel_Name (Index : Channel_Range) return String;

   --===========================================================================
   -- When the interface streaming is used, this method accepts external data
   --===========================================================================
   procedure Set_Data (Lat, Lon : Long_Float; Alt, Spd, Brg : Float);

   --===========================================================================
   -- Indicates if there is an active recording
   --===========================================================================
   function Recording return Boolean;

   --===========================================================================
   -- Indicates if the stream is a replay
   --===========================================================================
   function Is_Replay return Boolean;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Provides information about a recording file
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Replay_Record is tagged record

      Name     : Dynamic_String;

      Map      : Dynamic_String;

      Protocol : Protocol_Kind;

      Start    : Time;

      Span     : Duration;

      Distance : Natural;

      Altitude : Natural;

   end record;

   --===========================================================================
   --
   --===========================================================================
   function Get_Replay_Index return Replay_Range;

   --===========================================================================
   -- Returns the replay metadata (time, date, distance, altitudes, etc) and
   -- index of the next replay file closest to the given index.
   --===========================================================================
   function Get_Replay_Info (Index : Replay_Range) return Replay_Record;

   --===========================================================================
   -- Indicates if the given recording is valid
   --===========================================================================
   function Is_Valid (Index : Replay_Range) return Boolean;

   --===========================================================================
   -- Changes the active stream kind to file mode (replay)
   --===========================================================================
   procedure Configure_Replay (Index : Replay_Range);

   --===========================================================================
   -- Changes the active stream kind to file mode (replay)
   --===========================================================================
   procedure Configure_Channel (Index : Channel_Range);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Replay file metadata
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Replay_Info  is String (1..100);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default matadata
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Replay_Info : constant Replay_Info := (others => ' ');

   --===========================================================================
   -- Converts the info to metadata
   --===========================================================================
   function To_Metadata (This : Replay_Record) return Replay_Info;

   --===========================================================================
   -- Generates the info from metadata
   --===========================================================================
   procedure From_Metadata (This : in out Replay_Record; Data : String);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Replay_Array is array (Replay_Range) of aliased Replay_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Replay_Record : constant Replay_Record := (Name     => Empty_String,
                                                 Map      => Empty_String,
                                                 Protocol => Protocol_Gnav,
                                                 Start    => No_Time,
                                                 Span     => 0.0,
                                                 Distance => 0,
                                                 Altitude => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of registered replay files
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Replay_Register : Replay_Array := (others => No_Replay_Record);

   --===========================================================================
   -- Collects the pending data on any of the streams
   --===========================================================================
   procedure Collect_Data;

end Flight.Stream;
--------------------------------------------------------------------------------
