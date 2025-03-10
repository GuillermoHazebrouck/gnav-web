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
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;
with Ada.Calendar;
use  Ada.Calendar;
with Ada.Streams;
use  Ada.Streams;
-- Gnav
with Utility.Maps;
use  Utility.Maps;

--//////////////////////////////////////////////////////////////////////////////
-- This special program is able to parse a collection of METAR messages to
-- generate a meteo stations message.
--//////////////////////////////////////////////////////////////////////////////
package Meteo is

   --===========================================================================
   -- Starts fetching meteo information
   --===========================================================================
   procedure Start_Updating;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A metar station
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Meteo_Station_Record is tagged private;

   No_Meteo_Station_Record : constant Meteo_Station_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The range of meteo stations
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Station_Range is range 1..15;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A metar station
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Meteo_Station_Array is array (Station_Range) of Meteo_Station_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The stack of meteo stations, accessible from multiple tasks
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected type Meteo_Station_Stack is

      --========================================================================
      -- Configures the stations using the initialization file
      --========================================================================
      procedure Setup_Stations;

      --========================================================================
      -- Updates the stations when necessary
      --========================================================================
      procedure Update_Stations;

      --========================================================================
      -- Returns a stream containing all meteo stations
      --========================================================================
      function Get_Stations return Stream_Element_Array;

   private

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- The meteo stations
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Stations : Meteo_Station_Array := (others => No_Meteo_Station_Record);

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- The stream containing the compiled data processed during updates
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Buffer : aliased Stream_Element_Array (1..2_000);

      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      -- The last position of the cursor in the buffer
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Cursor : Stream_Element_Offset := 0;

   end Meteo_Station_Stack;

   --===========================================================================
   -- Returns the track stack
   --===========================================================================
   function Get_Stack return not null access Meteo_Station_Stack;

private

   subtype Metar_Name is String (1..4);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A metar station
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Meteo_Station_Record is tagged record

      Loaded        : Boolean;

      Active        : Boolean;

      Url           : Unbounded_String;

      Date          : Time;

      Next_Update   : Time;

      Name          : Metar_Name;

      Position      : Position_Record;

      Elevation     : Natural;

      Interval      : Duration;

      Qnh           : Natural;

      Wind_Speed    : Natural;

      Wind_Course   : Natural;

      Temperature   : Integer;

      Dew_Point     : Integer;

      Cloud_Base    : Natural;

      Precipitation : Natural;

      Visibility    : Natural;

   end record;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   procedure Parse_Message (This : in out Meteo_Station_Record; Message : String);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Meteo_Station_Record : constant Meteo_Station_Record := (Loaded        => False,
                                                               Active        => False,
                                                               Url           => Null_Unbounded_String,
                                                               Date          => Clock,
                                                               Next_Update   => Clock,
                                                               Name          => "ZZZZ",
                                                               Position      => No_Position_Record,
                                                               Elevation     => 0,
                                                               Interval      => 3605.0,
                                                               Qnh           => 1013,
                                                               Wind_Speed    => 0,
                                                               Wind_Course   => 0,
                                                               Temperature   => 0,
                                                               Dew_Point     => 0,
                                                               Cloud_Base    => 0,
                                                               Precipitation => 0,
                                                               Visibility    => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The stack of tracks
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stack : aliased Meteo_Station_Stack;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   task Maintain_Stations is

      entry Start;

   end Maintain_Stations;

end Meteo;
--------------------------------------------------------------------------------
