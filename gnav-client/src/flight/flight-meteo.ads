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
with Maps;
use  Maps;
with Utility.Calendar;
use  Utility.Calendar;

--//////////////////////////////////////////////////////////////////////////////
-- Provides meteo information for different METAR stations
--//////////////////////////////////////////////////////////////////////////////
package Flight.Meteo is

   subtype Metar_Name is String (1..4);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A metar station
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Meteo_Station_Record is record

      Loaded      : Boolean;

      Date        : Times;

      Name        : Metar_Name;

      Position    : Position_Record;

      Qnh         : Natural;

      Wind_Speed  : Natural;

      Wind_Course : Natural;

      Temperature : Integer;

      Dew_Point   : Integer;

      Cloud_Base  : Natural;

      Visibility  : Natural;

   end record;
   -----------------------------------------------------------------------------

   --===========================================================================
   -- Returns the station closest to the current position
   --===========================================================================
   function Get_Local_Station return not null access Meteo_Station_Record;

   --===========================================================================
   -- Returns the number of active stations
   --===========================================================================
   function Get_Number_Of_Stations return Natural;

   --===========================================================================
   -- Configures the initial data, timers and requests
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Indicates if the data is the most recent one
   --===========================================================================
   function Updated return Boolean;

private

   No_Meteo_Station_Record : constant Meteo_Station_Record := (Loaded      => False,
                                                               Date        => No_Time,
                                                               Name        => "ZZZZ",
                                                               Position    => No_Position_Record,
                                                               Qnh         => 1013,
                                                               Wind_Speed  => 0,
                                                               Wind_Course => 0,
                                                               Temperature => 0,
                                                               Dew_Point   => 0,
                                                               Cloud_Base  => 0,
                                                               Visibility  => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Station_Range is range 1..15;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stations : array (Station_Range) of aliased Meteo_Station_Record := (others => No_Meteo_Station_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Local_Station : Station_Range := Station_Range'First;

end Flight.Meteo;
--------------------------------------------------------------------------------
