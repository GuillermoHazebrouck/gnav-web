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
-- Gnav
with Math.Vector2;
use  Math.Vector2;
with Maps;
use  Maps;
with Utility.Calendar;
use  Utility.Calendar;
with Utility.Events;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Flight is
               
   --///////////////////////////////////////////////////////////////////////////
   -- Flight data
   --///////////////////////////////////////////////////////////////////////////
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kinds of flight modes
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Flight_Mode_Kinds is (Mode_Straight,  -- The turn rate is kept minimal
                              Mode_Circling,  -- At least 360 degrees turning in the same direction
                              Mode_Manouver); -- The turn rate is moderate to high, and not circling
                            
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The age of the data items
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Field_Kind is (Field_Position,
                            Field_Altitude,                            
                            Field_Level,
                            Field_Elevation,
                            Field_Vario,
                            Field_Speed,
                            Field_Airspeed,
                            Field_Course,
                            Field_Heading,
                            Field_Turn,
                            Field_Blow,
                            Field_Wind);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Age_Array is array (Data_Field_Kind) of Times;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   -- Indicates the origin of the data. Data without origin is only anecdotical.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Update_Kind is (Update_None,
                             Update_External,
                             Update_Internal);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Update_Array is array (Data_Field_Kind) of Data_Update_Kind;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The range of history dataframes (up to about 5 minutes)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type History_Range is new Positive range 1..300; 
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains the instantaneus flight variables
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Flight_Data_Record is tagged record
   
      Timestamp : Times;             -- The dataframe timestamp
      
      Mode      : Flight_Mode_Kinds; -- The detected flight mode
      
      Position  : Position_Record;   -- The position from the GPS (deg)
   
      Speed     : Float;             -- The ground speed (m/s)
   
      Step      : Float;             -- The distance to the previous step (m)
      
      Airspeed  : Float;             -- The indicated airspeed (km/h)
   
      Altitude  : Float;             -- The raw altitude above MSL (m)
             
      Level     : Float;             -- The smoothed altitude above MSL (m)
    
      Elevation : Float;             -- The altitude above the ground (m)

      Vario     : Float;             -- The vertical speed (m/s)
      
      Distance  : Float;             -- The cumulated horizontal distance (km)
      
      Heading   : Float;             -- The true heading (direction of the nose, deg)
      
      Course    : Float;             -- The path course (relative to the north, deg)
      
      Turn      : Float;             -- The turn rate (deg/s)
      
      Blow      : Vector2_Record;    -- The instantaneus wind blow vector (m/s)
   
      Wind      : Vector2_Record;    -- The mean wind vector (m/s)
   
      Ages      : Data_Age_Array;    -- The data ages (the data birthdate)
      
      Origin    : Data_Update_Array; -- The data origin (the update source)
      
   end record;
   
   --===========================================================================
   -- Indicates how old the data is in relation to now
   -- NOTE: No_Age indicates that the data is not valid. Also, the lapse can be
   -- ahead in time, since the cached time has a different update rate.
   --===========================================================================
   function Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Lapses;
   
   --===========================================================================
   -- Indicates how old the data is in relation to the original update
   -- NOTE: No_Age indicates that the data is not valid.
   --===========================================================================
   function Relative_Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Lapses;
   
   --===========================================================================
   -- Indicates if the data was set as an update. If not, it is just being
   -- carried from the past.
   --===========================================================================
   function Is_Update (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean;
      
   --===========================================================================
   -- Indicates if the data was collected less than 2 seconds ago
   --===========================================================================
   function Is_Recent (This : Flight_Data_Record; Field : Data_Field_Kind; Lapse : Float := 2.0) return Boolean;
      
   --===========================================================================
   -- Indicates if the data has been set
   --===========================================================================
   function Is_Valid (This : Flight_Data_Record; Field : Data_Field_Kind) return Boolean;
   
   --===========================================================================
   -- Returns an image of the data for the automatic position reporting system
   -- NOTE: this is only compatible with this IGC record: I023638GSP3941HDT
   --===========================================================================
   function Get_Igc_Image (This : Flight_Data_Record) return String;
   
   --===========================================================================
   -- Returns the data at a given history point (1 is the current point)
   --===========================================================================
   function Get_History (H : History_Range) return Flight_Data_Record;
   
   --===========================================================================
   -- Returns the previous state
   --===========================================================================
   function Get_Previous return Flight_Data_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default flight data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   No_Flight_Data : constant Flight_Data_Record := (Timestamp => No_Time,
                                                    Mode      => Mode_Straight,
                                                    Position  => No_Position_Record,
                                                    Speed     => 0.0,
                                                    Step      => 0.0,
                                                    Airspeed  => 0.0,
                                                    Altitude  => No_Altitude,
                                                    Level     => No_Altitude,
                                                    Elevation => 0.0,
                                                    Vario     => 0.0,
                                                    Distance  => 0.0,
                                                    Heading   => 0.0,
                                                    Course    => 0.0,
                                                    Turn      => 0.0,
                                                    Blow      => No_Vector2_Record,
                                                    Wind      => No_Vector2_Record,
                                                    Ages      => (others => No_Time),
                                                    Origin    => (others => Update_None));
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The most recent flight data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   Data : Flight_Data_Record := No_Flight_Data;
   
   --===========================================================================
   -- Completes the current data using the updated values
   --===========================================================================
   procedure Complete_Data;
   
   --===========================================================================
   -- Loads the current position to the circular buffer.
   -- This methiod will trigger On_Data_Cached.
   --===========================================================================
   procedure Cache_Data;
   
   --===========================================================================
   -- Clears the whole history
   --===========================================================================
   procedure Clear_History;

   --===========================================================================
   -- Returns the variation of the variable in the last three steps
   --===========================================================================
   function Variation (Field : Data_Field_Kind) return Float;
   
   --===========================================================================
   -- Returns the last steps
   --===========================================================================
   function Step (Field : Data_Field_Kind) return Float;
   
   --===========================================================================
   -- Event triggered when the data is cached
   --===========================================================================
   On_Data_Cached : Utility.Events.Event_Stack (4);
   
   --===========================================================================
   -- Event triggered when the data is cleared
   --===========================================================================
   On_Data_Cleared : Utility.Events.Event_Stack (4);
   
   --===========================================================================
   -- Occurs when the replay is reset
   --===========================================================================
   On_Data_Reset : Utility.Events.Event_Stack (4);
 
   --///////////////////////////////////////////////////////////////////////////
   -- Other functions
   --///////////////////////////////////////////////////////////////////////////
   
   --===========================================================================
   --
   --===========================================================================
   function Get_Local_Time return Times;
   
   --===========================================================================
   -- Gets the actual UTC time
   --===========================================================================
   function Get_Utc_Time return Times;
   
   --===========================================================================
   -- Indicates if the time (seconds since 00:00) is synchronized with GNSS
   --===========================================================================
   function Time_Synchronized return Boolean;
   
   --===========================================================================
   -- Indicates if the date (day/month/year) is synchronized with GNSS
   --===========================================================================
   function Date_Synchronized return Boolean;
   
   --===========================================================================
   -- Gets the UTC time
   --===========================================================================
   procedure Set_Time_Zone (Zone : Integer);
   
   --===========================================================================
   -- Gets the UTC time
   --===========================================================================
   function Get_Time_Zone return Integer;
   
   --===========================================================================
   -- Forces date and time synchronization
   --===========================================================================
   procedure Resync_Data_And_Time;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The safety height used to emit range warnings
   -- NOTE: when changed, the data will be adjusted the next regular update
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Safety_Height : Float := 250.0;
   
private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   History : array (History_Range) of Flight_Data_Record := (others => No_Flight_Data);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Current  : History_Range := History_Range'First;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Previous : Flight_Data_Record := No_Flight_Data;
    
   --===========================================================================
   -- Returns the previous index in the circular history buffer
   --===========================================================================
   procedure Get_Previous_Index (Index : in out History_Range);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last time that Clock_Offset was updated
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Time_Sync : Times := Cached_Time;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last time that Clock_Offset was updated
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Date_Sync : Times := Cached_Time;
 
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The time offset (between Clock and GPS, used to correct displayed time)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Time_Offset : Lapses := No_Lapse;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The time offset (between Clock and GPS, used to correct displayed time)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Date_Offset : Lapses := No_Lapse;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The local UTC offset (time zone) E.g.: (UTC+1 > 3600s)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Utc_Offset  : Lapses := No_Lapse;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Maximum lapse between measurements to evaluate the average elevation
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Level_Max_Lapse : constant Float := 3.5;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Maximum lapse between measurements to evaluate the average turn rate
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Turn_Rate_Max_Lapse : constant Float := 4.5;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Lower thershold to enter straight mode (deg/s)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Min_Turn_Straight : constant Float := 2.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Upper thershold to leave straight mode (deg/s)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Max_Turn_Straight : constant Float := 6.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Maximum lapse between measurements to evaluate the average vertical speed
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Vario_Max_Lapse : constant Float := 4.0;
 
end Flight;
--------------------------------------------------------------------------------
