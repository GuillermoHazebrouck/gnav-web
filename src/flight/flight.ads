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
                                                
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The age of the data items
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Field_Kind is (Field_Position,     
                            Field_Altitude, 
                            Field_Elevation,
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
   type Data_Origin_Kind is (Origin_None,
                             Origin_External,
                             Origin_Internal);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Data_Origin_Array is array (Data_Field_Kind) of Data_Origin_Kind;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains the instantaneus flight variables
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
   type Flight_Data_Record is tagged record
   
      Timestamp : Times;             -- The dataframe timestamp
      
      Position  : Position_Record;   -- The position from the GPS
   
      Speed     : Float;             -- The ground speed
   
      Step      : Float;             -- The distance to the previous step
      
      Airspeed  : Float;             -- The indicated airspeed (km/h)
   
      Altitude  : Float;             -- The gps altitude above MSL (m)
    
      Elevation : Float;             -- The altitude above the ground
      
      Distance  : Float;             -- The cumulated horizontal distance
      
      Heading   : Float;             -- The true heading (direction of the nose, deg)
      
      Course    : Float;             -- The path course (relative to the north, deg)
      
      Turn      : Float;             -- The turn rate (deg/s)
      
      Blow      : Vector2_Record;    -- The instantaneus wind blow vector (m/s)
   
      Wind      : Vector2_Record;    -- The mean wind vector (m/s)
   
      Ages      : Data_Age_Array;    -- The data ages
      
      Origin    : Data_Origin_Array; -- The data origin
      
   end record;
   
   --===========================================================================
   -- Indicates how old the data is in relation to now
   -- NOTE: No_Age indicates that the data is not valid.
   --===========================================================================
   function Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Lapses;
   
   --===========================================================================
   -- Indicates how old the data is in relation to the original update
   -- NOTE: No_Age indicates that the data is not valid.
   --===========================================================================
   function Relative_Age (This : Flight_Data_Record; Field : Data_Field_Kind) return Lapses;
   
   --===========================================================================
   -- Indicates if the data was an new update
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
   -- Sets the timestamp of the data field to the currently cached time
   --===========================================================================
   procedure Cache_Timestamp (This : in out Flight_Data_Record; Field : Data_Field_Kind);
   
   --===========================================================================
   -- Returns the previous state
   --===========================================================================
   function Get_Previous return Flight_Data_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default flight data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   No_Flight_Data : constant Flight_Data_Record := (Timestamp => No_Time,
                                                    Position  => No_Position_Record,
                                                    Speed     => 0.0,
                                                    Step      => 0.0,
                                                    Airspeed  => 0.0,
                                                    Altitude  => No_Altitude,
                                                    Elevation => 0.0,
                                                    Distance  => 0.0,
                                                    Heading   => 0.0,
                                                    Course    => 0.0,
                                                    Turn      => 0.0,
                                                    Blow      => No_Vector2_Record,
                                                    Wind      => No_Vector2_Record,
                                                    Ages      => (others => No_Time),
                                                    Origin    => (others => Origin_None));
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The most recent flight data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   Data : Flight_Data_Record := No_Flight_Data;
   
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
   -- Computes the rate of turn (deg/s) using recent data.
   --===========================================================================
   procedure Compute_Turn_Rate;
   
   --===========================================================================
   -- Indicates if the path is almost straight
   --===========================================================================
   function Straight_Flight return Boolean;
   
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
   On_Data_Cached : Utility.Events.Event_Stack;
   
   --===========================================================================
   -- Event triggered when the data is cleared
   --===========================================================================
   On_Data_Cleared : Utility.Events.Event_Stack;
   
   --===========================================================================
   -- Occurs when the replay is reset
   --===========================================================================
   On_Data_Reset : Utility.Events.Event_Stack;
   
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
   
private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The range of history dataframes
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type History_Range is new Positive range 1..600; 
   
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
   -- A counter to count the number of acquired messagess.
   -- NOTE: the counter is reset at regular intervals by Flight.Stream.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Message_Counter : Natural := 0;
   
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
   
end Flight;
--------------------------------------------------------------------------------
