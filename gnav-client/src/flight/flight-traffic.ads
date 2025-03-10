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
with Utility.Calendar;
use  Utility.Calendar;


--//////////////////////////////////////////////////////////////////////////////
-- This the package contains the surrounding traffic.
--//////////////////////////////////////////////////////////////////////////////
package Flight.Traffic is
 
   --===========================================================================
   -- Connects the timed signal that maintains the tracks
   --===========================================================================
   procedure Initialize;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Save_Configuration;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- This open variable can be used to control the emission of peridic requests
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   Enabled : Boolean := False;
    
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the local position should be registered
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   Follow : Boolean := False;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Aprs_Status is (Aprs_Disabled,      -- OFF    -> disabled by the system or the user
                        Aprs_Not_Receiving, -- RED    -> not getting anything back (broken uplink)
                        Aprs_Not_Reporting, -- YELLOW -> getting none or old data (but uplink up'n running)
                        Aprs_Nominal);      -- GREEN  -> getting recent tracks back
   
   --===========================================================================
   -- Indicates the status of the APRS
   --===========================================================================
   function Get_Status return Aprs_Status;
   
   --===========================================================================
   -- The data that will be downlinked for the current state
   --===========================================================================
   function Get_Aprs_Data return String;
   
   --===========================================================================
   -- > Marks the tracks older than 6 seconds as coasted.
   -- > Disables the tracks that are obsolete.
   --===========================================================================
   procedure Maintain_Tracks;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Traffic_Id is String (1..6);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Traffic_Id : constant Traffic_Id := (others => ' ');
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Different kind of traffic (based on FLARM ICD)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Traffic_Type is (Uknonwn,
                         Glider,
                         Tow_Plane,
                         Rotorcraft,
                         Skydiver,
                         Drop_Plane,
                         Hang_Glider,
                         Paraglider,
                         Prop,
                         Turbo_Jet,
                         Ballon,
                         Airship,
                         Unmanned,
                         Obstacle);                         

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents a traffic object.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Traffic_Record is tagged record
   
      Active        : Boolean;
      
      Time_Stamp    : Times;
      
      Last_Integral : Times;
      
      Id            : Natural;
      
      Position      : Position_Record;

      Altitude      : Natural;
      
      Speed         : Float;
      
      Vario         : Float;
      
      Course        : Float;
      
      Rotation      : Float;
      
      No_Track      : Boolean;
      
      Coasted       : Boolean;
      
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The default value
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
   No_Traffic_Record : constant Traffic_Record := (Active        => False,      
                                                   Time_Stamp    => No_Time,      
                                                   Last_Integral => No_Time,  
                                                   Id            => 0,      
                                                   Position      => No_Position_Record, 
                                                   Altitude      => 0,      
                                                   Speed         => 0.0,      
                                                   Vario         => 0.0,      
                                                   Course        => 0.0, 
                                                   Rotation      => 0.0,
                                                   No_Track      => False,
                                                   Coasted       => False);
        
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Contains the traffic objects
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Traffic_Data : array (1..100) of Traffic_Record := (others => No_Traffic_Record);
      
end Flight.Traffic;
--------------------------------------------------------------------------------
