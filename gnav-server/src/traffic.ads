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
use  Ada.Calendar;
with Ada.Streams;
use  Ada.Streams;
-- Gnav
with Users;
use  Users;
with Utility.Maps;
use  Utility.Maps;
with Utility.Ids;
use  Utility.Ids;
with Utility.Types;
use  Utility.Types;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides tracks within the configure region
--//////////////////////////////////////////////////////////////////////////////
package Traffic is
   
   --===========================================================================
   -- Configures the tracker from the command line arguments
   --===========================================================================
   procedure Setup_Tracker;
    
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Track_Record is tagged private;
   
   --===========================================================================
   -- Returns an IGC image of the data
   --===========================================================================
   function Get_Igc_Image (This : Track_Record) return String;
   
   No_Track : constant Track_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stack_Size : constant Natural := 250;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Track_Array is array (1..Stack_Size) of Track_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The stack of tracks, accessible from multiple tasks
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   protected type Track_Stack is
      
      --========================================================================
      -- Adds a track to the stack
      --========================================================================
      procedure Add_Track (New_Track : Track_Record);
      
      --========================================================================
      -- Incorporates the user to the tracks (fusion between the OGN and G-NAV)
      --========================================================================
      procedure Link_User (User : User_Record);
      
      --========================================================================
      -- Returns a stream containing all tracks from the stack
      --========================================================================
      function Get_Tracks (User    : User_Record;
                           Send_Id : Boolean := False) return Stream_Element_Array;
      
      --========================================================================
      -- Writes the tracks to a file
      --========================================================================
      procedure Write_Tracks (File_Name : String);
      
      --========================================================================
      -- Removes old tracks from the stack
      --========================================================================
      procedure Clean_Tracks;
      
      --========================================================================
      -- Returns the number of valid tracks
      --========================================================================
      function Count_Tracks return Natural;
      
   private
        
      Tracks : Track_Array := (others => No_Track);
      
   end;
   -----------------------------------------------------------------------------

   --===========================================================================
   -- Returns the track stack
   --===========================================================================
   function Get_Stack return not null access Track_Stack;
   
private
   
   --###########################################################################
   -- Tracking of local FLARM devices
   --###########################################################################
                  
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A local device to log
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Device_Record is record
      
      Id   : Natural;
      
      Name : Track_Names;
      
      Mark : Track_Marks;
      
   end record;
   
   No_Device : constant Device_Record := (Id   => 0,
                                          Name => No_Callsign, 
                                          Mark => No_Mark);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- This is filled at load time and remains invariant
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Local_Devices : array (1..15) of aliased Device_Record := (others => No_Device);
   
   --###########################################################################
   -- Stack
   --###########################################################################
             
   type Track_Source_Kind is (Source_None, Source_Ogn, Source_Gnav);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The track as represented in the binary file. The id is only for
   -- local use, so there are 14 bytes in total
   -- Tracks can live maximum 127 seconds from block timestamp
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Track_Record is tagged record
      
      -- Workspace variables (not encoded in the stream)
      Timestamp : Time;                 -- The reception time
      Time      : Natural;              -- The age in seconds since 00:00:00 UTC
      Position  : Position_Record;      -- The last position
      User      : User_Record;          -- The coupled user data
      Device    : access Device_Record; -- The reference to a local device
      Checked   : Boolean;              -- Indicates if the device has been checked
      Source    : Track_Source_Kind;    -- Indicates where the track comes from
      Id        : Natural := 0;         -- The OGN (FLARM) identifier
      
      -- Message fields                 -- Bytes / Units / Accuracy
                                        -- 1       Bit flags
      Age       : Short_Short_Integer;  -- 1       [s]     1
      Latitude  : Float;                -- 4       [deg]   ? (relative to reference lat)
      Longitude : Float;                -- 4       [deg]   ? (relative to reference lon)
      Altitude  : Short_Natural;        -- 2       [m]     1
      Vario     : Short_Short_Integer;  -- 1       [dm/s]  1
      Speed     : Short_Short_Natural;  -- 1       [km/h]  2
      Course    : Short_Short_Natural;  -- 1       [deg]   1.5
      Rotation  : Short_Short_Integer;  -- 1       [deg/s] 1
                                        -- 2       Device tailmark
                                        -- 2       User defined mark
 
   end record;
   
   --===========================================================================
   -- Updates the external fields
   --===========================================================================
   procedure Update_From (This : in out Track_Record; Other : Track_Record);
   
   --===========================================================================
   -- Updates the external fields
   --===========================================================================
   procedure Update_From (This : in out Track_Record; User : User_Record);
   
   --===========================================================================
   -- Returns 8-bits track flags based on the data
   --===========================================================================
   function Get_Bit_Flags (This : Track_Record) return Short_Short_Natural;
   
   --===========================================================================
   -- Returns the track mark based on the linked data
   --===========================================================================
   function Get_Tailmark (This : Track_Record) return Track_Marks;
   
   --===========================================================================
   -- Indicates if the user and the track are likely the same
   --===========================================================================
   function Merged_User (This : Track_Record; User : User_Record) return Boolean;
   
   No_Track : constant Track_Record := (Timestamp => Clock - 240.0,
                                        Time      => 0,
                                        Position  => No_Position_Record,
                                        User      => No_User,
                                        Device    => null,
                                        Checked   => False,
                                        Source    => Source_None,
                                        Id        => 0,
                                        Age       => 0,
                                        Latitude  => 0.0,
                                        Longitude => 0.0,
                                        Altitude  => 0,
                                        Vario     => 0,
                                        Speed     => 0,
                                        Course    => 0,
                                        Rotation  => 0);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The stack of tracks
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stack : aliased Track_Stack;
                   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Other tracker variables
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
   Reference       : Position_Record := (50.7, 3.8);
   
   Limits          : Position_Record := (1.0, 2.0);
   
   Ogn_Range       : Long_Float      := 150.0;
   
   Ogn_Ceiling     : Natural         := 2500; -- [m]
   
   Traffic_Range   : Float           := 40.0;
   
   Gliders_Only    : Boolean         := False;
   
   Small_Only      : Boolean         := False;
     
   Ogn_Url         : String (1..100) := (others => ' ');
    
   Ogn_Port        : Natural := 14580;
               
   Log_Lattency    : Float := 2.0;

end Traffic;
--------------------------------------------------------------------------------
