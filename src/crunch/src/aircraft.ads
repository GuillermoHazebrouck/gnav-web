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

--//////////////////////////////////////////////////////////////////////////////
-- This package provides all aircraft related data:
--  > Callsign, model, etc.
--  > Weight and balance
--  > Gliding performance and range
-- TODO: rename this to "Flight.Performance"
--//////////////////////////////////////////////////////////////////////////////
package Aircraft is

   --===========================================================================
   -- Reads the aircraft data from the data file 'data/aircraft.dat'
   --===========================================================================
   procedure Load_Aircraft_Data;
   
   --===========================================================================
   -- Compiles the aircraft data in native format
   --===========================================================================
   procedure Compile_Data;
   
private
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- String used to represent aircraft names and other aircraft data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Aircraft_Names is String (1..12);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Different flap configurations
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Flap_Kinds is (Flap_None, Flap_Down_1, Flap_Down_2, Flap_Up_1);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Represents the aerodynamic force coefficients
   -- NOTE: only one set is used for all equilibrium states
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Polar_Point_Record is record
      
      Cl : Float;
      
      Cd : Float;
      
   end record;
     
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Polar_Range is Natural range 1..40;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Polar_Point_Array is array (Polar_Range) of Polar_Point_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A point on the aircraft where mass can be attached
   -- It can be used for pilots, water balast, spin balast, lead balast, etc.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Point_Record is record
      
      -- Indicates if the mass point is used
      Active   : Boolean;
      
      -- The name of the mass point
      Label    : Aircraft_Names;
      
      -- The mass (in kg)
      Mass     : Float;
      
      -- The maximum allowed mass
      Mass_Max : Float; 
      
      -- The minimum allowed mass
      Mass_Min : Float; 
      
      -- The arm (in meters)
      Arm      : Float;
      
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default mass point value
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Mass_Point : constant Mass_Point_Record := (Active   => False,
                                                  Label    => (others => ' '),
                                                  Mass     => 0.0,
                                                  Mass_Max => 0.0,
                                                  Mass_Min => 0.0,
                                                  Arm      => 0.0);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The range of mass points (not all need to be active)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Point_Range is new Positive range 1..10;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Mass_Point_Array is array (Mass_Point_Range) of Mass_Point_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Aircraft_Record is record
      
      --
      --------------------------------------------------------------------------
      Valid : Boolean := False;
      
      -- The aircraft model (e.g.: "ASK-21")
      --------------------------------------------------------------------------
      Model : Aircraft_Names := (others => ' ');
   
      -- The aircraft registration (e.g.: "D-1142") 
      --------------------------------------------------------------------------
      Registration : Aircraft_Names := (others => ' ');
             
      -- The stalling speed for the current weight [EAS] (in m/s) 
      --------------------------------------------------------------------------
      V_S0 : Float := 0.0;
              
      -- The recomended landing speed [EAS] (in m/s) = 1.3 * Vs1 + 0.5 * Vw
      --------------------------------------------------------------------------
      V_LND : Float := 0.0;
         
      -- The maximum full-control-deflection airspeed [EAS] (in m/s) 
      --------------------------------------------------------------------------
      V_NO : Float := 0.0;
         
      -- The maximum airspeed [TAS] (in m/s) 
      --------------------------------------------------------------------------
      V_NE : Float := 0.0;
          
      -- The mass of the airplane
      --------------------------------------------------------------------------
      Empty_Mass : Float := 0.0;
   
      -- The current mass of the airplane
      --------------------------------------------------------------------------
      Total_Mass : Float := 0.0;
         
      -- The maximum takoff mass
      --------------------------------------------------------------------------
      Maximum_Mass : Float := 0.0;
      
      -- All mass points
      --------------------------------------------------------------------------
      Mass_Points : Mass_Point_Array := (others => No_Mass_Point);
       
      -- The actual number of mass points
      --------------------------------------------------------------------------
      Mass_Count : Natural := 0;
           
      -- The reference area used for the aerodynamic coefficients
      --------------------------------------------------------------------------
      Area : Float := 0.0;
   
      -- The aerodynamic force coefficients at equilibrium for the current flaps
      --------------------------------------------------------------------------
      Polar : Polar_Point_Array := (others => (0.0, 0.0));
      
      -- The actual number of points in the polar curve
      --------------------------------------------------------------------------
      Polar_Count : Natural := 0;
           
      -- The maximum lift coefficient for the current flap configuration
      --------------------------------------------------------------------------
      Cl_Max : Float;
      
   end record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Aircraft_Range is Positive range 1..10;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Aircraft_Array is array (Aircraft_Range) of aliased Aircraft_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Aircrafts : Aircraft_Array;
   
   
end Aircraft;
--------------------------------------------------------------------------------
