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
with Maps;
use  Maps;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
-- This package provides all aircraft related data:
--  > Callsign, model, etc.
--  > Weight and balance
--  > Gliding performance and range
-- TODO: rename this to "Flight.Performance"
--//////////////////////////////////////////////////////////////////////////////
package Flight.Aircraft is

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
   subtype Polar_Range is Natural range 1..30;
   
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
      
      -- The position (in meters)
      Position : Float;
      
   end record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Default mass point value
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Mass_Point : constant Mass_Point_Record := (Active   => False,
                                                  Label    => (others => ' '),
                                                  Mass     => 0.0,
                                                  Mass_Max => 0.0,
                                                  Mass_Min => 0.0,
                                                  Position => 0.0);
   
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
   
      -- The stall speed for the current mass [EAS] (in m/s) 
      --------------------------------------------------------------------------
      V_S0 : Float := 0.0;
              
      -- The recomended landing speed [EAS] (in m/s) = 1.3 * V_S0 + 0.5 * V_Wind
      --------------------------------------------------------------------------
      V_LND : Float := 0.0;
         
      -- The maximum speed for winch launch (in m/s) 
      --------------------------------------------------------------------------
      V_WL : Float := 0.0;
         
      -- The maximum full-control-deflection airspeed [EAS] (in m/s) 
      --------------------------------------------------------------------------
      V_NO : Float := 0.0;
         
      -- The maximum airspeed [TAS] (in m/s) 
      --------------------------------------------------------------------------
      V_NE : Float := 0.0;
          
      -- The mass of the airplane
      --------------------------------------------------------------------------
      Empty_Mass : Float := 0.0;
   
      -- The maximum takoff mass
      --------------------------------------------------------------------------
      Maximum_Mass : Float := 0.0;
      
      -- The current mass of the airplane (empy + mass points)
      --------------------------------------------------------------------------
      Total_Mass : Float := 0.0;
         
      -- All mass points
      --------------------------------------------------------------------------
      Mass_Points : Mass_Point_Array := (others => No_Mass_Point);
       
      -- The reference area used for the aerodynamic coefficients
      --------------------------------------------------------------------------
      Wing_Area : Float := 0.0;
   
      -- The aspect ratio
      --------------------------------------------------------------------------
      Aspect_Ratio : Float := 15.0;
      
      -- 4th degree polynomial for rough drag regime
      --------------------------------------------------------------------------
      T0, T1, T2, T3, T4 : Long_Float := 0.0;
      
      -- 4th degree polynomial for clean drag regime
      --------------------------------------------------------------------------
      L0, L1, L2, L3, L4 : Long_Float := 0.0;
      
      -- The aerodynamic force coefficients at equilibrium for the current drag
      -- factor and altitude
      --------------------------------------------------------------------------
      Polar : Polar_Point_Array := (others => (0.0, 0.0));
      
      -- The minimum lift coefficient (for the polars)
      --------------------------------------------------------------------------
      Cl_Min : Float := 0.15;
            
      -- The maximum lift coefficient (with neutral flaps)
      --------------------------------------------------------------------------
      Cl_Max : Float := 1.2;
        
      -- The maximum relation between the lift and drag coeficients
      --------------------------------------------------------------------------
      Cl_Cd_Max : Float := 0.0;
      
      -- The factor affecting the drag coefficient
      --------------------------------------------------------------------------
      Drag_Factor : Float := 0.0;
      
   end record;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Aircraft_Range is Positive range 1..15;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Aircraft_Array is array (Aircraft_Range) of aliased Aircraft_Record;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Aircrafts : Aircraft_Array;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   This_Aircraft : not null access Aircraft_Record := Aircrafts (Aircrafts'First)'Access;
   
   --===========================================================================
   -- Sets the next aircraft on the list as This_Aircraft
   --===========================================================================
   procedure Next_Aircraft;
   
   --===========================================================================
   -- Recalculates the aircraft mass using the local mass points.
   --===========================================================================
   procedure Recalculate_Mass;
   
   --===========================================================================
   -- Calculates the airspeed and sink rate in the equilibrium state for all
   -- polar nodes.
   -- NOTE: this must be run when the altitude and mass changed considerably.
   --===========================================================================
   procedure Calculate_Gliding_States;
   
   --===========================================================================
   -- Calculates the best gliding slopes in all directions for the current wind.
   --===========================================================================
   procedure Calculate_Gliding_Spectrum;
   
   --===========================================================================
   -- Returns the maximum altitude when getting to a given point in a straight
   -- line, considering uniform wind (not considering the wind aloft and the 
   -- necessary turn to align the craft in that direction).
   -- NOTE: No_Altitude is returned when the point is unreachable
   --===========================================================================
   function Get_Final_Altitude (Position : Position_Record) return Float;

   --===========================================================================
   -- Returns the altitude required to get to a given point in a straight
   -- line, considering uniform wind (not considering the wind aloft and the 
   -- necessary turn to align the craft in that direction).
   --===========================================================================
   function Get_Required_Altitude (Position : Position_Record) return Float;

   --===========================================================================
   -- Returns the optimal gliding speed (TAS) to a given point in a straight line.
   --===========================================================================
   function Get_Optimal_Speed (Position : Position_Record) return Float;

   --===========================================================================
   -- Returns the optimal gliding speed (TAS) in the current direction.
   --===========================================================================
   function Get_Optimal_Speed return Float;
   
   --===========================================================================
   -- Returns the expected optimal gliding ratio in the given direction (deg) 
   --===========================================================================
   function Get_Expected_Gliding_Ratio (Course : Float) return Float;
   
   --===========================================================================
   -- Returns the standard gliding ratio (CL/CD)max
   --===========================================================================
   function Get_Standard_Gliding_Ratio return Float;
   
   --===========================================================================
   -- Range_Straight: range straight ahead
   -- Range_Local   : range with back home margin
   --===========================================================================
   type Range_Mode_Kinds is (Range_Straight, Range_Local);
   
   --===========================================================================
   --
   --===========================================================================
   type Cone_Mode_Kinds is (Cone_Optimal, Cone_10_To_1);
      
   --===========================================================================
   --
   --===========================================================================
   procedure Set_Range_Mode (Value : Range_Mode_Kinds);
   
   --===========================================================================
   --
   --===========================================================================
   procedure Set_Cone_Mode (Value : Cone_Mode_Kinds);
   
   --===========================================================================
   --
   --===========================================================================
   procedure Set_Reference (Value : Position_Record);
      
   --===========================================================================
   --
   --===========================================================================
   function Get_Range_Mode return Range_Mode_Kinds;
   
   --===========================================================================
   --
   --===========================================================================
   function Get_Cone_Mode return Cone_Mode_Kinds;
   
   --===========================================================================
   -- Sets the expected inter-thermal vertical wind component (sink)
   --===========================================================================
   procedure Set_Sink (Value : Float);
      
   --===========================================================================
   -- Returns the expected inter-thermal vertical wind component (sink)
   --===========================================================================
   function Get_Sink return Float;
      
   --===========================================================================
   -- Returns the expected vertical speed when flying at the given airspeed
   --===========================================================================
   function Get_Vertical_Speed (Airspeed : Float) return Float;
      
   --===========================================================================
   -- Returns the maximum altitude when getting to a given point in a straight
   -- line and turning back to the given reference point.
   -- NOTE: No_Altitude is returned when the home point is unreachable
   --===========================================================================
   function Get_Final_Altitude_2 (Position : Position_Record) return Float;

   --===========================================================================
   -- Initializes the package
   --===========================================================================
   procedure Initialize;
   
   --===========================================================================
   -- Saves the configuration of the aircraft to the local storage
   --===========================================================================
   procedure Save_Configuration;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the data has been modified (it will be saved if so)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Modified : Boolean := False;
   
private
   
   --===========================================================================
   -- Updates the range cone
   --===========================================================================
   procedure Update_Cone;
   
end Flight.Aircraft;
--------------------------------------------------------------------------------
