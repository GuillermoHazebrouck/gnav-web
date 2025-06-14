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
with Gnav_Info;
with Flight.Aircraft;
with Flight.Plan;
with Flight.Register;
with Maps.Terrain;
with Maps.Airspaces.Monitor;
with Maps.Reference;
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
package body Flight.Simulation is
 
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Thermal_Record is tagged record
      
      Position  : Vector2_Record;
      
      Intensity : Float;
      
      Height    : Float;
      
   end record;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   T    : Long_Float                  := 0.0;
   Q    : Math.Vector2.Vector2_Record := Math.Vector2.No_Vector2_Record;
   R    : Vector2_Record              := New_Vector2_Record (1.0, 0.0);
   H    : Float                       := 300.0;
   Home : Maps.Position_Record        := Gnav_Info.Home_Position;
   -- Simulate a trajectory for development (1 second lapses)
   -- The trajectory is a mix of turns and dives at different vertical speeds
   --===========================================================================
   procedure Next_Simulation_Step (Millis : Interfaces.IEEE_Float_64) is

      use Interfaces;
      use Math.Vector2;
      use Flight;
      use Utility.Calendar;

      P : Maps.Position_Record := Flight.Data.Position;
      S : Long_Float     := 0.001 * Long_Float (Selected_Airspeed); -- km/s
      C : Long_Float     := 0.0;
      Z : Float          := 0.0;
      V : Float          := 0.0;
      D : Vector2_Record;
      W : Vector2_Record;

   begin

      if Gnav_Info.Simulation_Reset then

         Home := Flight.Plan.Home_Waypoint.Position;
         H    := 300.0 + Maps.Terrain.Get_Elevation (Home);
         Q    := Math.Vector2.No_Vector2_Record;
         R    := New_Vector2_Record (1.0, 0.0);

         Gnav_Info.Simulation_Reset := False;

      end if;

      W := Flight.Data.Wind;
      
      W.Scale (0.001);
      
      R.Normalize;

      -- Simple termal simulation: when turning go up
      if abs Selected_Turn_Rate < 10.0 then
         V := -0.4;
         S := 0.001 * Long_Float (Selected_Airspeed);
      else
         V :=  1.8;
         S := 0.001 * Long_Float (Flight.Aircraft.Get_Optimal_Speed); -- assume optimal speed
      end if;
      
      R.Rotate (Long_Float (Selected_Turn_Rate) * Math.Pi / 180.0);

      D := S * R + W;
        
      Q := Q + D;

      P := Maps.Position (Home, Q);
      
      Z := Flight.Aircraft.Get_Vertical_Speed (Selected_Airspeed);
      
      H := H + Z + V;

      Flight.Data.Timestamp := Cached_Time;

      Flight.Data.Origin := (others => Update_None);

      Flight.Data.Position := P;
      Flight.Data.Ages   (Field_Position) := Cached_Time;
      Flight.Data.Origin (Field_Position) := Update_External;

      Flight.Data.Altitude := Float (H);
      Flight.Data.Ages   (Field_Altitude) := Cached_Time;
      Flight.Data.Origin (Field_Altitude) := Update_External;
      
      Flight.Data.Speed := Float (1000.0 * D.Norm2);
      Flight.Data.Ages   (Field_Speed) := Cached_Time;
      Flight.Data.Origin (Field_Speed) := Update_External;

      Flight.Data.Heading := Float (90.0 - R.Bearing * 180.0 / Math.Pi);
      if Flight.Data.Heading < 0.0 then
         Flight.Data.Heading := Flight.Data.Course + 360.0;
      end if;
      Flight.Data.Ages   (Field_Heading) := Cached_Time;
      Flight.Data.Origin (Field_Heading) := Update_External;
      
      Flight.Data.Course  := Float (90.0 - D.Bearing * 180.0 / Math.Pi);
      if Flight.Data.Course < 0.0 then
         Flight.Data.Course := Flight.Data.Course + 360.0;
      end if;
      Flight.Data.Ages   (Field_Course) := Cached_Time;
      Flight.Data.Origin (Field_Course) := Update_External;

      Flight.Complete_Data;

      if Flight.Data.Is_Valid (Field_Position) then
         if Flight.Data.Is_Valid (Field_Altitude) then
            Maps.Airspaces.Monitor.Process_Location (Flight.Data.Position, Data.Altitude);
         end if;
         Maps.Reference.Update_Distance_To_Airfields (Flight.Data.Position);
      end if;

      Flight.Cache_Data;

      Flight.Register.Update;

      if not Flight.Data.Is_Recent (Field_Elevation) then
         Gnav_Info.Simulation_Reset := True;
      end if;
      
   end Next_Simulation_Step;
   -----------------------------------------------------------------------------
   
end Flight.Simulation;
--------------------------------------------------------------------------------
