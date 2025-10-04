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
with Glex.Colors;
with Glex.Fonts;
with Glex.Symbols;
with Maps.Terrain;
with Maps.Airspaces.Monitor;
with Maps.Reference;
with Math;
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
   -- The base of the clouds (mASL)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Cloud_Base        : Float      := 1300.0;
   Thermal_Distance  : Long_Float := 0.0;
   Thermal_Azimuth   : Long_Float := 0.0;
   Thermal_Intensity : Float      := 1.0;
   Thermal_Sing      : Boolean    := False;
   Vertical_Wind     : Float      := 0.0;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Thermal_Record is tagged record
      
      -- The position of ceiling
      -----------------------------
      Position  : Position_Record;
      
      -- The intensity in m/s
      -----------------------------
      Intensity : Float;
      
      -- The age
      -----------------------------
      Age       : Float;
            
   end record;
   -----------------------------------------------------------------------------
  
   
   
   --===========================================================================
   --
   --===========================================================================
   function Get_Intensity (This : Thermal_Record) return Float is
      
      Factor   : Float;
      Distance : Float;
      Position : Position_Record;
      Wind     : Vector2_Record := Flight.Data.Wind;
      
   begin
      
      Wind.Normalize;
      Wind.Revert;
      Wind.Scale (0.001 * Long_Float (Cloud_Base - Flight.Data.Altitude));
      
      Position := Maps.Position (This.Position, Wind);
      Distance := Maps.Distance (Flight.Data.Position, Position);
      
      if This.Age < 600.0 then     --> 10 minutes growing                                   
         Factor := This.Age / 600.0;               
      elsif This.Age < 900.0 then  --> 5 minutes decaying to zero
         Factor := (900.0 - This.Age) / 300.0;   
      end if;
      
      if Distance < 0.25 then
         return Factor * This.Intensity; --> Maximum at the core
      else
         return Factor * This.Intensity * 0.25 / Distance;
      end if;

   end Get_Intensity;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Create (This : in out Thermal_Record) is
      
      Vector : Vector2_Record := New_Vector2_Record (Thermal_Distance, 0.0);
      
   begin
      
      Thermal_Sing := not Thermal_Sing;
      
      This.Age := 0.0;
      
      Vector.Rotate (Thermal_Azimuth * Math.Pi / 180.0);
        
      This.Position := Maps.Position (Flight.Data.Position, Vector);
      
      if Thermal_Sing then
         This.Intensity :=  5.0;
      else
         This.Intensity := -5.0;
      end if;
           
      if Gnav_Info.Simulation_Reset then
         This.Age := 400.0;
      end if;
      
   end Create;
   -----------------------------------------------------------------------------
   
   
   
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Thermals : array (1..40) of Thermal_Record := (others => (Position  => No_Position_Record,
                                                             Intensity => 0.0,
                                                             Age       => 0.0));
     
   --===========================================================================
   --
   --===========================================================================
   procedure Shift_Thermals is
   begin
      
      Thermal_Distance := Thermal_Distance + 1.2;
      
      if Thermal_Distance > 15.0 then
         Thermal_Distance := 1.0;
      end if;
      
      Thermal_Azimuth := Thermal_Azimuth + 37.0; --> prime number here?
      
      if Thermal_Azimuth > 360.0 then
         Thermal_Azimuth := Thermal_Azimuth - 360.0;
      end if;
      
   end Shift_Thermals;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Update_Thermals is
      
      Wind : Vector2_Record := Flight.Data.Wind;
      
   begin
      Shift_Thermals;
      
      Wind.Scale (0.001); -- km/s
      
      Vertical_Wind := 0.0;
      
      for Thermal of Thermals loop

         if 
           Thermal.Age =   0.0 or else
           Thermal.Age > 900.0 or else
           Maps.Distance (Flight.Data.Position, Thermal.Position) > 15.0 
         then
           
            Thermal.Create;
            
            Shift_Thermals;
            
         else                        
            Thermal.Position := Maps.Position (Thermal.Position, Wind);
         
         end if;
         
         Vertical_Wind := Vertical_Wind + Thermal.Get_Intensity;
         
         Thermal.Age := Thermal.Age + 1.0;
         
      end loop;
      
   end Update_Thermals;
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
      V : Float          := Vertical_Wind;
      D : Vector2_Record;
      W : Vector2_Record;

   begin
      
      if Gnav_Info.Simulation_Reset then

         Home := Flight.Plan.Home_Waypoint.Position;
         H    := 600.0 + Maps.Terrain.Get_Elevation (Home);
         Q    := Math.Vector2.No_Vector2_Record;
         R    := New_Vector2_Record (1.0, 0.0);

         Gnav_Info.Simulation_Reset := False;

      end if;

      Update_Thermals;
      
      W := Flight.Data.Wind;
      
      W.Scale (0.001);
      
      R.Normalize;

      --if abs Selected_Turn_Rate < 10.0 then
      S := 0.001 * Long_Float (Selected_Airspeed);
      --else
      --   S := 0.001 * Long_Float (Flight.Aircraft.Get_Optimal_Speed); -- assume optimal speed
      --end if;
      
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
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Clouds (View : Map_View_Record) is

      use Glex.Colors;
      use all type Glex.Fonts.Font_Alignment_Types;
      
      X, Y, S  : Float;
      Color : Color_Record;

      North_East,
      South_West : Position_Record;

   begin
      
      View.Get_Limits (North_East, South_West);

      for Thermal of Thermals loop

         if
           Thermal.Intensity > 0.0 and then
           Maps.On_Region (Thermal.Position, North_East, South_West)
         then

            if Thermal.Age < 200.0 then
               Color := Color_White;
            elsif Thermal.Age < 300.0 then
               Color := Color_Gray_9;
            elsif Thermal.Age < 400.0 then
               Color := Color_Gray_8;
            elsif Thermal.Age < 500.0 then
               Color := Color_Gray_7;
            elsif Thermal.Age < 600.0 then
               Color := Color_Gray_6;
            else
               Color := Color_Gray_7;
            end if;
               
            if Thermal.Age < 600.0 then
               S := 0.02 + 0.02 * Thermal.Age / 600.0;
            else
               S := 0.04 - 0.01 * (Thermal.Age - 600.0) / 300.0;
            end if;

            View.Position_To_Screen (Thermal.Position, X, Y);

            Glex.Symbols.Draw (Glex.Symbols.Triangle_Down,
                               X, Y, S,
                               Color,
                               Alignment_LC);

         end if;

      end loop;

   end Draw_Clouds;
   -----------------------------------------------------------------------------
   
end Flight.Simulation;
--------------------------------------------------------------------------------
