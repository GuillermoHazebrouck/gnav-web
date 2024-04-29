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
with Math.Vector2;
use  Math.Vector2;
with Maps;
with Maps.Terrain;
with Timing.Events;
with Utility;
with Utility.Log;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Plan is

   --===========================================================================
   --
   --===========================================================================
   procedure Recompute_All_Tasks is
   begin

      for F in Flight_Plan_Range loop

         Flight_Plans (F).Recompute_Tasks;

      end loop;

   end Recompute_All_Tasks;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
   begin

      if Append_Flight_Plan then
         null;
      end if;

      Read_Flight_Plans;

      Timing.Events.Register_Timer (Timer    => Timing.Time_Delta,
                                    Callback => Update_Flight_Plan'Access);

      Timing.Events.Register_Timer (Timer    => 5.0,
                                    Callback => Save_If_Modified'Access);

      Maps.Terrain.On_Loaded.Connect (Recompute_All_Tasks'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The file storing the flights
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   File_Name : constant String := Utility.Base_Directory & "data/flight_list.dat";

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Bearing (This : Waypoint_Record) return String is
   begin

      return Utility.Strings.Float_Image (This.Bearing, 0) & "*";

   end Get_Bearing;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Distance (This : Waypoint_Record) return String is
   begin

      if This.Distance < 10.0 then

         return Utility.Strings.Float_Image (This.Distance, 1);

      else

         return Utility.Strings.Float_Image (This.Distance, 0);

      end if;

   end Get_Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Vector (This : Waypoint_Record) return String is
   begin

      if This.Distance < 10.0 then

         return Utility.Strings.Float_Image (This.Distance, 1) & " " & Utility.Strings.Float_Image (This.Bearing, 0) & "*";

      else

         return Utility.Strings.Float_Image (This.Distance, 0) & " " & Utility.Strings.Float_Image (This.Bearing, 0) & "*";

      end if;

   end Get_Vector;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Margin (This : Waypoint_Record) return String is

      use Utility.Strings;

   begin

      if This.Margin = No_Altitude then

         return "N.A.";

      elsif This.Margin > 0.0 then

         return "}" & Float_Image (abs This.Margin, 0);

      else

         return "{" & Float_Image (abs This.Margin, 0);

      end if;


   end Get_Margin;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Required_Altitude (This : Waypoint_Record) return String is

      use Utility.Strings;

   begin

      if This.Required = No_Altitude or This.Required > 9999.0 then

         return "----";

      elsif This.Required = 0.0 then

         return "";

      else

         return Float_Image (This.Required, 0);

      end if;

   end Get_Required_Altitude;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Optimal_Speed (This : Waypoint_Record) return String is

      use Utility.Strings;

   begin

      if This.Airspeed <= 0.0 then

         return "";

      else

         return Float_Image (3.6 * This.Airspeed, 0);

      end if;

   end Get_Optimal_Speed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Read_Flight_Plans is
   begin

      -- TODO: read flight plans from local storage

      Recompute_All_Tasks;

   end Read_Flight_Plans;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Write_Flight_Plans is
   begin

      null;

   end Write_Flight_Plans;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Save_If_Modified is
   begin

      if Modified then

         Write_Flight_Plans;

         Modified := False;

      end if;

   end Save_If_Modified;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Update_Flight_Plan is

      Plan   : access Flight_Plan_Record := Flight_Plan;
      Wpt    : access Waypoint_Record    := null;
      Dist   : Float;

   begin

      -- Calculate distance and bearing
      --------------------------------------------------------------------------

      for W in Waypoint_Range loop

         exit when not Plan.Waypoints (W).Is_Loaded;

         Wpt := Plan.Waypoints (W)'Access;

         -- Vector (distance/bearing)
         -----------------------------------------------------------------------

         Maps.Coordinates (Position_A => Flight.Data.Position,
                           Position_B => Wpt.Position,
                           Distance   => Wpt.Distance,
                           Bearing    => Wpt.Bearing);

         -- Altitude margin
         -----------------------------------------------------------------------

         Wpt.Margin := Flight.Aircraft.Get_Final_Altitude (Wpt.Position);

         if Wpt.Margin /= No_Altitude then

            Wpt.Margin := Wpt.Margin - Wpt.Elevation;

         end if;

         if Wpt.Margin = No_Altitude then

            Wpt.In_Range := False;

         else

            Wpt.In_Range := Wpt.Margin > 0.0;

         end if;

         -- Required altitude
         -----------------------------------------------------------------------

         Wpt.Required := Flight.Aircraft.Get_Required_Altitude (Wpt.Position);

         -- Optimal airspeed
         -----------------------------------------------------------------------

         Wpt.Airspeed := Flight.Aircraft.Get_Optimal_Speed (Wpt.Position);

         -- Status flags
         -----------------------------------------------------------------------

         Wpt.In_Proximity := Maps.Distance (Flight.Data.Position, Wpt.Position) < Proximity_Threshold;

         Wpt.Visited      := Plan.Waypoints (W).Visited or else Wpt.In_Proximity;

         Wpt.Is_Active    := W = Plan.Target;

         -- Task update
         -----------------------------------------------------------------------

         if Plan.Tasks (W).Is_Loaded then

            Plan.Tasks (W).Is_Active := Plan.Tasks (W).Point_B.Is_Active;

            Dist := Maps.Distance (Position_A => Flight.Data.Position,
                                   Position_B => Plan.Tasks (W).Point_B.Position);

            if Dist < Plan.Tasks (W).Length then

               Plan.Tasks (W).Progress := 1.0 - Dist / Plan.Tasks (W).Length;

            else

               Plan.Tasks (W).Progress := 0.0;

            end if;

         end if;

      end loop;

      -- Jump to the next waypoint when the active waypoint is reached
      --------------------------------------------------------------------------

      if Jump_In_Proximity and Plan.Waypoints (Plan.Target).In_Proximity then

         if not Plan.Go_Back then

            Goto_Next_Waypoint;

         else

            Goto_Previous_Waypoint;

         end if;

      end if;

   end Update_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Flight_Plan return access Flight_Plan_Record is
   begin

      return Flight_Plans (Active_Flight_Plan)'Access;

   end Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Activates the next flight plan
   --===========================================================================
   function Next_Flight_Plan return Boolean is

      F : Flight_Plan_Range renames Active_Flight_Plan;

   begin

      if F < Flight_Plan_Range'Last and then Flight_Plans (F + 1).Is_Loaded then

         F := F + 1;

         Modified := True;

         return True;

      end if;

      return False;

   end Next_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Activates the previous flight plan
   --===========================================================================
   function Previous_Flight_Plan return Boolean is

      F : Flight_Plan_Range renames Active_Flight_Plan;

   begin

      if F > Flight_Plan_Range'First then

         F := F - 1;

         Modified := True;

         return True;

      end if;

      return False;

   end Previous_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize (This : in out Flight_Plan_Record) is

      use Utility.Strings;

   begin

      This := No_Flight_Plan_Record;

      Override (This.Name, "PLAN");

      This.Is_Loaded := True;

      Override (This.Waypoints (1).Name, Gnav_Info.Home_Name);

      This.Waypoints (1).Is_Loaded := True;

      This.Waypoints (1).Position  := Gnav_Info.Home_Position;

      This.Recompute_Tasks;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Recompute_Tasks (This : in out Flight_Plan_Record) is
   begin

      This.Tasks    := (others => No_Task_Record);
      This.Length   := 0.0;
      This.Distance := 0.0;

      for W in Waypoint_Range loop

         -- Exit when reaching the last waypoit or when the next is not loaded
         -----------------------------------------------------------------------

         if W = Waypoint_Range'Last or else not This.Waypoints (W + 1).Is_Loaded then
            exit;
         end if;

         -- Load properties
         -----------------------------------------------------------------------

         if This.Go_Back then

            This.Tasks (W).Point_A := This.Waypoints (W+1)'unrestricted_access;
            This.Tasks (W).Point_B := This.Waypoints (W  )'unrestricted_access;

         else

            This.Tasks (W).Point_A := This.Waypoints (W  )'unrestricted_access;
            This.Tasks (W).Point_B := This.Waypoints (W+1)'unrestricted_access;

         end if;

         Maps.Coordinates (Position_A => This.Tasks (W).Point_A.Position,
                           Position_B => This.Tasks (W).Point_B.Position,
                           Distance   => This.Tasks (W).Length,
                           Bearing    => This.Tasks (W).Course);

         This.Tasks (W).Vector := Maps.Vector (Position_A => This.Tasks (W).Point_B.Position,
                                               Position_B => This.Tasks (W).Point_A.Position);

         This.Tasks (W).Vector.Normalize;

         This.Tasks (W).Is_Loaded := True;

         This.Tasks (W).Is_Active := This.Tasks (W).Point_B.Is_Active;

         This.Length := This.Length + This.Tasks (W).Length;

      end loop;

      for Waypoint of This.Waypoints loop

         exit when not Waypoint.Is_Loaded;

         This.Distance := Float'Max (This.Distance, Maps.Distance (This.Waypoints (1).Position, Waypoint.Position));

         Waypoint.Elevation := Maps.Terrain.Get_Elevation (Waypoint.Position);

      end loop;

   end Recompute_Tasks;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Go_Back (This : in out Flight_Plan_Record; Value : Boolean) is
   begin

      if This.Go_Back /= Value then

         This.Go_Back := Value;

         This.Recompute_Tasks;

      end if;

   end Set_Go_Back;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Length (This : Flight_Plan_Record) return String is
   begin

      return Float_Image (This.Length, 1);

   end Get_Length;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Distance (This : Flight_Plan_Record) return String is
   begin

      return Float_Image (This.Distance, 1);

   end Get_Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Length (This : Flight_Plan_Record) return Float is
   begin

      return This.Length;

   end Get_Length;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Append_Flight_Plan return Boolean is
   begin

      for F in Flight_Plan_Range loop

         if not Flight_Plans (F).Is_Loaded then

            Flight_Plans (F).Initialize;

            Active_Flight_Plan := F;

            Modified := True;

            return True;

         end if;

      end loop;

      return False;

   end Append_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   --  (See specification file)
   --===========================================================================
   function Remove_Flight_Plan return Boolean is

      F : Flight_Plan_Range renames Active_Flight_Plan;

   begin

      -- Note: it is not allowed to remove all flight plans, at least one must
      -- remain as reference.
      --------------------------------------------------------------------------

      if
        (F = Flight_Plan_Range'First and Flight_Plans (F+1).Is_Loaded) or
        (F > Flight_Plan_Range'First and Flight_Plans (F  ).Is_Loaded)
      then

         -- Move all items backwards
         -----------------------------------------------------------------------
         for I in F..Flight_Plan_Range'Last-1 loop

            Flight_Plans (I) := Flight_Plans (I+1);

            exit when not Flight_Plans (I).Is_Loaded;

         end loop;

         -- The last waypoint is always vacant after the removal
         -----------------------------------------------------------------------
         Flight_Plans (Flight_Plan_Range'Last) := No_Flight_Plan_Record;

         -- If the new target is not loaded, move it to the new last one
         -----------------------------------------------------------------------
         if not Flight_Plans (F).Is_Loaded then

            for I in reverse Flight_Plan_Range loop

               F := I;

               exit when Flight_Plans (I).Is_Loaded;

            end loop;

         end if;

         Modified := True;

         return True;

      end if;

      return False;

   end Remove_Flight_Plan;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Next_Waypoint return access Waypoint_Record is
   begin

      return Flight_Plan.Waypoints (Flight_Plan.Target)'Access;

   end Next_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Remove_Active_Waypoint return Boolean is

      W : Waypoint_Range renames Flight_Plan.Target;

   begin

      -- Note: it is not allowed to remove all waypoints, at least one must
      -- remain as reference.
      --------------------------------------------------------------------------

      if
        (W = Waypoint_Range'First and Flight_Plan.Waypoints (W+1).Is_Loaded) or
        (W > Waypoint_Range'First and Flight_Plan.Waypoints (W  ).Is_Loaded)
      then

         -- Move all items backwards
         -----------------------------------------------------------------------
         for I in W..Waypoint_Range'Last-1 loop

            Flight_Plan.Waypoints (I) := Flight_Plan.Waypoints (I+1);

            exit when not Flight_Plan.Waypoints (I).Is_Loaded;

         end loop;

         -- The last waypoint is always vacant after the removal
         -----------------------------------------------------------------------
         Flight_Plan.Waypoints (Waypoint_Range'Last) := No_Waypoint_Record;

         -- If the new target is not loaded, move it to the new last one
         -----------------------------------------------------------------------
         if not Flight_Plan.Waypoints (W).Is_Loaded then

            for I in reverse Waypoint_Range loop

               W := I;

               exit when Flight_Plan.Waypoints (I).Is_Loaded;

            end loop;

         end if;

         Flight_Plan.Recompute_Tasks;

         Modified := True;

         return True;

      end if;

      return False;

   end Remove_Active_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Append_Waypoint return Boolean is

      use Utility.Strings;

      W : Waypoint_Range renames Flight_Plan.Target;

   begin

      if W < Waypoint_Range'Last then

         -- Move all items forward
         -----------------------------------------------------------------------
         for I in reverse W..Waypoint_Range'Last-1 loop

            Flight_Plan.Waypoints (I+1).Name      := Flight_Plan.Waypoints (I).Name;

            Flight_Plan.Waypoints (I+1).Position  := Flight_Plan.Waypoints (I).Position;

            Flight_Plan.Waypoints (I+1).Is_Loaded := Flight_Plan.Waypoints (I).Is_Loaded;

         end loop;

         -- Advance to the copy
         -----------------------------------------------------------------------
         W := W + 1;

         -- Make sure the position is valid
         -----------------------------------------------------------------------
         if Flight_Plan.Waypoints (W).Position = No_Position_Record then

            Flight_Plan.Waypoints (W).Position := Flight.Data.Position;

         end if;

         Override (Flight_Plan.Waypoints (W).Name, "WPT");

         Flight_Plan.Waypoints (W).Is_Loaded := True;

         Flight_Plan.Recompute_Tasks;

         Modified := True;

         return True;

      end if;

      return False;

   end Append_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Prepend_Waypoint return Boolean is

      use Utility.Strings;

      W : Waypoint_Range renames Flight_Plan.Target;

   begin

      if W < Waypoint_Range'Last then

         -- Move all items forward
         -----------------------------------------------------------------------
         for I in reverse W..Waypoint_Range'Last-1 loop

            Flight_Plan.Waypoints (I+1).Name      := Flight_Plan.Waypoints (I).Name;

            Flight_Plan.Waypoints (I+1).Position  := Flight_Plan.Waypoints (I).Position;

            Flight_Plan.Waypoints (I+1).Is_Loaded := Flight_Plan.Waypoints (I).Is_Loaded;

         end loop;

         -- Stay with the copy
         -----------------------------------------------------------------------
         Override (Flight_Plan.Waypoints (W).Name, "WPT");

         Flight_Plan.Recompute_Tasks;

         Modified := True;

         return True;

      end if;

      return False;

   end Prepend_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Home_Waypoint return access Waypoint_Record is
   begin

      return Flight_Plan.Waypoints (1)'Access;

   end Home_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Goto_Previous_Waypoint is
   begin

      if Flight_Plan.Target > Waypoint_Range'First then

         for W in reverse Waypoint_Range'First..Flight_Plan.Target - 1 loop

            if
              Flight_Plan.Waypoints (W).Is_Loaded and then
              (not Jump_In_Proximity or not Flight_Plan.Waypoints (W).In_Proximity)
            then

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := False;

               Flight_Plan.Target := W;

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := True;

               return;

            end if;

         end loop;

      end if;

   end Goto_Previous_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Activates the next waypoint
   --===========================================================================
   procedure Goto_Next_Waypoint is
   begin

      if Flight_Plan.Target < Waypoint_Range'Last then

         for W in Flight_Plan.Target + 1..Waypoint_Range'Last loop

            if
              Flight_Plan.Waypoints (W).Is_Loaded and then
              (not Jump_In_Proximity or not Flight_Plan.Waypoints (W).In_Proximity)
            then

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := False;

               Flight_Plan.Target := W;

               Flight_Plan.Waypoints (Flight_Plan.Target).Is_Active := True;

               return;

            end if;

         end loop;

      end if;

   end Goto_Next_Waypoint;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Toggle_Go_Back is
   begin

      Flight_Plan.Go_Back := not Flight_Plan.Go_Back;

      Flight_Plan.Recompute_Tasks;

   end Toggle_Go_Back;
   -----------------------------------------------------------------------------

end Flight.Plan;
--------------------------------------------------------------------------------
