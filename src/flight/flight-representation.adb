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
with Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
-- Gnav
with Flight;
use  Flight;
with Flight.Plan;
use  Flight.Plan;
with Flight.Traffic;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Glex.Lines;
with Maps;
use  Maps;
with Utility.Strings;
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Representation is

   -- Vertex buffer Id's
   ---------------------------------

   Airplane        : Glex.Lines.Resource_Type;

   Route           : Glex.Lines.Resource_Type;

   Vector_Home     : Glex.Lines.Resource_Type;

   Vector_Waypoint : Glex.Lines.Resource_Type;

   Trajectory      : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (30);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of clusters
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Cluster_Range is Positive range 1..80;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The current cluster
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   C : Cluster_Range := Cluster_Range'First;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Cluster_Array is array (Cluster_Range) of Glex.Lines.Resource_Type;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The GPU data resources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Clusters : Cluster_Array;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- When the buffer reaches this size (or a bit more), the cluster is no
   -- longer updated and a new one is allocated.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Size_Threshold : constant Natural := 30;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the cluster should be updated due to one or more new points
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Point_Pending : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reset_Pending : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The last point loaded in the trajectory
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Position : Position_Record := No_Position_Record;

   Last_Load     : Times;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Trajectory_Point is
   begin

      if Data.Is_Update (Field_Position) then

         -- TODO: use Move_To when the previous point is too far or too old

         Trajectory.Line_To (Float (Data.Position.Lon),
                             Float (Data.Position.Lat));

         Last_Position := Data.Position;

         Point_Pending := True;

      end if;

   end Load_Trajectory_Point;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Check_Trajectory_Reset is
   begin

      if Reset_Pending then

         for Cluster of Clusters loop

            Cluster.Clear;

         end loop;

         Reset_Pending := False;

         Point_Pending := False;

         Utility.Log.Put_Message ("trajectory reset");

      end if;

   end Check_Trajectory_Reset;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Reset_Pending is
   begin

      Reset_Pending := True;

   end Set_Reset_Pending;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is

      Airplane_Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (3);

   begin

    --Flight.On_Data_Cached.Connect (Load_Trajectory_Point'Access);

      Airplane_Buffer.Move_To ( 0.00, 0.20);
      Airplane_Buffer.Line_To ( 0.00,-0.50);

      Airplane_Buffer.Move_To ( 0.50, 0.00);
      Airplane_Buffer.Line_To (-0.50, 0.00);

      Airplane_Buffer.Move_To ( 0.15,-0.50);
      Airplane_Buffer.Line_To (-0.15,-0.50);

      Airplane.Load (Airplane_Buffer);

      -- Listen to data reset (replay and other cases)
      ----------------------------------------------------
      On_Data_Reset.Connect (Set_Reset_Pending'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Flush_Path is
   begin

      Check_Trajectory_Reset;

      if Point_Pending then

         -- Load the buffer
         -----------------------------------------------

         Clusters (C).Load (Trajectory);

         Point_Pending := False;

         -- Jump to the next cluster if necessary
         -----------------------------------------------
         if Trajectory.Line_Count >= Size_Threshold then

            Trajectory.Reset;

            Trajectory.Move_To (Float (Last_Position.Lon),
                                Float (Last_Position.Lat));

            --Utility.Log.Put_Message ("new cluster" & Cluster_Range'Image (C));

            if C = Cluster_Range'Last then
               C := 1;
            else
               C := C + 1;
            end if;

         end if;

      end if;

   end Flush_Path;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw_Trajectory (View : Map_View_Record) is
   begin

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      Check_Trajectory_Reset;

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      for I in Cluster_Range loop

         Clusters (I).Draw (Color_Gray_2, 0.1);

      end loop;

      Glex.Get_Transform.Load_Unit;

   end Draw_Trajectory;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- TODO: build route out of the drawing procedure
   --===========================================================================
   procedure Draw_Route (View : Map_View_Record) is

      use Glex.Colors;
      use Glex.Fonts;
      use Utility.Strings;

      Point  : Point_Record;
      Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (Flight_Plan.Waypoints'Length);

   begin

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      for I in Flight_Plan.Waypoints'Range loop

         exit when not Flight_Plan.Waypoints (I).Is_Loaded;

         Point := Maps.Position_To_Map (Flight_Plan.Waypoints (I).Position);

         Buffer.Line_To (Float (Point.Get_X),
                         Float (Point.Get_Y));

      end loop;

      Buffer.Finish;

      Route.Load (Buffer);

      Route.Draw (Line_White,
                  0.003 * View.Zoom,
                  0.001 * View.Zoom);

      Glex.Get_Transform.Load_Unit;

   end Draw_Route;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Draws the waypoint names
   --===========================================================================
   procedure Draw_Waypoints (View : Map_View_Record) is

      use Glex.Colors;
      use Glex.Fonts;
      use Utility.Strings;

      Line   : Line_Color_Record;
      Point  : Point_Record;
      X, Y   : Float;
      Font   : Font_Style_Record := (Width     => 0.008,
                                     Height    => 0.022,
                                     Space     => 0.005,
                                     Rendering => Glex.Fonts.Font_Glow,
                                     Thickness => Glex.Fonts.Font_Bold);

   begin

      Glex.Get_Transform.Load_Unit;

      for I in Flight_Plan.Waypoints'Range loop

         exit when not Flight_Plan.Waypoints (I).Is_Loaded;

         View.Position_To_Screen (Flight_Plan.Waypoints (I).Position, X, Y);

         -- Waypoint name label
         ---------------------------------------
         if I = Waypoint_Range'First then

            Line := Line_Grass;

         elsif Flight_Plan.Waypoints (I).Is_Active then

            Line := Line_Magenta;

         else
            Line := Line_Cyan;

         end if;

         Glex.Fonts.Draw (Trim (Flight_Plan.Waypoints (I).Name),
                          X,
                          Y + 0.02,
                          Font,
                          Line,
                          Alignment_LC);

      end loop;

   end Draw_Waypoints;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Airplane (View     : Map_View_Record;
                            Position : Position_Record;
                            Course   : Float;
                            Color    : Color_Record := Color_White;
                            Shadow   : Color_Record := Color_Black) is

      use Ada.Numerics;

      Point     : Point_Record;
      Transform : Glex.Transform_Record := Glex.Get_Transform.all;
      X, Y      : Float;
      Angle     : Float := Course;
      Size      : constant Float := 0.07;

   begin

      Angle := - Course * Float (Math.Pi) / 180.0;

      View.Position_To_Screen (Position, X, Y);

      Glex.Get_Transform.Translate (X, Y);

      Glex.Get_Transform.Scale     (Size, Size * Glex.Aspect);

      Glex.Get_Transform.Rotate    (Angle);

      Airplane.Draw ((Shadow, Color), 0.06, 0.03, 1.0);

      Glex.Get_Transform.Load_Unit;

   end Draw_Airplane; pragma Inline (Draw_Airplane);
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Airplane (View : Map_View_Record) is
   begin

      Draw_Airplane (View, Flight.Data.Position, Flight.Data.Course);

   end Draw_Airplane;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Traffic (View : Map_View_Record) is

      use Flight.Traffic;

      Color : Color_Record;

   begin

      for T in Traffic_Range loop

         if Traffic_Data (T).Active then

            if Traffic_Data (T).Coasted then

               Color := Color_White;

            else

               Color := Color_Reddish;

            end if;

            Draw_Airplane (View,
                           Traffic_Data (T).Position,
                           Traffic_Data (T).Course,
                           Color,
                           Color_Gray_4);

         end if;

      end loop;

   end Draw_Traffic;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw_Vectors (View : Map_View_Record) is

      use Glex.Fonts;
      use Utility.Strings;

      Line_Color_Waypoint : constant Line_Color_Record := (Color_Magenta, Color_Gray_3);
      Line_Color_Home     : constant Line_Color_Record := (Color_Green,   Color_Gray_3);

      Point  : Point_Record;
      Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (1);

   begin

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      if Flight.Data.Position /= No_Position_Record then

         if Flight.Plan.Next_Waypoint.Position /= Flight.Data.Position then

            Point := Maps.Position_To_Map (Flight.Plan.Next_Waypoint.Position);

            Buffer.Move_To (Float (Point.Get_X),
                            Float (Point.Get_Y));

            Point := Maps.Position_To_Map (Flight.Data.Position);

            Buffer.Line_To (Float (Point.Get_X),
                            Float (Point.Get_Y));

            Vector_Waypoint.Load (Buffer);

            Vector_Waypoint.Draw (Line_Color_Waypoint,
                                  0.005 * View.Zoom,
                                  0.002 * View.Zoom);

         end if;

         if Flight.Plan.Home_Waypoint.Position /= Flight.Data.Position then

            Buffer.Reset;

            Point := Maps.Position_To_Map (Flight.Plan.Home_Waypoint.Position);

            Buffer.Move_To (Float (Point.Get_X),
                            Float (Point.Get_Y));

            Point := Maps.Position_To_Map (Flight.Data.Position);

            Buffer.Line_To (Float (Point.Get_X),
                            Float (Point.Get_Y));

            Vector_Home.Load (Buffer);

            Vector_Home.Draw (Line_Color_Home,
                              0.005 * View.Zoom,
                              0.002 * View.Zoom);

         end if;

      end if;

      Glex.Get_Transform.Load_Unit;

   end Draw_Vectors;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw (View : Map_View_Record) is
   begin

      Draw_Trajectory (View);

      Draw_Route      (View);

      Draw_Vectors    (View);

      Draw_Waypoints  (View);

      Draw_Traffic    (View);

      Draw_Airplane   (View);

   end Draw;
   -----------------------------------------------------------------------------

end Flight.Representation;
--------------------------------------------------------------------------------
