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

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of clusters
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Cluster_Range is Positive range 1..30;

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
   Cluster_Size : constant Natural := 30;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The buffer containing the last cluster
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Trajectory : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (Cluster_Size);

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

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates if the trajectory must be cut until the position is more stable
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Wait_For_Stable_Position : Boolean := True;

   --===========================================================================
   -- Loads the current position in the trajectory. If the data is varying too
   -- much (by 50%), it cuts the line and wait until it stabilizes to avoid
   -- inconsistent jumps. This has been observed when the application is resumed
   -- after being in the background.
   --===========================================================================
   procedure Load_Trajectory_Point is

      Distance_Threshol : constant Float := 0.1; -- (this is 100m)

   begin

      if
        Data.Is_Update (Field_Position) and then
        Data.Position /= Last_Position
      then

         declare
            D : Float := Distance  (Data.Position, Last_Position);
            V : Float := Variation (Field_Position);
            S : Float := Step      (Field_Position);
            P : Point_Record;
         begin

            if Wait_For_Stable_Position then

               if V < 0.5 and S < Distance_Threshol then

                  Wait_For_Stable_Position := False;

                  P := Maps.Position_To_Map (Data.Position);

                  Trajectory.Move_To (Float (P.Get_X),
                                      Float (P.Get_Y));

                  Last_Position := Data.Position;

               end if;

            elsif D > Distance_Threshol then

               if V > 0.8 or S > Distance_Threshol then

                  Wait_For_Stable_Position := True;

               else

                  P := Maps.Position_To_Map (Data.Position);

                  Trajectory.Line_To (Float (P.Get_X),
                                      Float (P.Get_Y));

                  Last_Position := Data.Position;

                  Point_Pending := True;

               end if;

            end if;

         end;

      end if;

      Flush_Path;

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

      Airplane_Buffer.Move_To ( 0.00, 0.20);
      Airplane_Buffer.Line_To ( 0.00,-0.50);

      Airplane_Buffer.Move_To ( 0.50, 0.00);
      Airplane_Buffer.Line_To (-0.50, 0.00);

      Airplane_Buffer.Move_To ( 0.15,-0.50);
      Airplane_Buffer.Line_To (-0.15,-0.50);

      Airplane.Load (Airplane_Buffer);

      -- Listen to every data cache
      ----------------------------------------------------
      Flight.On_Data_Cached.Connect (Load_Trajectory_Point'Access);

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
         if Trajectory.Line_Count >= Cluster_Size then

            Trajectory.Reset;

            declare
               P : Point_Record := Maps.Position_To_Map (Data.Position);
            begin

               Trajectory.Move_To (Float (P.Get_X),
                                   Float (P.Get_Y));

            end;

            -- Recycle buffer when reached the end
            -----------------------------------------------
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

      Check_Trajectory_Reset;

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      for I in Cluster_Range loop

         Clusters (I).Draw (Color_Gray_2, 0.002 * View.Zoom);

      end loop;

      Glex.Get_Transform.Load_Unit;

   end Draw_Trajectory;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- TODO: build route out of the drawing procedure
   --===========================================================================
   procedure Draw_Route (View : Map_View_Record) is

      use Glex.Colors;
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

      Line : Line_Color_Record;
      X, Y : Float;
      Font : Font_Style_Record := (Width     => 0.008,
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
                            Shadow   : Color_Record := Color_Black;
                            Size     : Float := 0.07) is

      use Ada.Numerics;

      Point     : Point_Record;
      Transform : Glex.Transform_Record := Glex.Get_Transform.all;
      X, Y      : Float;
      Angle     : Float := Course;

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
      use Glex.Fonts;
      use Utility.Strings;

      X, Y  : Float;
      Line  : Line_Color_Record;
      Font  : Font_Style_Record := (Width     => 0.007,
                                    Height    => 0.020,
                                    Space     => 0.004,
                                    Rendering => Glex.Fonts.Font_Glow,
                                    Thickness => Glex.Fonts.Font_Regular);

      Color : Color_Record;

   begin

      Maintain_Tracks;

      if Flight.Traffic.Get_Status = Aprs_Nominal then

         for T in Traffic_Data'Range loop

            if Traffic_Data (T).Active then

               if Traffic_Data (T).Coasted then

                  Color := Color_Pink;

               else

                  Color := Color_Yellow;

               end if;

               Draw_Airplane (View,
                              Traffic_Data (T).Position,
                              Traffic_Data (T).Course,
                              Color_Black,
                              Color,
                              0.04);

               View.Position_To_Screen (Traffic_Data (T).Position, X, Y);

               Glex.Fonts.Draw (Integer_Image (Traffic_Data (T).Altitude),
                                X,
                                Y - 0.080,
                                Font,
                                Line_White,
                                Alignment_CC);

               if abs Traffic_Data (T).Vario > 0.05 then

                  if Traffic_Data (T).Vario > 0.0 then
                     Line := Line_Green;
                  else
                     Line := Line_Red;
                  end if;

                  Glex.Fonts.Draw (Float_Image (Traffic_Data (T).Vario, 1),
                                   X,
                                   Y + 0.080,
                                   Font,
                                   Line,
                                   Alignment_CC);

               end if;

            end if;

         end loop;

      end if;

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
