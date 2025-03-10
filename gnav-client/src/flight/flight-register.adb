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
with Flight.Aircraft;
with Maps.Terrain;
with Utility.Notifications;
with Utility.Log;



--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Register is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Maximum lapse for a register
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Register_Max_Lapse : constant Float := 60.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Minimum lapse for a register
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Register_Min_Lapse : constant Float := 10.0;

   --===========================================================================
   -- Completes the flight register based on the current stage
   --===========================================================================
   procedure Update is

      H          : History_Range := Current;
      Sink_Vario : Float := 0.0;
      Sink_Lapse : Float := 0.0;
      Lift_Vario : Float := 0.0;
      Lift_Lapse : Float := 0.0;
      Ti, Tn, Tp : Float := 0.0;
      Td         : Float := 0.0;
      Vario      : Float := 0.0;
      Speed      : Float := 0.0;
      Mean_Speed : Float := 0.0;
      Sink_Speed : Float := 0.0;
      Start,
      Stop       : Times := No_Time;
      R          : Register_Range renames Current_Register;
      P          : Register_Range := R;

   begin

      if
        Data.Is_Update (Field_Vario)                     and then
        Data.Is_Recent (Field_Turn, Turn_Rate_Max_Lapse) and then
        Data.Mode /= Mode_Manouver
      then

         -- Jump to next register if necessary
         ------------------------------------------------------
         if
           (Register (R).Active            and then
            Register (R).Mode =  Data.Mode and then
            Register (R).Span >  Register_Max_Lapse) or else
           (Register (R).Active            and then
            Register (R).Mode /= Data.Mode and then
            Register (R).Span >  Register_Min_Lapse)
         then

            if R = Register_Range'Last then
               R := Register_Range'First;
            else
               R := R + 1;
            end if;

            Register (R) := No_Register_Record;

         end if;

         -- Get previous register index and tail time
         ------------------------------------------------------
         P := R;
         if P = Register_Range'First then
            P := Register_Range'Last;
         else
            P := P - 1;
         end if;

         if Register (P).Active then
            Tp := Seconds (Data.Timestamp - Register (P).Start);
            if Tp < 2.5 then
               return;
            end if;
         end if;

         -- Recompute current state of the flight
         -- NOTE: this could be optimized
         ------------------------------------------------------

         Vario := Data.Vario;
         Speed := Data.Speed;
         Start := Data.Timestamp;

         for K in History_Range loop

            if History (H).Is_Update (Field_Vario) then

               Ti := Seconds (Data.Timestamp - History (H).Timestamp);

               -- Stop when changing mode or when block is too long
               exit when History (H).Mode /= Data.Mode or else Ti > Register_Max_Lapse;

               -- Stop on previous register
               if Register (P).Active then
                  exit when Ti >= Tp;
               end if;

               Td := Ti - Tn;

               if History (H).Vario < 0.0 then

                  Sink_Lapse := Sink_Lapse + Td;

                  Sink_Vario := Sink_Vario + 0.5 * (Vario + History (H).Vario) * Td;

                  Sink_Speed := Sink_Speed + 0.5 * (Speed + History (H).Speed) * Td;

               else

                  Lift_Lapse := Lift_Lapse + Td;

                  Lift_Vario := Lift_Vario + 0.5 * (Vario + History (H).Vario) * Td;

               end if;

               Mean_Speed := Mean_Speed + 0.5 * (Speed + History (H).Speed) * Td;

               Tn    := Ti;
               Vario := History (H).Vario;
               Speed := History (H).Speed;
               Stop  := History (H).Timestamp;

            end if;

            Get_Previous_Index (H);

         end loop;

         if Ti > Register_Min_Lapse then

            Register (R).Active := True;

            Register (R).Start  := Start;

            Register (R).Stop   := Stop;

            Register (R).Mode   := Data.Mode;

            Register (R).Span   := Ti;

            Register (R).Speed  := Mean_Speed / Ti;

            Register (R).Vario  := (Sink_Vario + Lift_Vario) / Ti;

            -- Average sink and gliding ratio
            ---------------------------------
            if Sink_Lapse > 0.0 then

               Sink_Vario := Sink_Vario / Sink_Lapse;

               Sink_Speed := Sink_Speed / Sink_Lapse;

               Register (R).Sink := Sink_Vario;

               if Sink_Speed > 10.0 then --> at least 10m/s
                  Register (R).Ratio := Sink_Vario / Sink_Speed;
               end if;

            else
               Register (R).Sink  := 0.0;
               Register (R).Ratio := 0.0;

            end if;

            -- Average lift
            ---------------------------------
            if Lift_Lapse > 0.0 then

               Lift_Vario := Lift_Vario / Lift_Lapse;

               Register (R).Lift := Lift_Vario;

            else
               Register (R).Lift := 0.0;

            end if;

            Register (R).Course := Data.Course;

            -- The meassured gliding ratio is taken only after a relevant period
            -- of sink, or it won't be reliable.
            --------------------------------------------------------------------
            if Sink_Lapse > Register_Min_Lapse and Register (R).Ratio < 0.0 then

               Meassured_Gliding_Ratio  := Register (R).Ratio;

               Meassured_Gliding_Course := Register (R).Course;

            end if;

         end if;

      end if;

      --------------------------------------------------------------------------
      -- The sink warning indicates that the configured MC value is too
      -- optimistic for the meassured sink ratio during the last leg.
      -- A sound notification is emitted only once when the value goes below the
      -- selected one.
      -- To stop the warning, the MC must be lowered.
      --------------------------------------------------------------------------
      if Meassured_Gliding_Ratio < 0.0 then

         declare
            use Utility.Notifications;
            Sink_Warning_Was_Off   : Boolean := not Sink_Warning;
            Expected_Gliding_Ratio : Float   := Flight.Aircraft.Get_Expected_Gliding_Ratio (Meassured_Gliding_Course);
         begin

            Sink_Warning := Expected_Gliding_Ratio > Meassured_Gliding_Ratio;

            if
              Sink_Warning and then
              Sink_Warning_Was_Off
            then
               Add_Notification (Notify_Sink);
            end if;

         end;

      end if;

   end Update;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Sink_Warning_Active return Boolean is
   begin

      return Sink_Warning;

   end Sink_Warning_Active;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Register (Index : Register_Range) return not null access Register_Record is

      R : Integer := Integer (Current_Register) - Integer (Index) + 1;

   begin

      if R < Integer (Register_Range'First) then

         R := Integer (Register_Range'Last) + R;

      end if;

      return Register (Register_Range (R))'Access;

   end Get_Register;
   -----------------------------------------------------------------------------



end Flight.Register;
--------------------------------------------------------------------------------
