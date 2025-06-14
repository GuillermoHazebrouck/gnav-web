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
with Utility.Log;
with Utility.Storage;
with Utility.Strings;
with Utility.Units;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Flight.Wind is


   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Read_Wind is

      Value  : String := Utility.Storage.Get_Item ("WIND");

   begin

      if Value /= "" then

         declare
            use Utility.Strings;
            Reader : String_Buffer (Value'Length);
         begin

            Reader.Load (Value);

            Last_Manual_Wind.Set (Long_Float (Float_Value (Reader.Read_Next ('>'), 0.0)),
                                  Long_Float (Float_Value (Reader.Read_Next ('>'), 0.0)));

            Last_Metar_Wind.Set (Long_Float (Float_Value (Reader.Read_Next ('>'), 0.0)),
                                 Long_Float (Float_Value (Reader.Read_Next ('>'), 0.0)));

         end;

      end if;

   end Read_Wind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Save_Wind is

      use Utility.Strings;

   begin

      Utility.Storage.Set_Item ("WIND",
                                Float_Image (Float (Last_Manual_Wind.Get_X), 1) & ">" & Float_Image (Float (Last_Manual_Wind.Get_Y), 1) & ">" &
                                Float_Image (Float (Last_Metar_Wind.Get_X),  1) & ">" & Float_Image (Float (Last_Metar_Wind.Get_Y),  1));

   end Save_Wind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Metar_Wind (Speed, Course : Float; Time : Times) is

      use Utility.Strings;
      use Utility.Units;

      Wind_Speed,
      Wind_Course : Float;

      Wind : Vector2_Record;

   begin

      Wind_Course := (270.0 - Course) / 180.0 * Float (Math.Pi);

      Wind_Speed  := Convert (Speed, Unit_Kilometer_Hour, Unit_Meter_Second);

      Wind.Set_From_Polar (Long_Float (Wind_Course),
                           Long_Float (Wind_Speed));

      Last_Metar_Wind      := Wind;

      Last_Metar_Wind_Time := Time;

      if Source = Wind_Source_Metar then

         Data.Wind := Last_Metar_Wind;

         Data.Ages   (Field_Wind) := Last_Metar_Wind_Time;

         Data.Origin (Field_Wind) := Update_External;

      end if;

   end Set_Metar_Wind;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Manual_Wind (Wind : Vector2_Record) is
   begin

      Last_Manual_Wind := Wind;

      Last_Manual_Wind_Time := Cached_Time;

      if Source = Wind_Source_Manual then

         Data.Wind := Last_Manual_Wind;

         Data.Ages   (Field_Wind) := Last_Manual_Wind_Time;

         Data.Origin (Field_Wind) := Update_Internal;

      end if;

   end Set_Manual_Wind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Wind is
   begin

      --------------------------------------------------------------------------
      -- Decide which value to use based on the setup
      --------------------------------------------------------------------------
      case Source is

         when Wind_Source_Computation =>

            Data.Wind := Last_Computed_Wind;

            Data.Ages   (Field_Wind) := Last_Computed_Wind_Time;

            Data.Origin (Field_Wind) := Update_Internal;

         when Wind_Source_Manual =>

            Data.Wind := Last_Manual_Wind;

            Data.Ages   (Field_Wind) := Last_Manual_Wind_Time;

            Data.Origin (Field_Wind) := Update_Internal;

         when Wind_Source_Metar =>

            Data.Wind := Last_Metar_Wind;

            Data.Ages   (Field_Wind) := Last_Metar_Wind_Time;

            Data.Origin (Field_Wind) := Update_External;

      end case;

   end Load_Wind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Source (Value : Wind_Source_Mode) is
   begin

      if Source /= Value then

         Source := Value;

         Load_Wind;

      end if;

   end Set_Source;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Gets the source
   --===========================================================================
   function Get_Source return Wind_Source_Mode is
   begin

      return Source;

   end Get_Source;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compute_Wind is
   begin

      Utility.Log.Put_Message ("wind computation launched " & Data.Wind.Image);

      --------------------------------------------------------------------------
      -- Always make an own wind computation, so that it is available
      -- during the flight as first or second resource
      --------------------------------------------------------------------------
      case Computation_Mode is

         when Wind_Computation_Differential =>

            if
              Data.Is_Update (Field_Airspeed) and
              Data.Is_Update (Field_Speed)    and
              Data.Is_Update (Field_Course)   and
              Data.Is_Update (Field_Heading)
            then

               null;

            end if;

         when Wind_Computation_Path_Drift =>

            -- Go back in the history and check if we have turned more than
            -- 360 deg in (quasi) free motion. Then use homologous
            -- trajectory points to compute the trayectory drift.
            --
            -- NOTE:
            -- > The turn-rate must remain above a 8 deg/s.
            -- > The turn-rate must not vary more thant 20%.
            --
            -- NOTE:
            -- > The algorithm is simple and might not always be accurate,
            --   specially when there are large corrections. The calculation
            --   will stop if the trajectory is corrected considerably, but
            --   small corrections might pollute the result inadvertently.
            -- > The algorithm might not work in strong wind (high path
            --   curvature expected).
            -- > In simulations the algorithm seems to work quite well.
            -- > A better algorithm might be trying to fit a cicloid using
            --   least squares.
            --
            -----------------------------------------------------------------

            declare

               H         : History_Range  := Current;
               J         : Natural        := 0;
               N         : Long_Float     := 0.0;
               Angle     : Long_Float     := 0.0; -- deg
               Span      : Lapses         := No_Lapse; -- s
               Deviation : Float          := 0.0; -- deg
               Min_Turn  : constant Float := 8.0; -- deg/s
               V1, V2    : Vector2_Record;        -- m/s

            begin

               if
                 Data.Is_Update (Field_Turn) and
                 Data.Is_Update (Field_Course)
               then

                  if abs Data.Turn < Min_Turn then

                     -- The turn rate is too slow for wind computation
                     --------------------------------------------------------

                     return;

                  end if;

                  V1.Set_From_Polar (Long_Float (Data.Course) / 180.0 * Math.Pi, 1.0);

                  -- Check if the turn has been maintained for the
                  -- necessary time span to make two complete turns
                  -----------------------------------------------------------

                  for K in History_Range loop

                     Flight.Get_Previous_Index (H);

                     -- Skip data that is not valid
                     --------------------------------------------------------
                     if
                       History (H).Is_Update (Field_Turn)   and
                       History (H).Is_Update (Field_Course) and
                       History (H).Turn > Min_Turn
                     then

                        Deviation := abs ((Data.Turn - History (H).Turn) / Data.Turn);

                        V2.Set_From_Polar (Long_Float (History (H).Course) / 180.0 * Math.Pi, 1.0);

                        Angle := Angle + V1.Angle (V2) * 180.0 / Math.Pi;

                        V1 := V2;

                        Span := Data.Timestamp - History (H).Timestamp;

                        exit when abs Angle > 360.0 or Deviation > 0.2;

                     end if;

                  end loop;

                  if Span > No_Lapse and abs Angle > 360.0 and Deviation < 0.2 then

                     Data.Blow := Vector (History (H).Position, Data.Position);

                     Data.Blow.Scale (Long_Float (1000.0 / Seconds (Span)));

                     Data.Ages   (Field_Blow) := Cached_Time;

                     Data.Origin (Field_Blow) := Update_Internal;

                  end if;

                  -- Take the mean blow during the last 5 minutes
                  -----------------------------------------------------------

                  if Data.Is_Update (Field_Blow) then

                     H := Current;

                     V1 := Data.Blow;

                     N  := 1.0;

                     for K in History_Range loop

                        Get_Previous_Index (H);

                        -- Skip data that is not valid
                        -----------------------------------------------------
                        if History (H).Is_Update (Field_Blow) then

                           V1 := V1 + History (H).Blow;

                           N  := N + 1.0;

                           Span := Data.Timestamp - History (H).Timestamp;

                           exit when Span > Lapse_Of (300.0);

                        end if;

                     end loop;

                     if N > 0.0 then

                        Last_Computed_Wind := V1;

                        Last_Computed_Wind.Scale (1.0 / N);

                        Last_Computed_Wind_Time := Cached_Time;

                     end if;

                  end if;

               end if;

            end;

         when others =>

            null;

      end case;

      Load_Wind;

      Utility.Log.Put_Message ("wind computation done " & Data.Wind.Image);

   end Compute_Wind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Wind return Vector2_Record is
   begin

      --------------------------------------------------------------------------
      -- Decide which value to use based on the setup
      --------------------------------------------------------------------------
      case Source is

         when Wind_Source_Computation =>

            return Last_Computed_Wind;

         when Wind_Source_Manual =>

            return Last_Manual_Wind;

         when Wind_Source_Metar =>

            return Last_Metar_Wind;

      end case;

   end Get_Wind;
   -----------------------------------------------------------------------------

end Flight.Wind;
--------------------------------------------------------------------------------
