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
with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Maps is

    package Math renames Ada.Numerics.Elementary_Functions;

    Deg_Chr : constant Character := '*';
    Min_Chr : constant Character := ''';
    Seg_Chr : constant Character := '"';
    Dec_Chr : constant Character := '.';

    Deg_Str : constant String := (1 => Deg_Chr);
    Min_Str : constant String := (1 => Min_Chr);
    Seg_Str : constant String := (1 => Seg_Chr);
    Dec_Str : constant String := (1 => Dec_Chr);




   --===========================================================================
   --
   --===========================================================================
   function Sexagecimal_String (Value : Long_Float) return String is

      Degrees : Long_Float := Long_Float'Floor (abs Value);
      Minutes : Long_Float := 60.0 * (abs Value - Degrees);
      Seconds : Long_Float := 60.0 * (Minutes - Long_Float'Floor (Minutes));
      Decimal : Long_Float := 10.0 * (Seconds - Long_Float'Floor (Seconds));

   begin

      Minutes := Long_Float'Truncation (Minutes);

      return Trim (Integer'Image (Integer (Long_Float'Floor (Degrees)))) & Deg_Str &
             Trim (Integer'Image (Integer (Long_Float'Floor (Minutes)))) & Min_Str &
             Trim (Integer'Image (Integer (Long_Float'Floor (Seconds)))) & Dec_Str &
             Trim (Integer'Image (Integer (Long_Float'Floor (Decimal)))) & Seg_Str;

   end Sexagecimal_String;
   --------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Lat_String (Value : Long_Float) return String is
   begin
      if Value < 0.0 then
         return "S";
      else
         return "N";
      end if;
   end Lat_String;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Lon_String (Value : Long_Float) return String is
   begin
      if Value < 0.0 then
         return "W";
      else
         return "E";
      end if;
   end Lon_String;
   -----------------------------------------------------------------------------


   --===========================================================================
   --
   --===========================================================================
   function Lat_Image (Value : Position_Record) return String is
   begin
      return Lat_String (Value.Lat) & Sexagecimal_String (Value.Lat);
   end Lat_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Lon_Image (Value : Position_Record) return String is
   begin

      return Lon_String (Value.Lon) & Sexagecimal_String (Value.Lon);

   end Lon_Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Image (Value : Position_Record) return String is
   begin

      return Lat_Image (Value) & " " & Lon_Image (Value);

   end Image;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Sexagecimal_Value (Image : String) return Long_Float is

      J      : Natural    := 0;
      Sign   : Long_Float := 0.0;
      Result : Long_Float := 0.0;

   begin

      for I in Image'First..Image'Last loop

         if Image (I) = 'N' or Image (I) = 'E' then

            Sign :=  1.0;

            J := I + 1;

         elsif Image (I) = 'W' or Image (I) = 'S' then

            Sign := -1.0;

            J := I + 1;

         elsif Image (I) = Deg_Chr and I > J then

            Result := Long_Float'Value (Image (J..I - 1));

            J := I + 1;

         elsif Image (I) = Min_Chr and I > J then

            Result := Result + Long_Float'Value (Image (J..I - 1)) / 60.0;

            J := I + 1;

         elsif Image (I) = Seg_Chr and I > J then

            Result := Result + Long_Float'Value (Image (J..I - 1)) / 3600.0;

            exit;

         end if;

      end loop;

      return Sign * Result;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Value (Image : String) return Position_Record is

      Result : Position_Record := (0.0, 0.0);
      J      : Positive := Image'First;

   begin

      for I in Image'First..Image'Last loop

         if Image (I) = ' ' then

            J := I;

            exit;

         end if;

      end loop;

      if J > Image'First and J < Image'Last then

         Result.Lat := Sexagecimal_Value (Image (Image'First..J-1));

         Result.Lon := Sexagecimal_Value (Image (J+1..Image'Last));

      end if;

      return Result;

   end Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Distance (Position_A, Position_B : Position_Record) return Float is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Delta_Lat : Long_Float := Position_B.Lat - Position_A.Lat;
      Delta_Lon : Long_Float := Position_B.Lon - Position_A.Lon;
      Midle_Lat : Long_Float := (Position_B.Lat + Position_A.Lat) * Pi / 360.0;

      Delta_X, Delta_Y : Float;

   begin

      Delta_Y := Float (Meridian_Length * Delta_Lat);

      Delta_X := Float (Equator_Length * Cos (Midle_Lat) * Delta_Lon);

      return Sqrt (Delta_X * Delta_X + Delta_Y * Delta_Y);

   end Distance;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Shrinkage (Latitude : Long_Float) return Long_Float is

      use Ada.Numerics;
      use Ada.Numerics.Long_Elementary_Functions;

   begin

      return Meridian_Length / (Equator_Length * Cos (Latitude * Pi / 180.0));

   end Shrinkage;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Coordinates (Position_A,
                          Position_B  : Position_Record;
                          Distance    : out Float;
                          Bearing     : out Float) is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Delta_Lat : Long_Float := Position_B.Lat - Position_A.Lat;
      Delta_Lon : Long_Float := Position_B.Lon - Position_A.Lon;
      Midle_Lat : Long_Float := (Position_B.Lat + Position_A.Lat) * Pi / 360.0;

      Delta_X, Delta_Y : Float;

   begin

      Delta_Y  := Float (Meridian_Length * Delta_Lat);

      Delta_X  := Float (Equator_Length  * Cos (Midle_Lat) * Delta_Lon);

      Distance := Sqrt (Delta_X * Delta_X + Delta_Y * Delta_Y);

      if Delta_X = 0.0 and then Delta_Y = 0.0 then

         Bearing := 0.0;

      else

         Bearing  := 90.0 - Arctan (Delta_Y, Delta_X) * 180.0 / Pi;

         if Bearing < 0.0 then

            Bearing := Bearing + 360.0;

         end if;

      end if;

   exception
      when others =>

         Ada.Text_IO.Put_Line ("error while calculating coordinates");

   end Coordinates;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Vector (Position_A, Position_B : Position_Record) return Vector2_Record is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Delta_Lat : Long_Float :=  Position_B.Lat - Position_A.Lat;

      Delta_Lon : Long_Float :=  Position_B.Lon - Position_A.Lon;

      Parallel_Length : Long_Float := Equator_Length * Cos (Position_A.Lat * Pi / 180.0);

      Vector : Vector2_Record;

   begin

      Vector.Set_X (Delta_Lon * Parallel_Length);

      Vector.Set_Y (Delta_Lat * Meridian_Length);

      return Vector;

   end Vector;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Vector (Position_A,
                    Position_B : Position_Record;
                    Scale      : Float) return Vector2_Record is

      V : Vector2_Record := Vector (Position_A, Position_B);

   begin

      V.Scale (Long_Float (Scale));

      return V;

   end Vector;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Position (Reference : Position_Record;
                      Vector    : Vector2_Record) return Position_Record is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;
      use Ada.Numerics.Long_Elementary_Functions;

      Parallel_Length : Long_Float := Equator_Length * Cos (Reference.Lat * Pi / 180.0);

      Position : Position_Record;

   begin

      Position.Lat := Reference.Lat + Vector.Get_Y / Meridian_Length;

      Position.Lon := Reference.Lon + Vector.Get_X / Parallel_Length;

      return Position;

   end Position;
   -----------------------------------------------------------------------------

end Utility.Maps;
--------------------------------------------------------------------------------
