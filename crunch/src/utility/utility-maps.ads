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
with Math.Vector2;
use  Math.Vector2;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Utility.Maps is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The length in Km of a meridian degree at the Equator in WGS84
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Meridian_Length : constant Long_Float := 110.5800;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The length in Km of a parallel degree along the Equator in WGS84
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Equator_Length  : constant Long_Float := 111.3195;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Position_Record is record

      Lat : Long_Float;

      Lon : Long_Float;

   end record;

   --===========================================================================
   --
   --===========================================================================
   function Lat_Image (Value : Position_Record) return String;

   --===========================================================================
   --
   --===========================================================================
   function Lon_Image (Value : Position_Record) return String;

   --===========================================================================
   --
   --===========================================================================
   function Image (Value : Position_Record) return String;

   --===========================================================================
   --
   --===========================================================================
   function Value (Image : String) return Position_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Position_Record : constant Position_Record := (0.5 * Math.Pi, 0.5 * Math.Pi);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Position_Record_Array is array (Positive range <>) of Position_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Point_Record is Vector2_Record;

   --===========================================================================
   -- Returns the distance in kilometers.
   -- NOTE: the algorithm is simple and is only accurate for close points.
   --===========================================================================
   function Distance (Position_A, Position_B : Position_Record) return Float;

   --===========================================================================
   -- Returns the ratio between the latitudinal and longitudinal degree arcs at
   -- the given position on the earth.
   --===========================================================================
   function Shrinkage (Latitude : Long_Float) return Long_Float;

   --===========================================================================
   -- Returns the aproximate distance and bearing from point A to B.
   -- NOTE: the distance is in kilometers
   --===========================================================================
   procedure Coordinates (Position_A,
                          Position_B  : Position_Record;
                          Distance    : out Float;
                          Bearing     : out Float);

   --===========================================================================
   -- Returns the aproximate distance and bearing from point A to B.
   -- NOTE: the distance is in kilometers
   --===========================================================================
   function Vector (Position_A,
                    Position_B : Position_Record) return Vector2_Record ;

   --===========================================================================
   -- Returns the aproximate distance and bearing from point A to B.
   -- NOTE: the distance is in kilometers
   --===========================================================================
   function Vector (Position_A,
                    Position_B : Position_Record;
                    Scale      : Float) return Vector2_Record ;

   --===========================================================================
   -- Returns the aproximate position of the location pointed by the vector from
   -- the given reference.
   --===========================================================================
   function Position (Reference : Position_Record;
                      Vector    : Vector2_Record) return Position_Record;

end Utility.Maps;
--------------------------------------------------------------------------------
