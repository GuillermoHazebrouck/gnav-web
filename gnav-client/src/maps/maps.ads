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
with Glex;
with Math.Vector2;
use  Math.Vector2;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The length of a latitudinal degree at the Equator in WGS84 (in km)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Meridian_Length : constant Long_Float := 110.5800;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The length of a longitudinal degree along the Equator in WGS84 (in km)
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
   function Compact_Image (Value : Position_Record) return String;

   --===========================================================================
   --
   --===========================================================================
   function Value (Image : String) return Position_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The north pole
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
   -- Sets the reference point for the system conformal coordinates
   --===========================================================================
   procedure Set_Reference (Position : Position_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Terrain_Modes is (Monochrome, Colormap);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Lower zoom level: 1' on the screen height
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Lower_Zoom : constant Float := 1.0 / 60.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Upper zoom level: 2° on the screen height
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Upper_Zoom : constant Float := 2.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The zoom step: 1' on the screen height
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Zoom_Step : constant Float := 0.08;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The zoom step: in longitudinal minutes on the screen height
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Move_Step : constant Float := 0.08;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Altitude : constant Float := -10000.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The function that returns the estimated arrival altitude at a given position
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Range_Cone_Function : access function (Position : Position_Record) return Float := null;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Data used to represent the map
   -- Center : the intended screen center (geographic coordinates)
   -- Zoom   : the zoom level in number of latitudinal degrees per unit width
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Map_View_Record is tagged record

      X, Y         : Float           := 0.0;
      H, W         : Float           := 1.0;
      Center       : Position_Record := No_Position_Record;
      Zoom         : Float           := 0.0;
      Zero         : Float           := 0.0;
      Mode         : Terrain_Modes   := Colormap;
      Shadow       : Boolean         := False;
      Cone_Margin  : Float           := 200.0;
      Cone_Active  : Boolean         := False;
      Show_Terrain : Boolean         := True;

   end record;

   --===========================================================================
   --
   --===========================================================================
   procedure Zoom_In (This : in out Map_View_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Zoom_Out (This : in out Map_View_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Move_West (This : in out Map_View_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Move_East (This : in out Map_View_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Move_North (This : in out Map_View_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Move_South (This : in out Map_View_Record);

   --===========================================================================
   --
   --===========================================================================
   function Screen_To_Position (This : Map_View_Record; X, Y : Float) return Position_Record;

   --===========================================================================
   --
   --===========================================================================
   procedure Position_To_Screen (This : Map_View_Record; Position : Position_Record; X, Y : out Float);

   --===========================================================================
   -- Returns the limits of the viewing rectangle
   --===========================================================================
   procedure Get_Limits (This       : Map_View_Record;
                         North_East : out Position_Record;
                         South_West : out Position_Record);

   --===========================================================================
   --
   --===========================================================================
   function Position_To_Map (Position : Position_Record) return Point_Record;

   --===========================================================================
   --
   --===========================================================================
   function Map_To_Position (Point : Point_Record) return Position_Record;

   --===========================================================================
   --
   --===========================================================================
   function Get_Geographic_Matrix (This : Map_View_Record) return Glex.Transform_Record;

   --===========================================================================
   -- Indicates if the viewing rectangle contains the given area
   --===========================================================================
   function On_Clip (This       : Map_View_Record;
                     North_East : Position_Record;
                     South_West : Position_Record) return Boolean;

   --===========================================================================
   -- Indicates if the viewing rectangle contains the given screen point
   --===========================================================================
   function On_Clip (This : Map_View_Record; X, Y : Float) return Boolean;

   --===========================================================================
   -- Indicates if the given point is in the viewing area
   --===========================================================================
   function On_Clip (This     : Map_View_Record;
                     Position : Position_Record) return Boolean;

   --===========================================================================
   -- Indicates if the region contains the given point
   --===========================================================================
   function On_Region (Location   : Position_Record;
                       North_East : Position_Record;
                       South_West : Position_Record) return Boolean;

   --===========================================================================
   -- Computes the RGB componets of the terrain colormap at the given position
   --===========================================================================
   procedure Find_Color (This  : Map_View_Record;
                         Point : Position_Record;
                         Z, S,
                         Z_Min,
                         Z_Max : Float;
                         R, G, B : out Float);

   --===========================================================================
   -- Returns the distance in kilometers.
   -- NOTE: the algorithm is simple and is only accurate for close points.
   --===========================================================================
   function Distance (Position_A, Position_B : Position_Record) return Float;

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

   --===========================================================================
   -- Returns the name of the active dataset
   --===========================================================================
   function Get_Dataset_Name return String;

   --===========================================================================
   -- Returns the ratio between a longitudinal and latitudinal unit arcs at the
   -- reference position.
   --===========================================================================
   function Get_Shrinkage return Float;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The center used for conformal coordinates
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Center : Position_Record := (0.0, 0.0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The ratio between longitudinal and latitudinal unit arcs at the center.
   -- This is used for conformal coordinates
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Shrink : Float := 1.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The name of the active dataset (a directory in the local maps/ folder)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Dataset_Name : String (1..10) := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The location of the database containing the active map dataset
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Dataset_Path : String (1..10) := (others => ' ');

end Maps;
--------------------------------------------------------------------------------
