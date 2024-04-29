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
with Utility.Events;
with Utility.Streams;
use  Utility.Streams;
with Glex.Colormap;

--//////////////////////////////////////////////////////////////////////////////
-- This package manages the terrain data and representation. The terrain is
-- loaded in at most 10 regions of maximum 1.000.000 grid nodes.
--//////////////////////////////////////////////////////////////////////////////
package Maps.Terrain is

   --===========================================================================
   -- Draws the map terrain using OpenGL for the given zoom level
   --===========================================================================
   procedure Draw (View : Map_View_Record);

   --===========================================================================
   -- Returns the approximate elevation at the given position
   --===========================================================================
   function Get_Elevation (Position : Position_Record) return Float;

   --===========================================================================
   -- Notifies that the range function has changed
   --===========================================================================
   procedure Notify_Range_Changed;

   --===========================================================================
   -- Returns an info string
   --===========================================================================
   function Get_Info return String;

   --===========================================================================
   -- Indicates when the terrain is loaded
   --===========================================================================
   On_Loaded : Utility.Events.Event_Stack;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Static buffer containing the terrain data grid (20MB)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Terrain_Altitude_Array is array (1..10_000_000) of Short_Integer;

   -- The terrain altitude grid
   -- TODO: rename to Elevation
   --------------------------------
   Altitude : Terrain_Altitude_Array;

   -- South-East corner
   --------------------------------
   South_West : Position_Record := No_Position_Record;

   -- North-West corner
   --------------------------------
   North_East : Position_Record := No_Position_Record;

   -- The middle of this region
   --------------------------------
   Middle : Position_Record := No_Position_Record;

   -- The size of a cell (degrees)
   --------------------------------
   Cell_Size_Lat : Float := 0.0;

   -- The size of a cell (degrees)
   --------------------------------
   Cell_Size_Lon : Float := 0.0;

   -- No data value
   --------------------------------
   No_Data : Float := 0.0;

   -- Number of rows (latitude)
   --------------------------------
   N_Lat : Natural := 0;

   -- Number of columns (longitude)
   --------------------------------
   N_Lon : Natural := 0;

   -- The terrain colormap
   --------------------------------
   Colormap : Glex.Colormap.Resource_Type;

   -- Forces a graphic reload
   --------------------------------
   Reload : Boolean := True;

   -- This area has been loaded
   --------------------------------
   Loaded : Boolean := False;

   -- Indicates if the area is on the clip
   --------------------------------
   On_Clip : Boolean := False;

   -- Lower vertical limit (meters)
   --------------------------------
   Z_Min_Global : Float := 0.0;

   -- Upper vertical limit (meters)
   --------------------------------
   Z_Max_Global : Float := 0.0;

end Maps.Terrain;
--------------------------------------------------------------------------------
