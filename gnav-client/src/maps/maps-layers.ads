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
use  Glex;
with Glex.Basic;
with Glex.Colors;
use  Glex.Colors;
with Utility.Units;
use  Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps.Layers is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kind of layers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Layer_Kinds is (Layer_Rivers,
                        Layer_Lakes,
                        Layer_Rails,
                        Layer_Roads,
                        Layer_Borders,
                        Layer_Unknown);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Visible : array (Layer_Kinds) of Boolean := (others => True);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Colors : array (Layer_Kinds) of Glex.Colors.Color_Record := (Layer_Rivers    => Color_Water,
                                                                Layer_Lakes     => Color_Water,
                                                                Layer_Rails     => Color_Gray_3,
                                                                Layer_Roads     => Color_Gray_4,
                                                                Layer_Borders   => Color_Red,
                                                                Layer_Unknown   => Color_Yellow);

   --===========================================================================
   -- Initializes the data and launches the loader.
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Draws the map layers in map coordinates
   --===========================================================================
   procedure Draw (View : Map_View_Record);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A part in a layer (Eg.: a sector, a river, a railroad)
   -- The points are optional and used for calculations. This means they
   -- must be checked against null before usage.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Record is limited record

      Kind       : Layer_Kinds := Layer_Unknown;

      Loaded     : Boolean     := False;

      Base       : Glex.Basic.Resource_Type;

      North_East : Position_Record;

      South_West : Position_Record;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of all layers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Parts : array (1..1000) of Part_Record;



   --type Cluster_Range is range 0..1000;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A geographic region containing part records
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --type Cluster_Record is limited record

   --   Kind       : Layer_Kinds     := Layer_Unknown;

   --   First      : Cluster_Range   := 0;

   --   Last       : Cluster_Range   := 0;

   --   North_East : Position_Record := No_Position_Record;

   --   South_West : Position_Record := No_Position_Record;

   --end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A geographic region containing part records
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --Clusters : array (1..500) of Region_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --Parts : array (1..1000) of Glex.Basic.Resource_Type;

end Maps.Layers;
--------------------------------------------------------------------------------
