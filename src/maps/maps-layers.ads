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
   type Layer_Kinds is (Layer_Unknown,
                        Layer_Rivers,
                        Layer_Lakes,
                        Layer_Rails,
                        Layer_Roads,
                        Layer_Borders,
                        Layer_Airspaces);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Visible : array (Layer_Kinds) of Boolean := (Layer_Unknown => False, others => True);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Colors : array (Layer_Kinds) of Glex.Colors.Color_Record := (Layer_Unknown   => Color_White,
                                                                Layer_Rivers    => Color_Water,
                                                                Layer_Lakes     => Color_Water,
                                                                Layer_Rails     => Color_Gray_3,
                                                                Layer_Roads     => Color_Gray_4,
                                                                Layer_Borders   => Color_Red,
                                                                Layer_Airspaces => Color_Blue);

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
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Part_Names is String (1..20);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Part_Name : constant Part_Names := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Part_Infos is String (1..40);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Part_Infos : constant Part_Infos := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A part in a layer (Eg.: a sector, a river, a railroad)
   -- The points are optional and used for calculations. This means they
   -- must be checked against null before usage.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Record is tagged limited record

      Kind       : Layer_Kinds := Layer_Unknown;

      Loaded     : Boolean := False;

      Base       : Glex.Basic.Resource_Type;

      North_East : Position_Record;

      South_West : Position_Record;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of all layers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Parts : array (1..400) of Part_Record;

end Maps.Layers;
--------------------------------------------------------------------------------
