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
with Glex.Lines;
with Glex.Colors;
use  Glex.Colors;
with Utility.Atmosphere;
use  Utility.Atmosphere;
with Utility.Units;
use  Utility.Units;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps.Airspaces is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Airspace_Kinds is (Airspace_Ctr,
                           Airspace_Tma,
                           Airspace_Cta,
                           Airspace_Tsa,
                           Airspace_Tmz,
                           Airspace_Rmz,
                           Airspace_Unk);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Visible : array (Airspace_Kinds) of Boolean := (Airspace_Unk => False, others => True);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Colors : array (Airspace_Kinds) of Glex.Colors.Color_Record := (Airspace_Ctr => Color_Magenta,
                                                                   Airspace_Tma => Color_Blue,
                                                                   Airspace_Cta => Color_Blue,
                                                                   Airspace_Tsa => Color_Red,
                                                                   Airspace_Tmz => Color_Purple,
                                                                   Airspace_Rmz => Color_Green,
                                                                   Airspace_Unk => Color_Orange);

   --===========================================================================
   -- Initializes the data and launches the loader.
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Draws the map layers in map coordinates
   --===========================================================================
   procedure Draw (View : Map_View_Record);

   --===========================================================================
   -- Draws the airspace labels
   --===========================================================================
   procedure Draw_Labels (View : Map_View_Record);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Sector_Names is String (1..20);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Sector_Name : constant Sector_Names := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Class_Names is String (1..1);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Limits_Record is record

      Upper : Altitude_Strings := No_Altitude_String;

      Lower : Altitude_Strings := No_Altitude_String;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A part containing a basic map feature.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Airspace_Record is tagged limited record

      Name       : Sector_Names     := No_Sector_Name;

      Limits     : Limits_Record    := (No_Altitude_String, No_Altitude_String);

      Kind       : Airspace_Kinds   := Airspace_Unk;

      Class      : Class_Names      := "A";

      Label      : Limits_Record    := (No_Altitude_String, No_Altitude_String);

      Point      : Position_Record  := No_Position_Record;

      North_East : Position_Record  := No_Position_Record;

      South_West : Position_Record  := No_Position_Record;

      Loaded     : Boolean          := False;

      Resource   : Glex.Lines.Resource_Type;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of all airspaces
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Airspaces : array (1..50) of Airspace_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A static buffer to load the line resources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Lines_Limit  : constant Natural := 120;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A static buffer to load the line resources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Lines_Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (Lines_Limit);

end Maps.Airspaces;
--------------------------------------------------------------------------------
