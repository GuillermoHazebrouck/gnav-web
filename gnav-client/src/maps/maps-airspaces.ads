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

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps.Airspaces is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Allow_Notifications : Boolean := False;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Airspace_Kinds is (Airspace_Ctr, -- Control tower
                           Airspace_Tma, -- Terminal area
                           Airspace_Tmz, -- Transponder or radio mandatory
                           Airspace_Cta, -- Enroute area
                           Airspace_Res, -- Restricted area
                           Airspace_Gld, -- Gliding area
                           Airspace_Unk);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Notify_Kinds is (Notify_None,
                         Notify_In,
                         Notify_Out,
                         Notify_In_Out);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Visible : array (Airspace_Kinds) of Boolean := (Airspace_Unk => False, others => True);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which layers must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Colors : constant array (Airspace_Kinds) of Glex.Colors.Color_Record := (Airspace_Ctr => Color_Magenta,
                                                                            Airspace_Tma => Color_Blue,
                                                                            Airspace_Tmz => Color_Orange,
                                                                            Airspace_Cta => Color_Blue,
                                                                            Airspace_Res => Color_Red,
                                                                            Airspace_Gld => Color_Purple,
                                                                            Airspace_Unk => Color_Orange);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The thickness of the areas in latitudinal degrees
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Thickness : constant array (Airspace_Kinds) of Float := (Airspace_Ctr => 0.0034,
                                                            Airspace_Tma => 0.0020,
                                                            Airspace_Tmz => 0.0020,
                                                            Airspace_Cta => 0.0010,
                                                            Airspace_Res => 0.0020,
                                                            Airspace_Gld => 0.0034,
                                                            Airspace_Unk => 0.0020);

   --===========================================================================
   -- Initializes the data and launches the loader.
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Draws the map layers in map coordinates
   --===========================================================================
   procedure Draw (View : Map_View_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Clear_Focus;

   --===========================================================================
   --
   --===========================================================================
   procedure Save_Configuration_If_Changed;

   --===========================================================================
   --
   --===========================================================================
   procedure Save_Master_Alerts_Switch;

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
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Record is limited record

      North_East : Position_Record  := No_Position_Record;

      South_West : Position_Record  := No_Position_Record;

      Resource   : Glex.Lines.Resource_Type;

      First      : Natural          := 0;

      Last       : Natural          := 0;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A part containing a basic map feature.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Airspace_Record is tagged record

      Loaded      : Boolean          := False;

      Notify      : Notify_Kinds     := Notify_None;

      Active      : Boolean          := True;

      Name        : Sector_Names     := No_Sector_Name;

      Lower_Limit : Float            := 0.0;

      Upper_Limit : Float            := 0.0;

      Limits      : Limits_Record    := (No_Altitude_String, No_Altitude_String);

      Kind        : Airspace_Kinds   := Airspace_Unk;

      Class       : Class_Names      := "A";

      Label       : Limits_Record    := (No_Altitude_String, No_Altitude_String);

      North_East  : Position_Record  := No_Position_Record;

      South_West  : Position_Record  := No_Position_Record;

      Center      : Position_Record  := No_Position_Record;

      Flagged     : Boolean          := False; --> optimization flag

      First       : Natural          := 0;     --> first part

      Last        : Natural          := 0;     --> last part

      Inside      : Boolean          := False;

   end record;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function Contains (This : Airspace_Record; Position : Position_Record) return Boolean;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of all airspaces
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Parts     : array (1..500) of Part_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of all airspaces
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Airspaces : array (1..500) of Airspace_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Number_Of_Airspaces : Natural := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Focused_Airspace : Natural := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A static buffer to load the line resources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Lines_Limit  : constant Natural := 100;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A static buffer to load the line resources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Lines_Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (Lines_Limit);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A node
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Node_Record is record

      X : Float;

      Y : Float;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A static buffer containing all nodes (80kB)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Nodes_Buffer : array (1..20_000) of aliased Node_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Use this flag to indicate that there was a change on the configuration
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Status_Changed : Boolean := False;

end Maps.Airspaces;
--------------------------------------------------------------------------------
