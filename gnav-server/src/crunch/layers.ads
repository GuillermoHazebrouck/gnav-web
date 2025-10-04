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
with Ada.Finalization;
-- Gnav
with Math.Vector2;
with Math.Vector2_List;
with Stacks.Generic_List;
with Stacks.Linked;
with Utility.Maps;
use  Utility.Maps;


--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Layers is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The type of layer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Layer_Types is (Layer_Unknown,
                        Layer_River,
                        Layer_Lake,
                        Layer_Rail,
                        Layer_Road,
                        Layer_Border,
                        Layer_Airspace);

   Layer_Keys : constant array (Layer_Types) of Character := (Layer_Airspace => 'A',
                                                              Layer_Border   => 'B',
                                                              Layer_Lake     => 'L',
                                                              Layer_Rail     => 'T',
                                                              Layer_Road     => 'S',
                                                              Layer_River    => 'R',
                                                              Layer_Unknown  => '?');

   --===========================================================================
   -- Loads the shape files for the borders, rivers, etc.
   --===========================================================================
   procedure Load_Shape_Files;

   --===========================================================================
   -- Generates the native G-NAV file
   --===========================================================================
   procedure Compile_Data;

   --===========================================================================
   -- Indicates if there are layers loaded
   --===========================================================================
   function Is_Empty return Boolean;

   --===========================================================================
   -- Indicates if the given point is contained by one of the layers
   --===========================================================================
   function Contains (Point : Position_Record) return Boolean;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The type of layer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Form_Types is (Form_Polygon, Form_Polyline);

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
   No_Part_Info : constant Part_Infos := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A part in a layer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Record is new Stacks.Linked.Linked_Record with record

      North_East : Position_Record := No_Position_Record;

      South_West : Position_Record := No_Position_Record;

      Points     : Math.Vector2_List.Stack;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Part_Access is access all Part_Record'Class;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Part_List is new Stacks.Generic_List (T_Record => Part_Record,
                                                 T_Access => Part_Access,
                                                 Maximum  => 1000);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A layer of map features
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Layer_Record is new Stacks.Linked.Linked_Record with record

      Name       : Part_Names := No_Part_Name;

      Info       : Part_Infos := No_Part_Info;

      Parts      : Part_List.Stack;

      Form       : Form_Types  := Form_Polyline;

      Kind       : Layer_Types := Layer_Unknown;

      North_East : Position_Record := No_Position_Record;

      South_West : Position_Record := No_Position_Record;

   end record;
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   function Contains (This : Layer_Record; Position : Position_Record) return Boolean;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Layer_Access is access all Layer_Record'Class;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Layer_List is new Stacks.Generic_List (T_Record => Layer_Record,
                                                  T_Access => Layer_Access,
                                                  Maximum  => 500);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The collection of layers
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Layers : Layer_List.Stack;

   --===========================================================================
   -- Scans all the data to compute the part bounds
   --===========================================================================
   procedure Compute_Bounds (List : Layer_List.Stack);

   --===========================================================================
   -- Scans all the data and returns the global bounds
   --===========================================================================
   procedure Compute_Global_Bounds (List : Layer_List.Stack; South_West, North_East : out Position_Record);

end Layers;
--------------------------------------------------------------------------------
