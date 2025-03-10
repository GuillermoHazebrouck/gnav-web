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
with Widgets.Widget;
use  Widgets.Widget;
with Widgets.Button;
use  Widgets.Button;
with Glex.Lines;
with Glex.Colors;
use  Glex.Colors;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
generic

   Number_Of_Rows    : Natural;

   Number_Of_Columns : Natural;

   Number_Of_Pages   : Natural;

package Maps.Airspaces.Viewer is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type List_Record is tagged limited private;

   --===========================================================================
   --
   --===========================================================================
   procedure Next_Page (This : in out List_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Previous_Page (This : in out List_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Focus_Item (This : in out List_Record; X, Y : Float);

   --===========================================================================
   --
   --===========================================================================
   function Focused (This : in out List_Record) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Allocation (This : in out List_Record; Allocation : Allocation_Record);

   --===========================================================================
   --
   --===========================================================================
   function Contains (This : List_Record; X, Y : Float) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   function Is_Empty (This : List_Record) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   procedure Clear (This : in out List_Record);

   --===========================================================================
   --
   --===========================================================================
   procedure Build_List (This        : in out List_Record;
                         View        : Map_View_Record;
                         Position    : Position_Record;
                         Inside_Only : Boolean := True);

   --===========================================================================
   -- Draws the list of visible sectors with their name and limits
   --===========================================================================
   procedure Draw_List (This     : in out List_Record;
                        Position : Position_Record;
                        Blink    : Boolean);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Item_Record is record

      Index  : Natural := 0;

      Scale  : Float   := 0.0;

      Area   : Allocation_Record;

      Inside : Boolean := False;

   end record;

   Number_Of_Items : constant Natural := Number_Of_Rows * Number_Of_Columns;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Item_Array is array (1..Number_Of_Items) of Item_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Page_Array is array (1..Number_Of_Pages) of Item_Array;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type List_Record is tagged limited record

      Page   : Natural := 1;

      Focus  : Natural := 0;

      Pages  : Page_Array := (others => (others => (Index  => 0,
                                                   Scale  => 0.0,
                                                   Area   => (0.0, 0.0, 0.0, 0.0),
                                                   Inside => False)));
      Count  : Natural := 0;

      Panel  : Widget_Record;

      Frame  : Widget_Record;

      Active_Button : Button_Record;

      Notify_Button : Button_Record;

   end record;

end Maps.Airspaces.Viewer;
--------------------------------------------------------------------------------
