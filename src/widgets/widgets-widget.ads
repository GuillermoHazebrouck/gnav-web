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
use  Ada.Finalization;
-- Gnav
with Glex;
with Glex.Colors;
use  Glex.Colors;
with Glex.Basic;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Widgets.Widget is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Ratio_Float is Float range 0.0..1.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Dimension_Float is Float range 0.0..Float'Last;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Allocation_Record  is record

      X : Float := 0.0;

      Y : Float := 0.0;

      W : Dimension_Float := 0.0;

      H : Dimension_Float := 0.0;

   end record;
   -----------------------------------------------------------------------------

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Widget_Record is new Limited_Controlled with private;

   --===========================================================================
   -- Initializes the object
   --===========================================================================
   overriding procedure Initialize (This : in out Widget_Record);

   --===========================================================================
   -- Finalizes the object
   --===========================================================================
   overriding procedure Finalize (This : in out Widget_Record);

   --===========================================================================
   -- Copies the widget using another instance
   --===========================================================================
   procedure Copy (This : in out Widget_Record; Other : Widget_Record);

   --===========================================================================
   -- Represents the widget
   --===========================================================================
   procedure Draw (This : in out Widget_Record);

   --===========================================================================
   -- Sets the area covered by the widget
   --===========================================================================
   procedure Set_Allocation (This : in out Widget_Record; Value : Allocation_Record);

   --===========================================================================
   -- Gets the area covered by the widget
   --===========================================================================
   function Get_Allocation (This : in Widget_Record'Class) return Allocation_Record;

   --===========================================================================
   -- Indicates if the widget should be drawn
   --===========================================================================
   procedure Set_Visible (This : in out Widget_Record'Class; Value : Boolean);

   --===========================================================================
   -- Indicates if the widget should is drawn
   --===========================================================================
   function Get_Visible (This : in out Widget_Record'Class) return Boolean;

   --===========================================================================
   -- Sets the background color of the widget
   --===========================================================================
   procedure Set_Background_Color (This : in out Widget_Record'Class; Value : Color_Record);

   --===========================================================================
   -- Sets the border color of the widget
   --===========================================================================
   procedure Set_Border_Color (This : in out Widget_Record'Class; Value : Color_Record);

   --===========================================================================
   -- Gets the background color of the widget
   --===========================================================================
   function Get_Background_Color (This : in out Widget_Record'Class) return Color_Record;

   --===========================================================================
   -- Gets the border color of the widget
   --===========================================================================
   function Get_Border_Color (This : in out Widget_Record'Class) return Color_Record;

   --===========================================================================
   -- Sets the background transparency
   --===========================================================================
   procedure Set_Transparency (This : in out Widget_Record'Class; Value : Float);

   --===========================================================================
   -- Sets the the border on or off
   --===========================================================================
   procedure Set_Show_Border (This : in out Widget_Record'Class; Value : Boolean);

   --===========================================================================
   -- Indicates if the given point is inside the widget.
   -- NOTE: the widget must be visible.
   --===========================================================================
   function Contains (This : in out Widget_Record'Class; X, Y : Float) return Boolean;

private

   type Widget_Record is new Limited_Controlled with record

      -- Base graphical properties
      ------------------------------------------------

      Area_Resource    : Glex.Basic.Resource_Type;

      Allocation       : Allocation_Record;

      Reload_Base      : Boolean;

      Visible          : Boolean;

      Show_Background  : Boolean;

      Background_Color : Color_Record;

      Show_Border      : Boolean;

      Border_Color     : Color_Record;

      Border_Thickness : Float;

   end record;
   -----------------------------------------------------------------------------

end Widgets.Widget;
--------------------------------------------------------------------------------
