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
with Glex.Basic;
with Glex.Colors;
use  Glex.Colors;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Widgets.Panel is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Label_Sides is (Label_Left, Label_Right);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Panel_Record is new Widget_Record with private;

   --===========================================================================
   -- Initializes the object
   --===========================================================================
   overriding procedure Initialize (This : in out Panel_Record);

   --===========================================================================
   -- Finalizes the object
   --===========================================================================
   overriding procedure Finalize (This : in out Panel_Record);

   --===========================================================================
   -- Copies the panel from another instance
   --===========================================================================
   overriding procedure Copy (This : in out Panel_Record; Other : Panel_Record);

   --===========================================================================
   -- Represents the widget using the current open gl context
   --===========================================================================
   overriding procedure Draw (This : in out Panel_Record);

   --===========================================================================
   -- Sets the text to be represented
   --===========================================================================
   procedure Set_Label (This : in out Panel_Record; Text : String; Side : Label_Sides := Label_Left);

   --===========================================================================
   -- Sets the text color of the text
   --===========================================================================
   procedure Set_Label_Color (This : in out Panel_Record;
                              Fore : Color_Record;
                              Glow : Color_Record := Color_Black);

   --===========================================================================
   -- Sets the size of the label font (from 0 to 1, relative to the height)
   --===========================================================================
   procedure Set_Font_Size (This        : in out Panel_Record;
                            Value       : Dimension_Float;
                            Width_Ratio : Ratio_Float := 0.6;
                            Space_Ratio : Ratio_Float := 0.7);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A bounded string used to label buttons
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Panel_String is String (1..10);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The button content
   -- TODO: either use base class resource for the area, or use the base area as
   --       extra covering surface.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Panel_Record is new Widget_Record with record

      Area_Resource : Glex.Basic.Resource_Type;

      Reload_Base   : Boolean;

      Label         : Panel_String;

      Length        : Natural;

      Font_Color    : Line_Color_Record;

      Font_Size     : Dimension_Float;

      Width_Ratio   : Ratio_Float;

      Space_Ratio   : Ratio_Float;

      Show_Border   : Boolean;

      Label_Side    : Label_Sides;

   end record;

end Widgets.Panel;
--------------------------------------------------------------------------------
