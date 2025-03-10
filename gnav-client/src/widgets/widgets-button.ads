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
with Glex.Colors;
use  Glex.Colors;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Widgets.Button is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Button_Style_Kinds is (Button_Normal,
                               Button_Action,
                               Button_Disabled,
                               Button_Enabled,
                               Button_Ok,
                               Button_Cancel,
                               Button_Alive,
                               Button_Focus);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Button_Record is new Widget_Record with private;

   --===========================================================================
   -- Initializes the object
   --===========================================================================
   overriding procedure Initialize (This : in out Button_Record);

   --===========================================================================
   -- Finalizes the object
   --===========================================================================
   overriding procedure Finalize (This : in out Button_Record);

   --===========================================================================
   -- Copies the button using another instance
   --===========================================================================
   overriding procedure Copy (This : in out Button_Record; Other : Button_Record);

   --===========================================================================
   -- Represents the widget using the current open gl context
   --===========================================================================
   overriding procedure Draw (This : in out Button_Record);

   --===========================================================================
   -- Sets the text to be represented
   --===========================================================================
   procedure Set_Label (This : in out Button_Record; Text : String);

   --===========================================================================
   -- Sets the text color of the text
   --===========================================================================
   procedure Set_Label_Color (This : in out Button_Record;
                              Fore : Color_Record;
                              Glow : Color_Record := Color_Black);

   --===========================================================================
   -- Sets the text color of the text
   --===========================================================================
   procedure Set_Label_Color (This  : in out Button_Record;
                              Color : Line_Color_Record);

   --===========================================================================
   -- Sets the size of the label font (from 0 to 1, relative to the height)
   --===========================================================================
   procedure Set_Font_Size (This        : in out Button_Record;
                            Value       : Ratio_Float;
                            Width_Ratio : Ratio_Float := 0.6;
                            Space_Ratio : Ratio_Float := 0.7);

   --===========================================================================
   --
   --===========================================================================
   procedure Set_Style (This  : in out Button_Record;
                        Value : Button_Style_Kinds);

private

   -- Button styles

   Color_Line_Button_Normal   : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Normal   : constant Color_Record      := Color_Gray_6;
   Color_Font_Button_Normal   : constant Line_Color_Record := Line_White;

   Color_Line_Button_Enabled  : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Enabled  : constant Color_Record      := Color_Magenta;
   Color_Font_Button_Enabled  : constant Line_Color_Record := Line_White;

   Color_Line_Button_Disabled : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Disabled : constant Color_Record      := Color_Gray_2;
   Color_Font_Button_Disabled : constant Line_Color_Record := Line_Gray;

   Color_Line_Button_Alive    : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Alive    : constant Color_Record      := Color_Gray_3;
   Color_Font_Button_Alive    : constant Line_Color_Record := Line_Cyan;

   Color_Line_Button_Focus    : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Focus    : constant Color_Record      := Color_Pink;
   Color_Font_Button_Focus    : constant Line_Color_Record := Line_Cyan;

   Color_Line_Button_Action   : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Action   : constant Color_Record      := Color_Ocean;
   Color_Font_Button_Action   : constant Line_Color_Record := Line_White;

   Color_Line_Button_Ok       : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Ok       : constant Color_Record      := Color_Green;
   Color_Font_Button_Ok       : constant Line_Color_Record := Line_White;

   Color_Line_Button_Cancel   : constant Color_Record      := Color_Gray_4;
   Color_Back_Button_Cancel   : constant Color_Record      := Color_Red;
   Color_Font_Button_Cancel   : constant Line_Color_Record := Line_White;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A bounded string used to label buttons
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Button_String is String (1..25);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The button content
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Button_Record is new Widget_Record with record

      Label       : Button_String;

      Length      : Natural;

      Font_Color  : Line_Color_Record;

      Font_Size   : Dimension_Float;

      Width_Ratio : Ratio_Float;

      Space_Ratio : Ratio_Float;

   end record;

end Widgets.Button;
--------------------------------------------------------------------------------
