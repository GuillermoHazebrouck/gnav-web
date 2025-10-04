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
with Glex.Fonts;
with Utility.Strings;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Widgets.Button is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Draw (This : in out Button_Record) is

      S : Glex.Fonts.Font_Style_Record;
      X,
      Y : Float;
      L : constant Float := Float (This.Length);
      A : Allocation_Record := This.Get_Allocation;

   begin

      if This.Get_Visible then

         -- Draw the area and border from the widget class
         --------------------------------------------------------------------------

         Widget_Record (This).Draw;

         if This.Length in This.Label'Range or else This.Symbol /= None then

            -- Draw the text
            --------------------------------------------------------------------------
            S.Height := This.Font_Size * A.H;

            X := A.X + 0.5 * A.W;

            Y := A.Y + 0.5 * A.H;

            if This.Symbol /= None then

               Glex.Symbols.Draw (This.Symbol, X, Y, S.Height, This.Font_Color.Fore, Glex.Fonts.Alignment_CC, Glex.Symbols.Size_Height);

            else

               S.Width  := This.Width_Ratio * S.Height;

               S.Space  := This.Space_Ratio * S.Width;

               Glex.Fonts.Draw (This.Label (1..This.Length), X, Y, S, This.Font_Color, Glex.Fonts.Alignment_CC);

            end if;

         end if;

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Initialize (This : in out Button_Record) is
   begin

      Widget_Record (This).Initialize;

      This.Label           := (others => ' ');
      This.Length          := 0;

      This.Font_Color.Fore := Color_White;
      This.Font_Color.Glow := Color_Black;
      This.Font_Size       := 0.6;

      This.Width_Ratio     := 0.5;
      This.Space_Ratio     := 0.7;

      This.Symbol          := None;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Finalize (This : in out Button_Record) is
   begin

      Widget_Record (This).Finalize;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Copy (This : in out Button_Record; Other : Button_Record) is
   begin

      Widget_Record (This).Copy (Widget_Record (Other));

      This.Font_Color  := Other.Font_Color;
      This.Font_Size   := Other.Font_Size;
      This.Space_Ratio := Other.Space_Ratio;
      This.Width_Ratio := Other.Width_Ratio;
      This.Label       := Other.Label;
      This.Length      := Other.Length;

   end Copy;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label (This : in out Button_Record; Text : String) is

      New_Label : Button_String;

   begin

      Utility.Strings.Override (New_Label, Text);

      if This.Label /= New_Label then

         This.Label  := New_Label;

         This.Length := Text'Length;

      end if;

      This.Symbol := None;

   end Set_Label;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Symbol (This : in out Button_Record; Symbol : Symbol_Kinds) is
   begin

      This.Symbol := Symbol;

   end Set_Symbol;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label_Color (This : in out Button_Record;
                              Fore : Color_Record;
                              Glow : Color_Record := Color_Black) is
   begin

      This.Font_Color.Fore := Fore;

      This.Font_Color.Glow := Glow;

   end Set_Label_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label_Color (This  : in out Button_Record;
                              Color : Line_Color_Record) is
   begin

      This.Font_Color := Color;

   end Set_Label_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Font_Size (This        : in out Button_Record;
                            Value       : Ratio_Float;
                            Width_Ratio : Ratio_Float := 0.6;
                            Space_Ratio : Ratio_Float := 0.7) is
   begin

      This.Font_Size   := Value;

      This.Width_Ratio := Width_Ratio;

      This.Space_Ratio := Space_Ratio;

   end Set_Font_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Style (This  : in out Button_Record;
                        Value : Button_Style_Kinds) is
   begin

      case Value is

         when Button_Normal =>

            This.Set_Background_Color  (Color_Back_Button_Normal);
            This.Set_Border_Color      (Color_Line_Button_Normal);
            This.Set_Label_Color       (Color_Font_Button_Normal);

         when Button_Enabled =>

            This.Set_Background_Color  (Color_Back_Button_Enabled);
            This.Set_Border_Color      (Color_Line_Button_Enabled);
            This.Set_Label_Color       (Color_Font_Button_Enabled);

         when Button_Disabled =>

            This.Set_Background_Color (Color_Back_Button_Disabled);
            This.Set_Border_Color     (Color_Line_Button_Disabled);
            This.Set_Label_Color      (Color_Font_Button_Disabled);

         when Button_Action =>

            This.Set_Background_Color (Color_Back_Button_Action);
            This.Set_Border_Color     (Color_Line_Button_Action);
            This.Set_Label_Color      (Color_Font_Button_Action);

         when Button_Alive =>

            This.Set_Background_Color  (Color_Back_Button_Alive);
            This.Set_Border_Color      (Color_Line_Button_Alive);
            This.Set_Label_Color       (Color_Font_Button_Alive);

         when Button_Focus =>

            This.Set_Background_Color (Color_Back_Button_Focus);
            This.Set_Border_Color     (Color_Line_Button_Focus);
            This.Set_Label_Color      (Color_Font_Button_Focus);

         when Button_Ok =>

            This.Set_Background_Color (Color_Back_Button_Ok);
            This.Set_Border_Color     (Color_Line_Button_Ok);
            This.Set_Label_Color      (Color_Font_Button_Ok);

         when Button_Cancel =>

            This.Set_Background_Color (Color_Back_Button_Cancel);
            This.Set_Border_Color     (Color_Line_Button_Cancel);
            This.Set_Label_Color      (Color_Font_Button_Cancel);

      end case;

   end Set_Style;


end Widgets.Button;
--------------------------------------------------------------------------------
