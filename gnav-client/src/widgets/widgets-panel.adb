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
with Glex.Fonts;
use  Glex.Fonts;
with Utility.Strings;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Widgets.Panel is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Draw (This : in out Panel_Record) is

      X, Y, D : Float;

      S : Font_Style_Record;
      L : constant Float    := Float (This.Length);
      A : Allocation_Record := This.Get_Allocation;

      Alignment : Font_Alignment_Types := Alignment_LL;

   begin

      if This.Get_Visible then

         -- Compute font metrics
         -----------------------------------------------------------------------

         S.Height := This.Font_Size;

         S.Width  := This.Width_Ratio * S.Height;

         S.Space  := This.Space_Ratio * S.Width;

         if This.Length > 0 then
            D := (S.Width + S.Space) * L - S.Space;
         else
            D := 0.0;
         end if;

         -- Draw the area and border
         -----------------------------------------------------------------------

         if This.Reload_Base then

            declare

               Area_Buffer : Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (6);

               XM : Float := A.X + D + 0.5 * S.Width + S.Space;
               YM : Float := A.Y + A.H - 1.0 * S.Height;
               YT : Float := A.Y + A.H;
               YB : Float := A.Y;
               XR : Float := A.X + A.W;
               XL : Float := A.X;

            begin

               if This.Label_Side = Label_Right then

                  XM := 2.0 * A.X - XM + A.W;
                  XL := 2.0 * A.X - XL + A.W;
                  XR := 2.0 * A.X - XR + A.W;

               end if;

               Area_Buffer.Load_Node (XM, YM);

               Area_Buffer.Load_Node (XM, YT);

               Area_Buffer.Load_Node (XR, YT);

               Area_Buffer.Load_Node (XR, YB);

               Area_Buffer.Load_Node (XL, YB);

               Area_Buffer.Load_Node (XL, YM);

               This.Area_Resource.Load (Area_Buffer);

            end;

         end if;

         This.Area_Resource.Draw (This.Get_Background_Color, Glex.Basic.Triangle_Fan);

         if This.Show_Border then

            This.Area_Resource.Draw (This.Get_Border_Color, Glex.Basic.Line_Loop);

         end if;

         if This.Length in This.Label'Range then

            -- Draw the text
            --------------------------------------------------------------------

            case This.Label_Side is

            when Label_Left =>

               X := A.X;

               Alignment := Alignment_LL;

            when Label_Right =>

               X := A.X + A.W;

               Alignment := Alignment_LR;

            end case;

            Y := A.Y + A.H - 0.5 * S.Height;

            Glex.Fonts.Draw (This.Label (1..This.Length), X, Y, S, This.Font_Color, Alignment);

         end if;

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Initialize (This : in out Panel_Record) is
   begin

      Widget_Record (This).Initialize;

      This.Reload_Base := True;
      This.Label       := (others => ' ');
      This.Length      := 0;

      This.Font_Color.Fore := Color_White;
      This.Font_Color.Glow := Color_Black;

      This.Font_Size   := 0.05;
      This.Show_Border := True;
      This.Width_Ratio := 0.6;
      This.Space_Ratio := 0.7;
      This.Label_Side  := Label_Left;
      This.Set_Background_Color (Color_Gray_4);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Finalize (This : in out Panel_Record) is
   begin

      Widget_Record (This).Finalize;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Copy (This : in out Panel_Record; Other : Panel_Record) is
   begin

      -- Force the creation of own resources

      Widget_Record (This).Copy (Widget_Record (Other));

      This.Reload_Base := True;
      This.Label       := Other.Label;
      This.Length      := Other.Length;
      This.Font_Color  := Other.Font_Color;
      This.Font_Size   := Other.Font_Size;
      This.Width_Ratio := Other.Width_Ratio;
      This.Space_Ratio := Other.Space_Ratio;
      This.Show_Border := Other.Show_Border;
      This.Label_Side  := Other.Label_Side;

   end Copy;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label (This : in out Panel_Record; Text : String; Side : Label_Sides := Label_Left) is

      New_Label  : Panel_String;

      Old_Length : Natural := This.Length;

   begin

      Utility.Strings.Override (New_Label, Text);

      if This.Label /= New_Label or This.Label_Side /= Side then

         This.Label       := New_Label;

         This.Length      := Natural'Min (Text'Length, New_Label'Length);

         This.Label_Side  := Side;

         This.Reload_Base := True;

      end if;

   end Set_Label;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Label_Color (This : in out Panel_Record;
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
   procedure Set_Font_Size (This        : in out Panel_Record;
                            Value       : Dimension_Float;
                            Width_Ratio : Ratio_Float := 0.6;
                            Space_Ratio : Ratio_Float := 0.7) is
   begin

      This.Font_Size   := Value;

      This.Width_Ratio := Width_Ratio;

      This.Space_Ratio := Space_Ratio;

      This.Reload_Base := True;

   end Set_Font_Size;
   -----------------------------------------------------------------------------

end Widgets.Panel;
--------------------------------------------------------------------------------
