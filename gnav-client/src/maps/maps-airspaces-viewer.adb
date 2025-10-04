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
with Glex.Symbols;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Airspaces.Viewer is

   --===========================================================================
   --
   --===========================================================================
   procedure Next_Page (This : in out List_Record) is
   begin

      if This.Focus = 0 and then This.Page < This.Count then
         This.Page := This.Page + 1;
      end if;

   end Next_Page;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Previous_Page (This : in out List_Record) is
   begin

      if This.Focus = 0 and then This.Page > This.Pages'First then
         This.Page := This.Page - 1;
      end if;

   end Previous_Page;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Toggle_Focused_Item_Status (This : in out List_Record; X, Y : Float) is
   begin

      if
        This.Page  in Page_Array'Range and then
        This.Focus in Item_Array'Range
      then

         declare
            I : Natural := This.Pages (This.Page) (This.Focus).Index;
         begin

            if I in 1..Number_Of_Airspaces then

               if This.Active_Button.Contains (X, Y) then

                  Airspaces (I).Active := not Airspaces (I).Active;

                  Status_Changed := True;

               elsif Allow_Notifications and then Airspaces (I).Active and then This.Notify_Button.Contains (X, Y) then

                  -- Circular toggling function
                  ---------------------------------------------

                  case Airspaces (I).Notify is

                     when Notify_In =>

                        Airspaces (I).Notify := Notify_Out;

                     when Notify_Out =>

                        Airspaces (I).Notify := Notify_In_Out;

                     when Notify_In_Out =>

                        Airspaces (I).Notify := Notify_None;

                     when Notify_None =>

                        Airspaces (I).Notify := Notify_In;

                  end case;

                  Status_Changed := True;

               end if;

            end if;

         end;

      end  if;

   end Toggle_Focused_Item_Status;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Focus_Item (This : in out List_Record; X, Y : Float) is

      Last_Focus : Natural := This.Focus;

   begin

      if not (This.Page in Page_Array'Range) then
         return;
      end if;

      if This.Panel.Contains (X, Y) then

         This.Focus := 0;

         for S in Item_Array'Range loop

            if S /= Last_Focus then

               declare
                  Item : Item_Record renames  This.Pages (This.Page) (S);
                  Area : Allocation_Record := This.Panel.Get_Allocation;
               begin

                  if This.Pages (This.Page) (S).Index in Airspaces'Range and then
                    X > Item.Area.X - 0.5 * Item.Area.W and then
                    X < Item.Area.X + 0.5 * Item.Area.W and then
                    Y > Item.Area.Y - 0.5 * Item.Area.H and then
                    Y < Item.Area.Y + 0.5 * Item.Area.H
                  then
                     This.Focus := S;

                     Area.X := Item.Area.X - 0.5 * Item.Area.W - 0.008;
                     Area.W := Item.Area.W + 0.016;
                     Area.Y := Area.Y + Area.H + 0.02;
                     Area.H := 0.140;

                     This.Frame.Set_Allocation (Area);

                     Area.X := Area.X + 0.003;
                     Area.Y := Area.Y + 0.05;
                     Area.W := 0.5 * (Area.W - 0.009);
                     Area.H := 0.082;

                     This.Active_Button.Set_Allocation (Area);
                     This.Active_Button.Set_Font_Size (0.4, 0.3);

                     Area.X := Area.X + Area.W + 0.003;

                     This.Notify_Button.Set_Allocation (Area);
                     This.Notify_Button.Set_Font_Size (0.4, 0.5);

                  end if;

               end;

            end if;

         end loop;

         if This.Focus /= 0 then
            Focused_Airspace := This.Pages (This.Page) (This.Focus).Index;
         else
            Focused_Airspace := 0;
         end if;

      elsif This.Focus in Item_Array'Range and then This.Frame.Contains (X, Y) then

         This.Toggle_Focused_Item_Status (X, Y);

      end if;

   end Focus_Item;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Focused (This : in out List_Record) return Boolean is
   begin

      return This.Focus /= 0;

   end Focused;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- NOTE: start with P = K = 0
   --===========================================================================
   procedure Add_Airspace (This   : in out List_Record;
                           P      : in out Natural;
                           K      : in out Natural;
                           I      : Natural;
                           Inside : Boolean) is
      H, W   : Float;
      S      : Float;
      Sx, Sy : Float;
   begin

      if K in 1 .. Number_Of_Items - 1 then
         K := K + 1;
      else
         K := 1;
         P := P + 1;
      end if;

      if P not in 1 .. Number_Of_Pages then
         return;
      end if;

      H := This.Pages (P) (K).Area.H;
      W := This.Pages (P) (K).Area.W;

      S  := 0.0;
      Sx := abs Float (Airspaces (I).North_East.Lon - Airspaces (I).South_West.Lon) / (0.8 * W) * Shrink;
      Sy := abs Float (Airspaces (I).North_East.Lat - Airspaces (I).South_West.Lat) / (0.8 * H) * Aspect;

      if Sx > 0.0 and then Sx > Sy then
         S := 1.0 / Sx;
      elsif Sy > 0.0 then
         S := 1.0 / Sy;
      end if;

      if S = 0.0 then
         return;
      end if;

      This.Pages (P) (K).Index  := I;
      This.Pages (P) (K).Scale  := S;
      This.Pages (P) (K).Inside := Inside;
      This.Count := P;

   end Add_Airspace;
   pragma Inline (Add_Airspace);
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Reset_Pages (This : in out List_Record) is
   begin

      This.Count := 0;
      This.Focus := 0;
      This.Page  := 1;

      for P in 1 .. Number_Of_Pages loop

         for K in 1 .. Number_Of_Items loop

            This.Pages (P) (K).Index  := 0;
            This.Pages (P) (K).Scale  := 0.0;
            This.Pages (P) (K).Inside := False;

         end loop;

      end loop;

      Focused_Airspace := 0;

   end Reset_Pages;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Build_List (This        : in out List_Record;
                         View        : Map_View_Record;
                         Position    : Position_Record;
                         Inside_Only : Boolean := True) is

      P, K : Natural := 0;

   begin

      This.Reset_Pages;

      -- Reset flag if necessary
      --------------------------------------------------------------------------
      if not Inside_Only then

         for I in 1 .. Number_Of_Airspaces loop

            Airspaces (I).Flagged := False;

         end loop;

      end if;

      -- Contained sectors go first
      --------------------------------------------------------------------------
      if Position /= No_Position_Record then --> chance with map rectangular bounds

         for I in 1 .. Number_Of_Airspaces loop

            if Airspaces (I).Contains (Position) then

               This.Add_Airspace (P, K, I, True);

               Airspaces (I).Flagged := True;

               exit when P > Number_Of_Pages;

            end if;

         end loop;

      end if;

      -- Contained in the viewing rectangle
      --------------------------------------------------------------------------
      if not Inside_Only then

         for I in 1 .. Number_Of_Airspaces loop

            if not Airspaces (I).Flagged
              and then View.On_Clip (Airspaces (I).North_East,
                                     Airspaces (I).South_West)
            then

               This.Add_Airspace (P, K, I, False);

               exit when P > Number_Of_Pages;

            end if;

         end loop;

      end if;

      This.Focus := 0;
      This.Page  := 1;

      Focused_Airspace := 0;

   end Build_List;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Allocation (This : in out List_Record; Allocation : Allocation_Record) is

      X, Y  : Float;
      H, W  : Float;
      K     : Natural := 1;
      Area  : Allocation_Record;

   begin

      H := Allocation.H / Float (Number_Of_Rows);
      W := Allocation.W / Float (Number_Of_Columns);
      X := Allocation.X + 0.5 * W;
      Y := Allocation.Y - 0.5 * H + Allocation.H;

      for P in 1 .. Number_Of_Pages loop

         K := 0;

         Area := (X, Y, W, H);

         for R in 1 .. Number_Of_Rows loop

            Area.X := X;

            for C in 1 .. Number_Of_Columns loop

               K := K + 1;

               This.Pages (P) (K).Area := Area;

               Area.X := Area.X + W;

            end loop;

            Area.Y := Area.Y - H;

         end loop;

      end loop;

      This.Panel.Set_Allocation (Allocation);
      This.Panel.Set_Background_Color (Color_Gray_7);
      This.Frame.Set_Background_Color (Color_Gray_6);

   end Set_Allocation;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Contains (This : List_Record; X, Y : Float) return Boolean is
   begin

      return
         This.Panel.Contains (X, Y) or else
        (This.Focus in Item_Array'Range and then This.Frame.Contains (X, Y));

   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Is_Empty (This : List_Record) return Boolean is
   begin

      return This.Count = 0;

   end Is_Empty;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Clear (This : in out List_Record) is
   begin

      This.Count := 0;

   end Clear;
   -----------------------------------------------------------------------------


   Font_1 : Font_Style_Record := (Width     => 0.008,
                                  Height    => 0.026,
                                  Space     => 0.006,
                                  Rendering => Glex.Fonts.Font_Extra_Glow,
                                  Thickness => Glex.Fonts.Font_Bold);

   Font_2 : Font_Style_Record := (Width     => 0.006,
                                  Height    => 0.022,
                                  Space     => 0.006,
                                  Rendering => Glex.Fonts.Font_Glow,
                                  Thickness => Glex.Fonts.Font_Bold);

   --===========================================================================
   -- Draws the list of visible sectors with their name and limits
   --===========================================================================
   procedure Draw_List (This     : in out List_Record;
                        Position : Position_Record;
                        Blink    : Boolean) is

      use Glex.Fonts;

      I    : Natural;
      X, Y : Float;

      Matrix : Glex.Transform_Record;
      Sector : Color_Record;

   begin

      if not (This.Page in Page_Array'Range) then
         return;
      end if;

      if This.Focus in Item_Array'Range then
         This.Panel.Set_Background_Color (Color_Gray_4);
      else
         This.Panel.Set_Background_Color (Color_Gray_7);
         --This.Draw_Focus (Position, Blink);
         --return;
      end if;
      --This.Panel.Set_Transparency (0.5);

      This.Panel.Draw;

      for S in Item_Array'Range loop

         declare
            Item : Item_Record renames This.Pages (This.Page) (S);
         begin

            I := This.Pages (This.Page) (S).Index;

            if I in Airspaces'Range then

               Matrix.Load_Unit;

               Matrix.Translate (Item.Area.X,
                                 Item.Area.Y);

               Matrix.Scale     (Item.Scale,
                                 Item.Scale * Glex.Aspect);

               Matrix.Translate (Float (Center.Lon - Airspaces (I).Center.Lon) * Shrink,
                                 Float (Center.Lat - Airspaces (I).Center.Lat));

               Glex.Get_Transform.Copy (Matrix);

               if Airspaces (I).Active then
                  Sector := Colors (Airspaces (I).Kind);
               else
                  Sector := Color_White;
               end if;

               if S = This.Focus then
                  for J in Airspaces (I).First .. Airspaces (I).Last loop
                     Parts (J).Resource.Draw (Color_Gray_8, 0.007 / Item.Scale);
                  end loop;
               end if;

               for J in Airspaces (I).First .. Airspaces (I).Last loop
                  Parts (J).Resource.Draw (Sector, 0.002 / Item.Scale);
               end loop;

               Glex.Get_Transform.Load_Unit;

               -- Vertical limits
               --------------------------------------------------------------------

               Glex.Fonts.Draw (Trim (Airspaces (I).Label.Upper),
                                X         => Item.Area.X,
                                Y         => Item.Area.Y + 0.01,
                                Style     => Font_1,
                                Color     => Line_Yellow,
                                Alignment => Alignment_LC);

               Glex.Fonts.Draw (Trim (Airspaces (I).Label.Lower),
                                X         => Item.Area.X,
                                Y         => Item.Area.Y - 0.01,
                                Style     => Font_1,
                                Color     => Line_Yellow,
                                Alignment => Alignment_TC);

               if S = This.Focus then

                  This.Frame.Draw;

                  X := This.Frame.Get_Allocation.X + 0.5 * This.Frame.Get_Allocation.W;
                  Y := This.Frame.Get_Allocation.Y;

                  Glex.Symbols.Draw (Glex.Symbols.Triangle_Down,
                                     X         => Item.Area.X,
                                     Y         => Y,
                                     Size      => 0.016,
                                     Color     => Color_Gray_6,
                                     Alignment => Alignment_TC);

                  Y := Y + 0.012;

                  Glex.Fonts.Draw (Trim (Airspaces (I).Name (1..13)),
                                   X         => X,
                                   Y         => Y,
                                   Style     => Font_2,
                                   Color     => Line_White,
                                   Alignment => Alignment_LC);

                  if Airspaces (I).Active then
                     This.Active_Button.Set_Style (Button_Ok);
                     This.Active_Button.Set_Label ("ON");
                  else
                     This.Active_Button.Set_Style (Button_Cancel);
                     This.Active_Button.Set_Label ("OFF");
                  end if;

                  if Airspaces (I).Active and Allow_Notifications then

                     This.Notify_Button.Set_Style (Button_Enabled);

                  else
                     This.Notify_Button.Set_Style (Button_Disabled);

                  end if;

                  case Airspaces (I).Notify is

                     when Notify_In =>

                        This.Notify_Button.Set_Label ("{");

                     when Notify_Out =>

                        This.Notify_Button.Set_Label ("}");

                     when Notify_In_Out =>

                        This.Notify_Button.Set_Label ("{}");

                     when Notify_None =>
                        This.Notify_Button.Set_Label ("X");
                        This.Notify_Button.Set_Style (Button_Alive);

                  end case;

                  This.Active_Button.Draw;
                  This.Notify_Button.Draw;

               end if;

               -- Airplane position
               --------------------------------------------------------------------

               -- if Blink then
               --
               --    Ax := Float (Position.Lon - Airspaces (I).Center.Lon) * Item.Scale * Shrink;
               --    Ay := Float (Position.Lat - Airspaces (I).Center.Lat) * Item.Scale * Aspect;
               --
               --    if abs Ax < 0.48 * Item.Area.W and then abs Ay < 0.48 * Item.Area.H then
               --
               --       Glex.Symbols.Draw (Glex.Symbols.Triangle_Down,
               --                          X         => Item.Area.X + Ax,
               --                          Y         => Item.Area.Y + Ay,
               --                          Size      => 0.008,
               --                          Color     => Color_Orange,
               --                          Alignment => Alignment_LC);
               --
               --    end if;
               --
               -- end if;

            end if;

         end;

      end loop;

      if This.Count > 1 and Blink then

         -- Right arrow
         --------------------------------------------------------------------
         if This.Page < This.Count then

            X := This.Panel.Get_Allocation.X + This.Panel.Get_Allocation.W;
            Y := This.Panel.Get_Allocation.Y + This.Panel.Get_Allocation.H * 0.5;

            Glex.Symbols.Draw (Glex.Symbols.Triangle_Right,
                               X         => X,
                               Y         => Y,
                               Size      => 0.018,
                               Color     => Color_Gray_3,
                               Alignment => Alignment_CC);

         end if;

         -- Left arrow
         --------------------------------------------------------------------
         if This.Page > 1 then

            X := This.Panel.Get_Allocation.X;
            Y := This.Panel.Get_Allocation.Y + This.Panel.Get_Allocation.H * 0.5;

            Glex.Symbols.Draw (Glex.Symbols.Triangle_Left,
                               X         => X,
                               Y         => Y,
                               Size      => 0.018,
                               Color     => Color_Gray_3,
                               Alignment => Alignment_CC);

         end if;

      end if;

      Glex.Get_Transform.Load_Unit;

   end Draw_List;
   -----------------------------------------------------------------------------

end Maps.Airspaces.Viewer;
--------------------------------------------------------------------------------
