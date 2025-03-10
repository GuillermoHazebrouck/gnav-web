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
-- Gnav

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Widgets.Widget is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (This : in out Widget_Record) is
   begin

      if This.Visible then

         if This.Reload_Base then

            declare
               Area_Buffer : aliased Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (4);
            begin

               Area_Buffer.Load_Node (This.Allocation.X,
                                      This.Allocation.Y);
               Area_Buffer.Load_Node (This.Allocation.X,
                                      This.Allocation.Y + This.Allocation.H);

               Area_Buffer.Load_Node (This.Allocation.X + This.Allocation.W,
                                      This.Allocation.Y + This.Allocation.H);

               Area_Buffer.Load_Node (This.Allocation.X + This.Allocation.W,
                                      This.Allocation.Y);

               This.Area_Resource.Load (Area_Buffer);

            end;

            This.Reload_Base := False;

         end if;

         This.Area_Resource.Draw (This.Background_Color, Glex.Basic.Triangle_Fan);

         if This.Show_Border then

            This.Area_Resource.Draw (This.Border_Color, Glex.Basic.Line_Loop);

         end if;

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Reload (This : in out Widget_Record) is
   begin

      This.Reload_Base := True;

   end Reload;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Allocation (This : in out Widget_Record; Value : Allocation_Record) is
   begin

      This.Reload_Base := This.Reload_Base or else This.Allocation /= Value;

      This.Allocation := Value;

   end Set_Allocation;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Gets the area covered by the widget
   --===========================================================================
   function Get_Allocation (This : in Widget_Record'Class) return Allocation_Record is
   begin

      return This.Allocation;

   end Get_Allocation;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Visible (This : in out Widget_Record'Class; Value : Boolean) is
   begin

      This.Visible := Value;

   end Set_Visible;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Visible (This : in out Widget_Record; Value : Boolean) return Boolean is
   begin

      return This.Visible;

   end Get_Visible;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Visible (This : Widget_Record'Class) return Boolean is
   begin

      return This.Visible;

   end Get_Visible;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Background_Color (This : in out Widget_Record'Class; Value : Color_Record) is
   begin

      This.Background_Color   := Value;
      This.Background_Color.A := 1.0;

   end Set_Background_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Transparency (This : in out Widget_Record'Class; Value : Float) is
   begin

      This.Background_Color.A := Value;

   end Set_Transparency;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Show_Border (This : in out Widget_Record'Class; Value : Boolean) is
   begin

      This.Show_Border := Value;

   end Set_Show_Border;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Border_Color (This : in out Widget_Record'Class; Value : Color_Record) is
   begin

      This.Border_Color := Value;

   end Set_Border_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Background_Color (This : Widget_Record'Class) return Color_Record is
   begin

      return This.Background_Color;

   end Get_Background_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Border_Color (This : Widget_Record'Class) return Color_Record is
   begin

      return This.Border_Color;

   end Get_Border_Color;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Contains (This : Widget_Record'Class; X, Y : Float) return Boolean is
   begin

      return
        This.Visible                              and then
        X > This.Allocation.X                     and then
        Y > This.Allocation.Y                     and then
        X < This.Allocation.X + This.Allocation.W and then
        Y < This.Allocation.Y + This.Allocation.H;

   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Initialize (This : in out Widget_Record) is
   begin

      This.Reload_Base        := True;
      This.Visible            := True;
      This.Show_Background    := True;
      This.Background_Color   := Color_Gray_5;
      This.Show_Border        := False;
      This.Border_Color       := Color_Black;
      This.Border_Thickness   := 1.0;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   overriding procedure Finalize (This : in out Widget_Record) is
   begin

      -- TODO:
      -- If widgets are created in a procedure, we must give back resource to GPU
      -- In the current implementation, all widgets must be statically declared in a package!

      null;

   end Finalize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Copies the widget
   --===========================================================================
   procedure Copy (This : in out Widget_Record; Other : Widget_Record) is
   begin

      This.Allocation       := Other.Allocation;
      This.Background_Color := Other.Background_Color;
      This.Border_Color     := Other.Border_Color;
      This.Border_Thickness := Other.Border_Thickness;
      This.Reload_Base      := True;
      This.Show_Background  := Other.Show_Background;
      This.Show_Border      := Other.Show_Background;
      This.Visible          := Other.Visible;

   end Copy;
   -----------------------------------------------------------------------------

end Widgets.Widget;
--------------------------------------------------------------------------------
