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
with Interfaces;
-- Gnav
with Glex;
with Glex.Colors;
with Glex.Colormap;
with Utility.Log;
with Utility.Storage;
with Utility.Strings;
with Utility.Streams;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Terrain is

   pragma Warnings (Off);

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_High_Resolution return Boolean is
   begin

      return High_Resolution;

   end Get_High_Resolution;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_High_Resolution (Value : Boolean) is
   begin

      if High_Resolution /= Value then

         High_Resolution := Value;

         Reload := True;

         if High_Resolution then
            Utility.Storage.Set_Item ("RESOLUTION", "HIGH");
         else
            Utility.Storage.Set_Item ("RESOLUTION", "LOW");
         end if;

      end if;

   end Set_High_Resolution;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Clears the grid
   --===========================================================================
   procedure Clear_Grid;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Previous view
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Old_View : Map_View_Record;


   --===========================================================================
   -- Notifies that a reload is required
   --===========================================================================
   procedure Notify_Range_Changed is
   begin

      if Old_View.Cone_Active then

         Reload := Loaded;

      end if;

   end Notify_Range_Changed;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Clears the grid
   --===========================================================================
   procedure Clear_Grid is
   begin

      --Utility.Log.Put_Message ("clearing the terrain grid");

      N_Lon         := 0;

      N_Lat         := 0;

      Cell_Size_Lat := 0.0;

      Cell_Size_Lon := 0.0;

      Altitude      := (others => 0);

      Reload        := True;

   end Clear_Grid;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buffer_Limit : constant Natural := 2000;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Static buffer used to build the colormap
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buffer : Glex.Colormap.Buffer_Type := Glex.Colormap.New_Buffer (Nodes    =>     Buffer_Limit,
                                                                   Elements => 2 * Buffer_Limit);

   --===========================================================================
   -- Draws an specific region of terrain when it fits the view
   --===========================================================================
   procedure Draw (View : Map_View_Record) is
   begin

      -- Check if the clip is valid
      --------------------------------------------------------------------------
      if not Loaded or Z_Max_Global < View.Zero then

         return;

      end if;

      -- Reload graphics if necessary
      --------------------------------------------------------------------------
      if View /= Old_View or Reload then

         Reload   := False;

         Old_View := View;

         Buffer.Reset;

         if not View.Show_Terrain then

            for Colormap of Colormaps loop
               Colormap.Load (Buffer);
            end loop;

            return;

         end if;

         declare

            -- The intended resolution (cells/side)
            -----------------------------------------------
            View_Aspect  : constant Float := View.W / View.H * Glex.Aspect;
            Resolution   : Float; --> maximum number of tiles on the sides
            Resolution_X : Float; --> intended size of the tiles on X
            Resolution_Y : Float; --> intended size of the tiles on Y

            -- The required number of cells to cover a tile
            -----------------------------------------------
            Tile_Step_X : Positive := 1;
            Tile_Step_Y : Positive := 1;

            -- Number of vertical and horizontal tiles
            -----------------------------------------------
            Tile_Count_X : Natural := 0;
            Tile_Count_Y : Natural := 0;

            -- The size of a tile in isometric coordinates
            -----------------------------------------------
            Tile_Size_X : Float;
            Tile_Size_Y : Float;

            -- Screen limits
            -----------------------------------------------
            Screen_NE : Position_Record;
            Screen_SW : Position_Record;

            -- The offset between the screen and the grid
            -----------------------------------------------
            Offset_Lat,
            Offset_Lon : Float;

            -- Grid boundary indices
            -----------------------------------------------
            Grid_L  : Integer;
            Grid_B  : Integer;
            Grid_T  : Integer;
            Grid_R  : Integer;

            N : Natural := Colormaps'Length;
            T : Natural := 0;

         begin

            On_Clip := False;

            View.Get_Limits (Screen_NE, Screen_SW);

            -- Check if the chart is outside the clip boundary (nothing to draw)
            --------------------------------------------------------------------
            if
              South_West.Lon >= Screen_NE.Lon or
              North_East.Lon <= Screen_SW.Lon or
              North_East.Lat <= Screen_SW.Lat or
              South_West.Lat >= Screen_NE.Lat
            then

               for Colormap of Colormaps loop
                  Colormap.Load (Buffer);
               end loop;

               return;

            end if;

            On_Clip := True;

            -- Compute the size of a tile in isometric map coordinates
            --------------------------------------------------------------------
            if High_Resolution then
               Resolution := 60.0;
            else
               Resolution := 42.0;
            end if;

            Resolution_X := View.Zoom / Resolution; --latdeg/width
            Resolution_Y := Resolution_X;

            if View_Aspect > 1.0 then
               Resolution_Y := Resolution_Y * View_Aspect;
            else
               Resolution_X := Resolution_X / View_Aspect;
            end if;

            Tile_Step_X := Positive (Float'Ceiling (View.W * Resolution_X / Cell_Size_Lon / Shrink));
            Tile_Step_Y := Positive (Float'Ceiling (View.H * Resolution_Y / Cell_Size_Lat / Glex.Aspect));

            Tile_Size_X := Cell_Size_Lon * Float (Tile_Step_X) * Shrink;
            Tile_Size_Y := Cell_Size_Lat * Float (Tile_Step_Y);

            -- Compute grid corner
            --------------------------------------------------------------------
            Offset_Lat  := Float (Screen_SW.Lat - South_West.Lat);
            Offset_Lon  := Float (Screen_SW.Lon - South_West.Lon);

            Grid_B      := Integer'Max (1, Integer'Min (N_Lat, Integer (Float'Floor (Offset_Lat / Cell_Size_Lat))));
            Grid_L      := Integer'Max (1, Integer'Min (N_Lon, Integer (Float'Floor (Offset_Lon / Cell_Size_Lon))));

            Offset_Lat  := Float (Screen_NE.Lat - South_West.Lat);
            Offset_Lon  := Float (Screen_NE.Lon - South_West.Lon);

            Grid_T      := Integer'Min (N_Lat, Integer'Max (1, Integer (Float'Ceiling (Offset_Lat / Cell_Size_Lat)) + Tile_Step_X));
            Grid_R      := Integer'Min (N_Lon, Integer'Max (1, Integer (Float'Ceiling (Offset_Lon / Cell_Size_Lon)) + Tile_Step_Y));

            -- Calculate number of tiles
            --------------------------------------------------------------------
            Tile_Count_X := (Grid_R - Grid_L) / Tile_Step_X;
            Tile_Count_Y := (Grid_T - Grid_B) / Tile_Step_Y;

            T := (Tile_Count_X + 1) * (Tile_Count_Y + 1);

            -- Check that there is something to draw
            -----------------------------------------------------------------------
            if Tile_Count_X = 0 or Tile_Count_Y = 0 then

               return;

            -- Check if the map can be hold in one colormap
            -----------------------------------------------------------------------
            elsif T < Buffer_Limit then

               N := 1;

            -- Check that the tiles can be fully loaded
            -----------------------------------------------------------------------
            elsif T / N > Buffer_Limit - Tile_Count_X - 1 then

               -- (normally this should not occur due to resolution contrains)

               return;

            end if;

            declare

               X, Y, Z : Float;
               I, J    : Natural;
               I1, J1  : Natural;
               I2, J2  : Natural;
               R, G, B : Float;

               P  : Position_Record;
               P0 : Position_Record;
               PD : Position_Record;

               Tile_Init_Y : Natural;
               Tile_Last_Y : Natural;
               Tile_Span_Y : Natural;

            begin

               -- Position reference and increment
               --------------------------------------------------------------------

               P0.Lat := South_West.Lat + Long_Float (Grid_B - 1) * Long_Float (Cell_Size_Lat);
               P0.Lon := South_West.Lon + Long_Float (Grid_L - 1) * Long_Float (Cell_Size_Lon);

               PD.Lat := Long_Float (Tile_Step_Y) * Long_Float (Cell_Size_Lat);
               PD.Lon := Long_Float (Tile_Step_X) * Long_Float (Cell_Size_Lon);

               -- Vertices
               --------------------

               P.Lat := P0.Lat;

               Y := Float (P.Lat - Center.Lat);

               I := (Grid_B - 1) * N_Lon;

               for C in 1..N loop

                  Buffer.Reset;

                  Tile_Init_Y := (C-1) * Tile_Count_Y / N;
                  Tile_Last_Y :=  C    * Tile_Count_Y / N;
                  Tile_Span_Y := Tile_Last_Y - Tile_Init_Y;

                  for K in Tile_Init_Y .. Tile_Last_Y loop

                     P.Lon := P0.Lon;

                     X := Float (P.Lon - Center.Lon) * Shrink;

                     J := Grid_L;

                     for S in 0..Tile_Count_X loop

                        Z := Float (Altitude (I + J));

                        View.Find_Color (P, Z, 1.0, Z_Min_Global, Z_Max_Global, R, G ,B);

                        Buffer.Load_Node (X, Y, R, G, B);

                        P.Lon := P.Lon + PD.Lon;

                        X := X + Tile_Size_X;

                        J := J + Tile_Step_X;

                     end loop;

                     P.Lat := P.Lat + PD.Lat;

                     if K < Tile_Last_Y then

                        Y := Y + Tile_Size_Y;

                        I := I + N_Lon * Tile_Step_Y;

                     end if;

                  end loop;

                  -- Elements
                  --------------------

                  I1 := 0;
                  I2 := 1;
                  J1 := Tile_Count_X + 1;
                  J2 := Tile_Count_X + 2;

                  for S in 1..Tile_Span_Y loop

                     for K in 1..Tile_Count_X loop

                        Buffer.Load_Element (I1, I2, J2);

                        Buffer.Load_Element (J2, J1, I1);

                        I1 := I1 + 1;
                        I2 := I2 + 1;
                        J1 := J1 + 1;
                        J2 := J2 + 1;

                     end loop;

                     I1 := I1 + 1;
                     I2 := I2 + 1;
                     J1 := J1 + 1;
                     J2 := J1 + 1;

                  end loop;

                  Colormaps (C).Load (Buffer);

               end loop;

               -- Reset unusued colormaps
               ----------------------------------------------
               if N < Colormaps'Length then

                  Buffer.Reset;

                  for C in N + 1 .. Colormaps'Last loop

                     Colormaps (C).Load (Buffer);

                  end loop;

               end if;

            end;

         end;

      elsif not On_Clip then

         return;

      end if;

      -- Render
      --------------------------------------------------------------------------

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      for Colormap of Colormaps loop
         Colormap.Draw;
      end loop;

      Glex.Get_Transform.Load_Unit;

   exception
      when others =>
         Utility.Log.Put_Message ("error while drawing terrain");

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Elevation (Position : Position_Record) return Float is

      I, J : Integer := 0;

   begin

      if
        Cell_Size_Lon > 0.0 and then
        Cell_Size_Lat > 0.0 and then
        North_East.Lat >= Position.Lat and then
        North_East.Lon >= Position.Lon and then
        South_West.Lat <= Position.Lat and then
        South_West.Lon <= Position.Lon
      then

         I := Integer ((Position.Lon - South_West.Lon) / Long_Float (Cell_Size_Lon));

         J := Integer ((Position.Lat - South_West.Lat) / Long_Float (Cell_Size_Lat));

         if I in 1..N_Lon and J in 1..N_Lat then

            return Float (Altitude ((J - 1) * N_Lon + I));

         end if;

      end if;

      return 0.0;

   end Get_Elevation;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Info return String is

      use Utility.Strings;

   begin

      return "---";

   end Get_Info;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Inside_Area (Position : Position_Record) return Boolean is
   begin

      return
        North_East.Lat >= Position.Lat and then
        North_East.Lon >= Position.Lon and then
        South_West.Lat <= Position.Lat and then
        South_West.Lon <= Position.Lon;

   end Inside_Area;
   -----------------------------------------------------------------------------


end Maps.Terrain;
--------------------------------------------------------------------------------
