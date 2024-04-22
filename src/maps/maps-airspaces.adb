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
with Ada.Streams;
use  Ada.Streams;
with Interfaces;
-- Gnav
with Glex.Fonts;
with Utility.Atmosphere;
use  Utility.Atmosphere;
with Utility.Log;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Airspaces is


   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Invalid_Lat_Long : exception;

   --===========================================================================
   -- Specific function for airspace position data (Eg. 505130N 0031110E)
   --===========================================================================
   function Sexagecimal_Value (Image : String) return Long_Float is

      Sign   : Long_Float := 1.0;
      Result : Long_Float := 0.0;
      F      : constant Natural := Image'First - 1;

   begin

      if Image'Length > 0 then

         case Image (Image'Last) is

         when 'N' | 'S' =>

            Result := Long_Float'Value (Image (F + 1..F + 2)) +
                      Long_Float'Value (Image (F + 3..F + 4)) / 60.0 +
                      Long_Float'Value (Image (F + 5..F + 6)) / 3600.0;

            if Image (Image'Last) = 'S' then
               Sign := -1.0;
            end if;

         when 'E' | 'O' =>

            Result := Long_Float'Value (Image (F + 1..F + 3)) +
                      Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                      Long_Float'Value (Image (F + 6..F + 7)) / 3600.0;

            if Image (Image'Last) = 'O' then
               Sign := -1.0;
            end if;

         when others =>

            case Image (Image'First) is

               when 'N' | 'S' =>

                  Result := Long_Float'Value (Image (F + 2..F + 3)) +
                            Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                            Long_Float'Value (Image (F + 6..F + 7)) / 3600.0;

                  if Image (Image'First) = 'S' then
                     Sign := -1.0;
                  end if;

               when 'E' | 'O' =>

                  Result := Long_Float'Value (Image (F + 2..F + 4)) +
                            Long_Float'Value (Image (F + 5..F + 6)) / 60.0 +
                            Long_Float'Value (Image (F + 7..F + 8)) / 3600.0;

                  if Image (Image'First) = 'O' then
                     Sign := -1.0;
                  end if;

               when others =>

                  null;

            end case;

         end case;

         return Sign * Result;

      else

         Utility.Log.Put_Message ("warning: invalid lat/long");

         raise Invalid_Lat_Long;

      end if;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Recomputes the altitude string based on the current units and QNH
   --===========================================================================
   procedure Recompute (This : in out Airspace_Record) is
   begin

      This.Label.Lower := To_Altitude (This.Limits.Lower);
      This.Label.Upper := To_Altitude (This.Limits.Upper);

   end Recompute;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Parse_Info (This : in out Airspace_Record; Info : String) is

      use Utility.Strings;

      Reader : String_Buffer (40);

   begin

      Reader.Load (Info);

      declare
         Kind : String := Reader.Read_Next ('/');
      begin
            if Kind = "CTR" then This.Kind := Airspace_Ctr;
         elsif Kind = "TMA" then This.Kind := Airspace_Tma;
         elsif Kind = "CTA" then This.Kind := Airspace_Cta;
         elsif Kind = "TMZ" then This.Kind := Airspace_Tmz;
         elsif Kind = "RMZ" then This.Kind := Airspace_Rmz;
         elsif Kind = "TSA" then This.Kind := Airspace_Tsa;
         else                    This.Kind := Airspace_Unk;
         end if;
      end;

      Override (This.Class,        Reader.Read_Next ('/'));
      Override (This.Limits.Lower, Reader.Read_Next ('/'));
      Override (This.Limits.Upper, Reader.Read_Next ('/'));
      This.Point.Lat := Sexagecimal_Value (Reader.Read_Next (' '));
      This.Point.Lon := Sexagecimal_Value (Reader.Read_Next (' '));

      This.Recompute;

   end Parse_Info;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Updates the vertical sector limits based on the current QNH and unit
   --===========================================================================
   procedure Recompute_Vertical_Limits is
   begin

      for A of Airspaces loop

         if A.Loaded then
            A.Recompute;
         end if;

      end loop;

   end Recompute_Vertical_Limits;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads the reference through a server request
   --===========================================================================
   procedure Load_Airspaces (S : in out Stream_Reader_Type) is

      N : Natural;
      P : Natural := 0;
      K : Airspace_Kinds := Airspace_Tma;

      X, Y   : Float;
      Origin : Position_Record;

   begin

      if S.Is_Empty then
         return;
      end if;

      -- Data origin
      -----------------------------------
      Origin.Lat := S.Read_Long_Float;
      Origin.Lon := S.Read_Long_Float;

      -- Number of sectors
      -----------------------------------
      N := S.Read_Natural;

      Utility.Log.Put_Message ("loading " & Integer_Image (N) & " airspaces");
      Utility.Log.Put_Message ("stream size = " & Integer_Image (S.Get_Size));

      for I in 1 .. N loop

         if I > Airspaces'Last then
            Utility.Log.Put_Message ("warning: part resources full");
            return;
         end if;

         -- Airspace name
         -----------------------------------
         Override (Airspaces (I).Name, S.Read_String (20));

         -- Airspace info
         -----------------------------------
         Airspaces (I).Parse_Info (S.Read_String (40));

         -- Airspace bounds
         -----------------------------------
         Airspaces (I).South_West.Lat := Long_Float (S.Read_Float) + Origin.Lat;
         Airspaces (I).South_West.Lon := Long_Float (S.Read_Float) + Origin.Lon;
         Airspaces (I).North_East.Lat := Long_Float (S.Read_Float) + Origin.Lat;
         Airspaces (I).North_East.Lon := Long_Float (S.Read_Float) + Origin.Lon;

         -- Airspace points
         -----------------------------------
         P := S.Read_Natural;

         if P > Lines_Limit then
            Utility.Log.Put_Message ("warning: airspace line too long");
         end if;

         Lines_Buffer.Reset;

         for J in 1 .. P loop

            Y := (S.Read_Float + Float (Origin.Lat - Center.Lat));
            X := (S.Read_Float + Float (Origin.Lon - Center.Lon)) * Maps.Shrink;

            Lines_Buffer.Line_To (X, Y);

         end loop;

         Lines_Buffer.Close;

         Lines_Buffer.Finish;

         Airspaces (I).Resource.Load (Lines_Buffer);

         Airspaces (I).Loaded := True;

      end loop;

   end Load_Airspaces;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      Utility.Resources.Request_Binary_Resource ("airspaces.bin",
                                                 Load_Airspaces'Access);

      Utility.Atmosphere.On_Qnh_Changed.Connect (Recompute_Vertical_Limits'Access);

      Utility.Atmosphere.On_Altitude_Unit_Changed.Connect (Recompute_Vertical_Limits'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw_Labels (View : Map_View_Record) is

      use Glex.Fonts;

      X, Y       : Float;
      North_East : Position_Record;
      South_West : Position_Record;
      Font       : Font_Style_Record := (Width     => 0.008,
                                         Height    => 0.022,
                                         Space     => 0.006,
                                         Rendering => Glex.Fonts.Font_Glow,
                                         Thickness => Glex.Fonts.Font_Regular);

      Color : Line_Color_Record := (Fore => Color_White,
                                    Glow => Color_Gray_8);

   begin

      View.Get_Limits (North_East, South_West);

      for I in Airspaces'Range loop

         if
           Airspaces (I).Loaded                     and then
           Airspaces (I).Point.Lon < North_East.Lon and then
           Airspaces (I).Point.Lat < North_East.Lat and then
           Airspaces (I).Point.Lon > South_West.Lon and then
           Airspaces (I).Point.Lat > South_West.Lat
         then

            View.Position_To_Screen (Airspaces (I).Point, X, Y);

            Color.Glow := Colors (Airspaces (I).Kind);

            Glex.Fonts.Draw (Trim (Airspaces (I).Label.Upper),
                             X         => X,
                             Y         => Y + 0.01,
                             Style     => Font,
                             Color     => Color,
                             Alignment => Alignment_LC);

            Glex.Fonts.Draw (Trim (Airspaces (I).Label.Lower),
                             X         => X,
                             Y         => Y - 0.01,
                             Style     => Font,
                             Color     => Color,
                             Alignment => Alignment_TC);

            if View.Zoom < 0.6 then

               Glex.Fonts.Draw (Trim (Airspaces (I).Name),
                                X         => X,
                                Y         => Y + 0.05,
                                Style     => Font,
                                Color     => Color,
                                Alignment => Alignment_LC);

            end if;

         end if;

      end loop;

   end Draw_Labels;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (View : Map_View_Record) is
   begin

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      for I in Airspaces'Range loop

         if
           Airspaces (I).Loaded         and then
           Visible (Airspaces (I).Kind) and then
           View.On_Clip (Airspaces (I).North_East,
                         Airspaces (I).South_West)
         then

            Airspaces (I).Resource.Draw (Colors (Airspaces (I).Kind), 0.002);

         end if;

      end loop;

      Glex.Get_Transform.Load_Unit;

   end Draw;
   -----------------------------------------------------------------------------

end Maps.Airspaces;
--------------------------------------------------------------------------------
