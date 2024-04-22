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
with Utility.Log;
with Utility.Strings;
use  Utility.Strings;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;



--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Reference is

   --===========================================================================
   -- Loads the references from the requested file
   --===========================================================================
   procedure Load_References (S : in out Stream_Reader_Type) is

      R : Natural;
      K : Character;

   begin

      R := S.Read_Natural;
      R := Natural'Min (References'Length, R);

      Utility.Log.Put_Message ("loading " & Integer_Image (R) & " reference points");

      for I in 1..R loop

         K := S.Read_Character;

         case K is
            when 'A' => References (I).Kind := Reference_Airfield;
            when 'C' => References (I).Kind := Reference_City;
            when 'V' => References (I).Kind := Reference_Village;
            when 'L' => References (I).Kind := Reference_Landmark;
            when 'W' => References (I).Kind := Reference_Water;
            when 'T' => References (I).Kind := Reference_Turbine;
            when 'O' => References (I).Kind := Reference_Woods;
            when 'P' => References (I).Kind := Reference_Landouts;
            when others => null;
         end case;

         Override (References (I).Id,   S.Read_String ( 4));
         Override (References (I).Name, S.Read_String (14));

         References (I).Position.Lat := S.Read_Long_Float;
         References (I).Position.Lon := S.Read_Long_Float;

         References (I).Is_Loaded := True;

      end loop;

      Utility.Log.Put_Message ("reference points loaded");

   end Load_References;
   -----------------------------------------------------------------------------


   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      Utility.Resources.Request_Binary_Resource ("reference.bin",
                                                 Load_References'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fonts
   -- TODO: maybe it would be better to use regular fonts and different colors
   --       for each navigation mode.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Fonts : constant array (Reference_Types) of Glex.Fonts.Font_Style_Record :=

     (Reference_City =>     (Width     => 0.007,
                             Height    => 0.018,
                             Space     => 0.006,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold),
      Reference_Village =>  (Width     => 0.007,
                             Height    => 0.018,
                             Space     => 0.006,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold),
      Reference_Woods =>    (Width     => 0.007,
                             Height    => 0.018,
                             Space     => 0.005,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold),
      Reference_Airfield => (Width     => 0.007,
                             Height    => 0.018,
                             Space     => 0.006,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold),
      Reference_Landouts => (Width     => 0.007,
                             Height    => 0.018,
                             Space     => 0.006,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold),
      Reference_Turbine =>  (Width     => 0.012,
                             Height    => 0.022,
                             Space     => 0.006,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold),
      Reference_Landmark => (Width     => 0.012,
                             Height    => 0.022,
                             Space     => 0.006,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold),
      Reference_Water    => (Width     => 0.007,
                             Height    => 0.018,
                             Space     => 0.006,
                             Rendering => Glex.Fonts.Font_Glow,
                             Thickness => Glex.Fonts.Font_Bold));

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Colors
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Lines : constant array (Reference_Types) of Glex.Colors.Line_Color_Record :=

     (Reference_City     => (Fore => (1.00, 1.00, 1.00, 1.00), --(0.80, 0.80, 0.80, 1.00)
                             Glow => (0.35, 0.35, 0.35, 1.00)),
      Reference_Village  => (Fore => (1.00, 1.00, 1.00, 1.00), --(0.85, 0.85, 0.25, 1.00),
                             Glow => (0.55, 0.55, 0.55, 1.00)),
      Reference_Woods    => (Fore => (0.60, 0.80, 0.60, 1.00),
                             Glow => (0.30, 0.50, 0.30, 1.00)),
      Reference_Airfield => (Fore => (1.00, 1.00, 1.00, 1.00), --(0.80, 0.80, 0.80, 1.00),
                             Glow => (0.30, 0.30, 1.00, 1.00)),
      Reference_Landouts => (Fore => (1.00, 1.00, 1.00, 1.00), --(0.80, 0.80, 0.80, 1.00),
                             Glow => (0.30, 0.30, 1.00, 1.00)),
      Reference_Turbine  => (Fore => (1.00, 1.00, 1.00, 1.00),
                             Glow => (0.30, 0.30, 0.30, 1.00)),
      Reference_Landmark => (Fore => (1.00, 1.00, 1.00, 1.00), --(0.60, 0.60, 0.00, 1.00),
                             Glow => (0.80, 0.20, 0.20, 0.60)),
      Reference_Water    => (Fore => (0.00, 1.00, 1.00, 1.00),
                             Glow => (0.50, 0.60, 1.00, 1.00)));

   --===========================================================================
   --
   --===========================================================================
   procedure Draw (View : Map_View_Record) is

      use Glex.Fonts;
      use Utility.Strings;

      North_East : Position_Record;
      South_West : Position_Record;
      X, Y       : Float;

   begin

      Glex.Get_Transform.Load_Unit;

      View.Get_Limits (North_East,
                       South_West);

      for R in References'Range loop

         exit when not References (R).Is_Loaded;

         if
           References (R).Position.Lat < North_East.Lat and then
           References (R).Position.Lon < North_East.Lon and then
           References (R).Position.Lat > South_West.Lat     and then
           References (R).Position.Lon > South_West.Lon
         then

            View.Position_To_Screen (References (R).Position, X, Y);

            case References (R).Kind is

               when Reference_Woods =>

                  if View.Zoom < 0.20 then

                     Glex.Fonts.Draw ("}}}",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                     Glex.Fonts.Draw (Trim (References (R).Name),
                                      X, Y - 0.03,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_TC);

                  elsif View.Zoom < 0.40 then

                     Glex.Fonts.Draw ("}}",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                     Glex.Fonts.Draw (Trim (References (R).Id),
                                      X, Y - 0.03,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_TC);

                  else

                     Glex.Fonts.Draw ("}",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                  end if;

               when Reference_Turbine =>

                  if View.Zoom < 0.60 then

                     Glex.Fonts.Draw ("Y",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                  end if;

               when Reference_Landouts =>

                  Glex.Fonts.Draw ("X",
                                   X, Y,
                                   Fonts (References (R).Kind),
                                   Lines (References (R).Kind),
                                   Alignment_CC);

                  if View.Zoom < 0.12 then

                     Glex.Fonts.Draw (Trim (References (R).Name),
                                      X, Y - 0.03,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_TC);

                  end if;

               when Reference_Landmark =>

                  if View.Zoom < 0.40 then

                     Glex.Fonts.Draw ("}",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                  end if;

               when Reference_Water =>

                  if View.Zoom < 0.12 then

                     Glex.Fonts.Draw ("~~~",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                     Glex.Fonts.Draw (Trim (References (R).Name),
                                      X, Y - 0.03,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_TC);

                  elsif View.Zoom < 0.20 then

                     Glex.Fonts.Draw ("~~",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                     Glex.Fonts.Draw (Trim (References (R).Id),
                                      X, Y - 0.03,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_TC);

                  else

                     Glex.Fonts.Draw ("~",
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);

                  end if;

               when Reference_Airfield =>

                  Glex.Fonts.Draw (Trim (References (R).Id),
                                   X, Y,
                                   Fonts (References (R).Kind),
                                   Lines (References (R).Kind),
                                   Alignment_CC);

               when Reference_City |
                    Reference_Village =>

                  if View.Zoom > 0.20 then

                     if
                       References (R).Kind = Reference_City or else
                       View.Zoom < 0.40
                     then

                        Glex.Fonts.Draw (Trim (References (R).Id),
                                         X, Y,
                                         Fonts (References (R).Kind),
                                         Lines (References (R).Kind),
                                         Alignment_CC);

                     end if;

                  else

                     Glex.Fonts.Draw (Trim (References (R).Name),
                                      X, Y,
                                      Fonts (References (R).Kind),
                                      Lines (References (R).Kind),
                                      Alignment_CC);
                  end if;

            end case;

         end if;

      end loop;

   end Draw;
   -----------------------------------------------------------------------------

end Maps.Reference;
