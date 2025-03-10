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
with Gnav_Info;
with Glex.Fonts;
with Glex.Symbols;
with Widgets.Widget;
use  Widgets.Widget;
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

   Reference_Info_Panel : Widget_Record;

   --===========================================================================
   -- Converts the character to a reference kind
   --===========================================================================
   function To_Reference_Kind (C : Character) return Reference_Kinds is
   begin

      case C is
         when 'A' => return Reference_Airfield;
         when 'C' => return Reference_City;
         when 'V' => return Reference_Village;
         when 'L' => return Reference_Landmark;
         when 'W' => return Reference_Water;
         when 'T' => return Reference_Turbine;
         when 'O' => return Reference_Woods;
         when 'P' => return Reference_Landouts;
         when others => return Reference_Landmark;
      end case;

   end To_Reference_Kind;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads the references from the requested file
   --===========================================================================
   procedure Load_References (S : in out Stream_Reader_Type) is
   begin

      Utility.Log.Put_Message ("loading refereces");

      -- Simple references

      Number_Of_References := S.Read_Natural;

      if Number_Of_References > References'Length then
         Number_Of_References := 0;
         Utility.Log.Put_Message ("warning: too many references, maximum is " & Integer_Image (References'Length));
         return;
      else
         Utility.Log.Put_Message (Integer_Image (Number_Of_References));
      end if;

      for I in 1..Number_Of_References loop

         References (I).Kind := To_Reference_Kind (S.Read_Character);

         References (I).Position.Lat := S.Read_Long_Float;
         References (I).Position.Lon := S.Read_Long_Float;

      end loop;

      -- Named references

      Number_Of_Named_References := S.Read_Natural;

      if Number_Of_Named_References > Named_References'Length then
         Number_Of_Named_References := 0;
         Utility.Log.Put_Message ("warning: too many named references, maximum is " & Integer_Image (Named_References'Length));
         return;
      end if;

      for I in 1..Number_Of_Named_References loop

         Named_References (I).Kind := To_Reference_Kind (S.Read_Character);

         Override (Named_References (I).Id,   S.Read_String ( 4));
         Override (Named_References (I).Name, S.Read_String (14));

         Named_References (I).Position.Lat := S.Read_Long_Float;
         Named_References (I).Position.Lon := S.Read_Long_Float;

      end loop;

      -- Airfields

      Number_Of_Airfields := S.Read_Natural;

      if Number_Of_Airfields > Airfields'Length then
         Number_Of_Airfields := 0;
         Utility.Log.Put_Message ("warning: too many airfields, maximum is " & Integer_Image (Airfields'Length));
         return;
      end if;

      for I in 1..Number_Of_Airfields loop

         Override (Airfields (I).Id,        S.Read_String ( 4));
         Override (Airfields (I).Name,      S.Read_String (14));

         Airfields (I).Stations (1) := Natural (S.Read_Short_Natural);
         Airfields (I).Stations (2) := Natural (S.Read_Short_Natural);
         Airfields (I).Stations (3) := Natural (S.Read_Short_Natural);

         Airfields (I).Position.Lat  := S.Read_Long_Float;
         Airfields (I).Position.Lon  := S.Read_Long_Float;

      end loop;

      -- Stations

      Number_Of_Stations := S.Read_Natural;

      if Number_Of_Stations > Stations'Length then
         Number_Of_Stations := 0;
         Utility.Log.Put_Message ("warning: too many radio stations, maximum is " & Integer_Image (Stations'Length));
         return;
      end if;

      for I in 1..Number_Of_Stations loop

         Override (Stations (I).Name,      S.Read_String (14));
         Override (Stations (I).Frequency, S.Read_String ( 7));

         Stations (I).Airfield := Natural (S.Read_Short_Natural);

      end loop;

      Utility.Log.Put_Message ("reference points loaded");

      -- Use the system reference position to initialize the distance

      Update_Distance_To_Airfields (Gnav_Info.Home_Position);

   end Load_References;
   -----------------------------------------------------------------------------


   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      -- Request the data
      --------------------------------------------------------------------------

      Utility.Resources.Request_Binary_Resource ("reference.bin",
                                                 Load_References'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Fonts
   -- TODO: maybe it would be better to use regular fonts and different colors
   --       for each navigation mode.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Fonts : constant array (Reference_Kinds) of Glex.Fonts.Font_Style_Record :=

     (Reference_City =>     (Width     => 0.008,
                             Height    => 0.020,
                             Space     => 0.007,
                             Rendering => Glex.Fonts.Font_Extra_Glow,
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
      Reference_Airfield => (Width     => 0.008,
                             Height    => 0.020,
                             Space     => 0.007,
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
   Lines : constant array (Reference_Kinds) of Glex.Colors.Line_Color_Record :=

     (Reference_City     => (Fore => (1.00, 1.00, 1.00, 1.00), --(0.80, 0.80, 0.80, 1.00)
                             Glow => (0.05, 0.05, 0.05, 1.00)),
      Reference_Village  => (Fore => (1.00, 1.00, 1.00, 1.00), --(0.85, 0.85, 0.25, 1.00),
                             Glow => (0.55, 0.55, 0.55, 1.00)),
      Reference_Woods    => (Fore => (0.60, 0.80, 0.60, 1.00),
                             Glow => (0.10, 0.40, 0.10, 1.00)),
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
      Count      : Natural := 0;

   begin

      Glex.Get_Transform.Load_Unit;

      View.Get_Limits (North_East,
                       South_West);

      -- Annonimous references (some woods, wind turbines, landmarks, etc.)
      --------------------------------------------------------------------------

      Count := 0;

      for Reference of References loop

         Count := Count + 1;
         exit when Count > Number_Of_References;

         if
           Reference.Position.Lat < North_East.Lat and then
           Reference.Position.Lon < North_East.Lon and then
           Reference.Position.Lat > South_West.Lat and then
           Reference.Position.Lon > South_West.Lon
         then

            View.Position_To_Screen (Reference.Position, X, Y);

            case Reference.Kind is

               when Reference_Woods =>

                  if View.Zoom < 0.20 then

                     Glex.Fonts.Draw ("}}}",
                                      X, Y,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_CC);

                  elsif View.Zoom < 0.40 then

                     Glex.Fonts.Draw ("}}",
                                      X, Y,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_CC);

                  else

                     Glex.Fonts.Draw ("}",
                                      X, Y,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_CC);

                  end if;

               when Reference_Turbine =>

                  if View.Zoom < 0.60 then

                     Glex.Fonts.Draw ("Y",
                                      X, Y,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_CC);

                  end if;

               when Reference_Landouts =>

                  Glex.Fonts.Draw ("X",
                                   X, Y,
                                   Fonts (Reference.Kind),
                                   Lines (Reference.Kind),
                                   Alignment_CC);

               when Reference_Landmark =>

                  if View.Zoom < 0.40 then

                     Glex.Symbols.Draw (Glex.Symbols.Triangle_Up,
                                        X, Y,
                                        Size      => 0.016,
                                        Color     => Lines (Reference.Kind).Glow,
                                        Alignment => Alignment_CC);

                  end if;

               when others => null;

            end case;

         end if;

      end loop;

      -- Named references (cities, woods, lakes, etc.)
      --------------------------------------------------------------------------

      Count := 0;

      for Reference of Named_References loop

         Count := Count + 1;
         exit when Count > Number_Of_Named_References;

         if
           Reference.Position.Lat < North_East.Lat and then
           Reference.Position.Lon < North_East.Lon and then
           Reference.Position.Lat > South_West.Lat and then
           Reference.Position.Lon > South_West.Lon
         then

            View.Position_To_Screen (Reference.Position, X, Y);

            case Reference.Kind is

               when Reference_Woods =>

                  if View.Zoom < 0.20 then

                     Glex.Fonts.Draw ("}}}",
                                      X, Y,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_CC);

                     Glex.Fonts.Draw (Trim (Reference.Name),
                                      X, Y - 0.03,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_TC);

                  elsif View.Zoom < 0.40 then

                     Glex.Fonts.Draw ("}}",
                                      X, Y,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_CC);

                     Glex.Fonts.Draw (Trim (Reference.Id),
                                      X, Y - 0.03,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_TC);

                  else

                     Glex.Fonts.Draw ("}",
                                      X, Y,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_CC);

                  end if;

               when Reference_Water =>

                  Glex.Fonts.Draw (Trim (Reference.Name),
                                   X, Y - 0.03,
                                   Fonts (Reference.Kind),
                                   Lines (Reference.Kind),
                                   Alignment_TC);

               when Reference_City =>

                  if View.Zoom > 0.40 then

                     Glex.Fonts.Draw (Trim (Reference.Id),
                                      X, Y + 0.025,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_LC);

                  else

                     Glex.Fonts.Draw (Trim (Reference.Name),
                                      X, Y + 0.025,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_LC);
                  end if;

                  Glex.Symbols.Draw (Glex.Symbols.Square,
                                     X, Y,
                                     Size      => 0.008,
                                     Color     => Lines (Reference.Kind).Glow,
                                     Alignment => Alignment_CC);

               when Reference_Village =>

                  if View.Zoom > 0.20 then

                     if View.Zoom < 0.40 then

                        Glex.Fonts.Draw (Trim (Reference.Id),
                                         X, Y + 0.025,
                                         Fonts (Reference.Kind),
                                         Lines (Reference.Kind),
                                         Alignment_LC);

                     end if;

                  else

                     Glex.Fonts.Draw (Trim (Reference.Name),
                                      X, Y + 0.025,
                                      Fonts (Reference.Kind),
                                      Lines (Reference.Kind),
                                      Alignment_LC);
                  end if;

                  Glex.Symbols.Draw (Glex.Symbols.Square,
                                     X, Y,
                                     Size      => 0.008,
                                     Color     => Lines (Reference.Kind).Glow,
                                     Alignment => Alignment_CC);

               when others => null;

            end case;

         end if;

      end loop;

      -- Airfields
      --------------------------------------------------------------------------

      Count := 0;

      for Airfield of Airfields loop

         Count := Count + 1;
         exit when Count > Number_Of_Airfields;

         if
           Airfield.Position.Lat < North_East.Lat and then
           Airfield.Position.Lon < North_East.Lon and then
           Airfield.Position.Lat > South_West.Lat and then
           Airfield.Position.Lon > South_West.Lon
         then

            View.Position_To_Screen (Airfield.Position, X, Y);

            Glex.Fonts.Draw (Trim (Airfield.Id),
                             X, Y + 0.025,
                             Fonts (Reference_Airfield),
                             Lines (Reference_Airfield),
                             Alignment_LC);

            Glex.Symbols.Draw (Glex.Symbols.Diamond,
                               X, Y,
                               Size      => 0.014,
                               Color     => Lines (Reference_Airfield).Glow,
                               Alignment => Alignment_CC);

         end if;

      end loop;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Is_Valid (This : Reference_Info) return Boolean is
   begin

      return This.Index /= 0;

   end Is_Valid;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Font used to draw the reference info box
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Font_1 : Glex.Fonts.Font_Style_Record :=  (Width     => 0.008,
                                              Height    => 0.026,
                                              Space     => 0.007,
                                              Rendering => Glex.Fonts.Font_Extra_Glow,
                                              Thickness => Glex.Fonts.Font_Bold);

   --===========================================================================
   --
   --===========================================================================
   procedure Draw (This : Reference_Info) is

      use Glex.Fonts;
      use Utility.Strings;

      X, Y : Float;
      Area : Allocation_Record;
      Has_Frequency : Boolean := False;

   begin

      if This.Index > 0 then

         Area.X := 0.35;
         Area.Y := 0.01;
         Area.W := 0.30;

         Glex.Get_Transform.Load_Unit;

         case This.Kind is

            when Stack_Airfield =>

               --Reference_Info_Panel.Draw;

               Y := 0.05;

               -- Display the local stations if there is at least one
               -----------------------------------------------------------------
               for I in Airfields (This.Index).Stations'Range loop

                  exit when Airfields (This.Index).Stations (I) = 0;

                  Has_Frequency := True;

                  Glex.Fonts.Draw (Trim (Stations (Airfields (This.Index).Stations (I)).Name),
                                   0.492, Y,
                                   Font_1,
                                   Line_Yellow,
                                   Alignment_LR);

                  Glex.Fonts.Draw (Stations (Airfields (This.Index).Stations (I)).Frequency,
                                   0.507, Y,
                                   Font_1,
                                   Line_Orange,
                                   Alignment_LL);

                  Y := Y + 0.05;

               end loop;

               -- If there are no radio stations, display the airfield name
               -----------------------------------------------------------------
               if not Has_Frequency then

                  Glex.Fonts.Draw (Trim (Airfields (This.Index).Name),
                                   0.45, Y,
                                   Font_1,
                                   Lines (Reference_Airfield),
                                   Alignment_LC);

               end if;

            when others =>

               null;

         end case;

         Area.H := Y + 0.07 - Area.Y;

         Reference_Info_Panel.Set_Allocation (Area);

      end if;

   end Draw;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Select_Reference (Position : Position_Record) return Reference_Info is

      Selection : Reference_Info;

   begin

      Selection.Index := 0;

      for I in 1 .. Number_Of_Airfields loop

         if Distance (Airfields (I).Position, Position) < 1.0 then

            Selection.Kind  := Stack_Airfield;

            Selection.Index := I;

            return Selection;

         end if;

      end loop;

      return Selection;

   end Select_Reference;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Clear (This : in out Reference_Info) is
   begin

      This.Index := 0;

   end Clear;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Contains (This : Reference_Info; X, Y : Float) return Boolean is
   begin

      return This.Index /= 0 and then Reference_Info_Panel.Contains (X, Y);

   end Contains;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Number_Of_Stations return Natural is
   begin

      return Number_Of_Stations;

   end Get_Number_Of_Stations;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Station_Name (Index : Natural) return String is
   begin

      if Index in Stations'Range then

         return Stations (Index).Name;

      else
         return "";

      end if;

   end Get_Station_Name;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Last_Position : Position_Record := No_Position_Record;
   --===========================================================================
   --
   --===========================================================================
   procedure Update_Distance_To_Airfields (Position : Position_Record) is
   begin

      if Position /= No_Position_Record then

         if
           Last_Position = No_Position_Record or else
           Distance (Position, Last_Position) > 1.0
         then

            Last_Position := Position;

            for A in 1..Number_Of_Airfields loop

               Airfields (A).Distance := Natural (Distance (Position, Airfields (A).Position));

            end loop;

         end if;

      end if;

   end Update_Distance_To_Airfields;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   function Get_Distance_To_Airfield (Index : Natural) return Natural is
   begin

      if Index in Airfields'Range then

         return Airfields (Index).Distance;

      else
         return 0;

      end if;

   end Get_Distance_To_Airfield;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Distance_To_Station (Index : Natural) return Natural is
   begin

      if Index in Stations'Range then

         return Get_Distance_To_Airfield (Stations (Index).Airfield);

      else
         return 0;

      end if;

   end Get_Distance_To_Station;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Station_Has_Airfield (Index : Natural) return Boolean is
   begin

      return Stations (Index).Airfield in 1..Number_Of_Airfields;

   end Station_Has_Airfield;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Station_Frequency (Index : Natural) return String is
   begin

      if Index in Stations'Range then

         return Stations (Index).Frequency;

      else
         return "";

      end if;

   end Get_Station_Frequency;
   -----------------------------------------------------------------------------


end Maps.Reference;
--------------------------------------------------------------------------------
