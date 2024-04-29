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
package body Maps.Layers is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A static buffer to load the basic resources
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Basic_Limit  : constant Natural := 250;
   Basic_Buffer : Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (Basic_Limit);

   --===========================================================================
   -- Loads the reference through a server request
   --===========================================================================
   procedure Load_Map_Layers (S : in out Stream_Reader_Type) is

      L : Natural;
      P : Natural;
      C : Natural;
      R : Natural     := 0;
      K : Layer_Kinds := Layer_Unknown;

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

      -- Number of layers
      -----------------------------------
      L := S.Read_Natural;

      Utility.Log.Put_Message ("loading " & Integer_Image (L) & " map layers");
      Utility.Log.Put_Message ("stream size = " & Integer_Image (S.Get_Size));

      for I in 1 .. L loop

         -- Layer kind
         -----------------------------------

         case S.Read_String (1) (1) is
            when 'R'    => K := Layer_Rivers;
            when 'L'    => K := Layer_Lakes;
            when 'T'    => K := Layer_Rails;
            when 'S'    => K := Layer_Roads;
            when 'B'    => K := Layer_Borders;
            when 'A'    => K := Layer_Airspaces;
            when others => K := Layer_Unknown;
         end case;

         Utility.Log.Put_Message ("kind: " & Layer_Kinds'Image (K));

         -- Layer parts
         -----------------------------------
         P := S.Read_Natural;

         Utility.Log.Put_Message ("parts: " & Natural'Image (P));

         for J in 1 .. P loop

            if R >= Parts'Last then
               Utility.Log.Put_Message ("warning: part resources full");
               return;
            end if;

            R := R + 1;

            Parts (R).Kind := K;

            -- Part bounds
            -----------------------------------
            Parts (R).South_West.Lat := Long_Float (S.Read_Float) + Origin.Lat;
            Parts (R).South_West.Lon := Long_Float (S.Read_Float) + Origin.Lon;
            Parts (R).North_East.Lat := Long_Float (S.Read_Float) + Origin.Lat;
            Parts (R).North_East.Lon := Long_Float (S.Read_Float) + Origin.Lon;

            -- Part points
            -----------------------------------
            C := S.Read_Natural;

          --Utility.Log.Put_Message ("   " & Natural'Image (J) & " size: " & Natural'Image (C));

            if C > Basic_Limit then
               Utility.Log.Put_Message ("warning: layer line too long");
            end if;

            Basic_Buffer.Reset;

            for N in 1 .. C loop

               Y := (S.Read_Float + Float (Origin.Lat - Center.Lat));
               X := (S.Read_Float + Float (Origin.Lon - Center.Lon)) * Maps.Shrink;

               Basic_Buffer.Load_Node (X, Y);

            end loop;

            Parts (R).Base.Load (Basic_Buffer);
            Parts (R).Loaded := True;

         end loop;

      end loop;

   end Load_Map_Layers;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      Utility.Resources.Request_Binary_Resource ("layers.bin",
                                                 Load_Map_Layers'Access);

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (View : Map_View_Record) is

      N : Natural := 0;

   begin

      Glex.Get_Transform.Copy (View.Get_Geographic_Matrix);

      for P in Parts'Range loop

         if Parts (P).Loaded and then Visible (Parts (P).Kind) then

            if View.On_Clip (Parts (P).North_East,
                             Parts (P).South_West)
            then

               N := N + 1;

               Parts (P).Base.Draw (Colors (Parts (P).Kind), Glex.Basic.Line_Strip);

            end if;

         end if;

      end loop;

      Glex.Get_Transform.Load_Unit;

   end Draw;
   -----------------------------------------------------------------------------

end Maps.Layers;
--------------------------------------------------------------------------------
