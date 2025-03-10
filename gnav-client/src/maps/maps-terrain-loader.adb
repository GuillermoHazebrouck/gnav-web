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
with Timing.Events;
with Utility.Streams;
use  Utility.Streams;
with Utility.Strings;
use  Utility.Strings;
with Utility.Log;
with Utility.Requests;
use  Utility.Requests;
with Utility.Resources;

--//////////////////////////////////////////////////////////////////////////////
-- This package manages the terrain data and representation. The terrain is
-- loaded in at most 14 regions of maximum 1.000.000 grid nodes.
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Terrain.Loader is

   Parts : Natural := 0;

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Terrain_Part (S : in out Stream_Reader_Type; Header : Boolean) is
   begin

      if not S.Is_Empty then

         if Header then

            -- This is the info header
            --------------------------------------------------------------------

            Utility.Log.Put_Message ("loading info");

            N_Lon := S.Read_Natural; --4
            N_Lat := S.Read_Natural; --8

            if N_Lon * N_Lat > Altitude'Length then

               Utility.Log.Put_Message ("terrain region too large");

               return;

            end if;

            South_West.Lat := S.Read_Long_Float; -- 16
            South_West.Lon := S.Read_Long_Float; -- 24

            Cell_Size_Lon := S.Read_Float; -- 28
            Cell_Size_Lat := S.Read_Float; -- 32

            Z_Min_Global := S.Read_Float; -- 36
            Z_Max_Global := S.Read_Float; -- 40

            North_East.Lat := South_West.Lat + Long_Float (N_Lat) * Long_Float (Cell_Size_Lat);
            North_East.Lon := South_West.Lon + Long_Float (N_Lon) * Long_Float (Cell_Size_Lon);

         end if;

         -- This is an elevation data file
         -----------------------------------------------------------------------

         declare
            F : Natural := S.Read_Natural;
            L : Natural := F + (S.Get_Size - 4) / 2 - 1; --> 2 bytes for each elevation value
         begin

            if F in Altitude'Range and then L in Altitude'Range then

               for I in F .. L loop
                  exit when S.Is_Empty;
                  Altitude (I) := S.Read_Short_Integer;
               end loop;

            end if;

         end;

         -- Check if everything has been loaded
         --------------------------------------

         Parts := Parts + 1;

         if Parts = 5 then
            Loaded := True;
            Utility.Log.Put_Message ("terrain loaded");
            Maps.Terrain.On_Loaded.Trigger;
         end if;

      end if;

   end Load_Terrain_Part;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Terrain_Head (S : in out Stream_Reader_Type) is
   begin

      Load_Terrain_Part (S, True);

   end Load_Terrain_Head;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Terrain_Rest (S : in out Stream_Reader_Type) is
   begin

      Load_Terrain_Part (S, False);

   end Load_Terrain_Rest;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is
   begin

      -- Open all requests and wait for the responses
      -------------------------------------------------------

      Utility.Log.Put_Message ("requesting terrain files");

      Utility.Resources.Request_Binary_Resource ("terrain_1.bin", Load_Terrain_Head'Access);
      Utility.Resources.Request_Binary_Resource ("terrain_2.bin", Load_Terrain_Rest'Access);
      Utility.Resources.Request_Binary_Resource ("terrain_3.bin", Load_Terrain_Rest'Access);
      Utility.Resources.Request_Binary_Resource ("terrain_4.bin", Load_Terrain_Rest'Access);
      Utility.Resources.Request_Binary_Resource ("terrain_5.bin", Load_Terrain_Rest'Access);

   end Initialize;
   -----------------------------------------------------------------------------


end Maps.Terrain.Loader;
--------------------------------------------------------------------------------
