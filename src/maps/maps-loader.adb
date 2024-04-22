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
with Ada.Directories;
with Ada.Text_IO;
-- Gnav
with Maps.Terrain;
with Maps.Layers;
with Maps.Layers.Airspaces;
with Maps.Reference;
with Utility;
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Maps.Loader is

   --===========================================================================
   -- Returns the path to the given dataset name
   --===========================================================================
   function Get_Path (Name : String) return String is
   begin

      return Utility.Base_Directory & "maps/" & Name & "/";

   end Get_Path;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads a given dataset
   --===========================================================================
   procedure Load_Dataset (Name : String) is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Id : File_Type;
      Folder  : String := Get_Path (Name);

   begin

      if Name = "" then

         Utility.Log.Put_Message ("warning: map dataset not provided");

         return;

      elsif not Ada.Directories.Exists (Folder) then

         Utility.Log.Put_Message ("invalid map dataset " & Name);

         return;

      elsif Name = -Dataset_Name then

         Utility.Log.Put_Message ("map data for " & Name & " already loaded");

         return;

      else
         Dataset_Name := +Name;

         Dataset_Path := +Folder;

      end if;

      Utility.Log.Put_Message ("loading map data for " & Name & " in " & Folder);

      Maps.Terrain.Load_Grid_File;

      Maps.Layers.Load_Shape_Files;

      Maps.Layers.Airspaces.Generate_Airspaces;

      Maps.Reference.Load_Reference_File;

   exception

      when others =>

         Utility.Log.Put_Message ("problem while loading dataset " & Name);

         Close (File_Id);

   end Load_Dataset;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the list of datasets in memory
   --===========================================================================
   function Get_Datasets return Dataset_Array is
   begin

      return (others => Empty_String);

   end Get_Datasets;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Removes the given dataset
   --===========================================================================
   procedure Delete_Dataset (Name : Dynamic_String) is
   begin

      null;

   end Delete_Dataset;
   -----------------------------------------------------------------------------



end Maps.Loader;
--------------------------------------------------------------------------------
