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
with Glex.Colors;
use  Glex.Colors;
with Math.Vector2;
use  Math.Vector2;
with Utility.Strings;
use  Utility.Strings;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Maps.Reference is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The different kind of references
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Reference_Types is (Reference_Airfield,
                            Reference_City,
                            Reference_Village,
                            Reference_Landmark,
                            Reference_Water,
                            Reference_Turbine,
                            Reference_Woods,
                            Reference_Landouts);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Indicates which references must be displayed when calling Draw
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Visible : array (Reference_Types) of Boolean := (others => True);

   --===========================================================================
   -- Initializes the data and launches the loader.
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Draws the map references in geographic coordinates
   --===========================================================================
   procedure Draw (View : Map_View_Record);

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Reference_Id is String (1..4);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Reference_Name is String (1..14);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Reference_Record is tagged record

      Kind        : Reference_Types;

      Id          : Reference_Id;

      Name        : Reference_Name;

      Position    : Position_Record;

      Is_Loaded   : Boolean;

   end record;

   No_Reference_Record : constant Reference_Record := (Kind      => Reference_City,
                                                       Id        => (others => ' '),
                                                       Name      => (others => ' '),
                                                       Position  => No_Position_Record,
                                                       Is_Loaded => False);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All references
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   References : array (1..500) of Reference_Record;

end Maps.Reference;
