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
   type Reference_Kinds is (Reference_Airfield,
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
   Visible : array (Reference_Kinds) of Boolean := (others => True);

   --===========================================================================
   -- Initializes the data and launches the loader.
   --===========================================================================
   procedure Initialize;

   --===========================================================================
   -- Draws the map references in geographic coordinates
   --===========================================================================
   procedure Draw (View : Map_View_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- An object pointing to a given reference
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Reference_Info is tagged private;

   --===========================================================================
   -- Indicates if the reference exists
   --===========================================================================
   function Is_Valid (This : Reference_Info) return Boolean;

   --===========================================================================
   -- Draws an info panel with information associated to the given reference
   --===========================================================================
   procedure Draw (This : Reference_Info);

   --===========================================================================
   -- Clears the reference
   --===========================================================================
   procedure Clear (This : in out Reference_Info);

   --===========================================================================
   -- Indicates if the info panel is active and contains the given coordinates
   --===========================================================================
   function Contains (This : Reference_Info; X, Y : Float) return Boolean;

   --===========================================================================
   --
   --===========================================================================
   function Select_Reference (Position : Position_Record) return Reference_Info;

   --===========================================================================
   -- Returns the distance to the given airfield in km
   --===========================================================================
   function Get_Distance_To_Airfield (Index : Natural) return Natural;

   --===========================================================================
   --
   --===========================================================================
   procedure Update_Distance_To_Airfields (Position : Position_Record);

   --===========================================================================
   --
   --===========================================================================
   function Get_Number_Of_Stations return Natural;

   --===========================================================================
   --
   --===========================================================================
   function Get_Station_Name (Index : Natural) return String;

   --===========================================================================
   --
   --===========================================================================
   function Get_Station_Frequency (Index : Natural) return String;

   --===========================================================================
   -- Returns the distance to the given station in km
   --===========================================================================
   function Get_Distance_To_Station (Index : Natural) return Natural;

   --===========================================================================
   --
   --===========================================================================
   function Station_Has_Airfield (Index : Natural) return Boolean;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stack_Kinds is (Stack_General, Stack_Named, Stack_Airfield);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Reference_Info is tagged record

      Kind  : Stack_Kinds := Stack_Airfield;

      Index : Natural := 0;

   end record;

   --///////////////////////////////////////////////////////////////////////////
   -- Annonimous references (wind turbines, obstacles, etc)
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Reference_Record is tagged record

      Kind     : Reference_Kinds;

      Position : Position_Record;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All general references that don't need a name, just icons
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   References : array (1..750) of Reference_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Number of references
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Number_Of_References : Natural := 0;

   --///////////////////////////////////////////////////////////////////////////
   -- Named references (cities, lakes, etc)
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Reference_Id is String (1..4);

   No_Reference_Id : constant Reference_Id := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Reference_Name is String (1..14);

   No_Reference_Name : constant Reference_Name := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Named_Reference_Record is new Reference_Record with record

      Id   : Reference_Id;

      Name : Reference_Name;

   end record;

   No_Named_Reference_Record : constant Named_Reference_Record := (Kind     => Reference_City,
                                                                   Id       => No_Reference_Id,
                                                                   Name     => No_Reference_Name,
                                                                   Position => No_Position_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All named references, like cities, woods, etc.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Named_References : array (1..750) of Named_Reference_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Number of references
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Number_Of_Named_References : Natural := 0;

   --///////////////////////////////////////////////////////////////////////////
   -- Airfields
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Station_Array is array (1..3) of Natural;

   No_Station_Array : constant Station_Array := (others => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Airfield_Record is record

      Position  : Position_Record;

      Id        : Reference_Id;

      Name      : Reference_Name;

      Stations  : Station_Array;

      Elevation : Natural;

      Distance  : Natural;

   end record;

   No_Airfield_Record : constant Airfield_Record := (Id        => No_Reference_Id,
                                                     Name      => No_Reference_Name,
                                                     Stations  => No_Station_Array,
                                                     Elevation => 0,
                                                     Distance  => 0,
                                                     Position  => No_Position_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All airfields.
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Airfields : array (1..200) of Airfield_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Number of airfields
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Number_Of_Airfields : Natural := 0;

   --///////////////////////////////////////////////////////////////////////////
   -- Radio stations for airfieds and other sectors
   --///////////////////////////////////////////////////////////////////////////

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Frequencies
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Frequency_String is String (1..7);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Frequency : constant Frequency_String := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Radio station
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Station_Record is record

      Name      : Reference_Name;

      Frequency : Frequency_String;

      Airfield  : Natural;

   end record;

   No_Station_Record : constant Station_Record := (Name      => No_Reference_Name,
                                                   Frequency => No_Frequency,
                                                   Airfield  => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Stations : array (1..200) of Station_Record := (others => No_Station_Record);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Number_Of_Stations : Natural := 0;

end Maps.Reference;
