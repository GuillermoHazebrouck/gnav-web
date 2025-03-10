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
with Utility;
use  Utility;
with Utility.Types;
use  Utility.Types;
with Utility.Maps;
use  Utility.Maps;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package References is

   --===========================================================================
   -- Opens all reference files and compiles them in binary format
   --===========================================================================
   procedure Compile_Data;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The type of reference
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
   -- The type of reference
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Reference_Kind_Key : array (Reference_Types) of Character :=
     (Reference_Airfield  => 'A',
      Reference_City      => 'C',
      Reference_Village   => 'V',
      Reference_Landmark  => 'L',
      Reference_Water     => 'W',
      Reference_Turbine   => 'T',
      Reference_Woods     => 'O',
      Reference_Landouts  => 'P');

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
   type Reference_Record is record

      Kind        : Reference_Types;

      Id          : Reference_Id;

      Name        : Reference_Name;

      Position    : Position_Record;

      Is_Loaded   : Boolean;

   end record;
   -----------------------------------------------------------------------------

   No_Reference_Record : constant Reference_Record := (Kind        => Reference_City,
                                                       Id          => No_Reference_Id,
                                                       Name        => No_Reference_Name,
                                                       Position    => No_Position_Record,
                                                       Is_Loaded   => False);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All references
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   References : array (1..1500) of Reference_Record := (others => No_Reference_Record);




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Frequency_Type is String (1..7);

   No_Frequency : constant Frequency_Type := (others => ' ');

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Frequency_Record is record

      Name      : Reference_Name;

      Frequency : Frequency_Type;

      Id        : Reference_Id;

      Airfield  : Short_Natural;

   end record;
   -----------------------------------------------------------------------------

   No_Frequency_Record : Frequency_Record := (Frequency => No_Frequency,
                                              Name      => No_Reference_Name,
                                              Id        => No_Reference_Id,
                                              Airfield  => 0);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All radio stations
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Radio_Stations : array (1..500) of Frequency_Record := (others => No_Frequency_Record);



   type Station_Array is array (1..3) of Short_Natural;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All airfields
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Airfield_Record is record

      Id        : Reference_Id;

      Name      : Reference_Name;

      Stations  : Station_Array;

      Position  : Position_Record;

      Is_Loaded : Boolean;

   end record;
   -----------------------------------------------------------------------------

   No_Airfield_Record : constant Airfield_Record := (Id        => No_Reference_Id,
                                                     Name      => No_Reference_Name,
                                                     Stations  => (others => 0),
                                                     Position  => No_Position_Record,
                                                     Is_Loaded => False);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Airfields : array (1..500) of Airfield_Record := (others => No_Airfield_Record);


end References;
--------------------------------------------------------------------------------
