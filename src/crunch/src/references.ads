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

   No_Reference_Record : constant Reference_Record := (Kind        => Reference_City,
                                                       Id          => (others => ' '),
                                                       Name        => (others => ' '),
                                                       Position    => No_Position_Record,
                                                       Is_Loaded   => False);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- All references
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   References : array (1..500) of Reference_Record;

end References;
