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
with Ada.Numerics.Discrete_Random;

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
package body Utility.Ids is
         
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Random_Range is range 1..36;
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   package Randoms is new Ada.Numerics.Discrete_Random (Random_Range);
      
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Generator : Randoms.Generator;
   
   --===========================================================================
   --
   --===========================================================================
   function Get_New_Id return String is
      
      N  : Random_Range;
      
      Id : String (1..8);
      
   begin
      
      for I in Id'Range loop
         
         N := Randoms.Random (Generator);
         
         case N is
            when  1 => Id (I) := 'A';
            when  2 => Id (I) := 'B';
            when  3 => Id (I) := 'C';
            when  4 => Id (I) := 'D';
            when  5 => Id (I) := 'E';
            when  6 => Id (I) := 'F';
            when  7 => Id (I) := 'G';  
            when  8 => Id (I) := 'H'; 
            when  9 => Id (I) := 'I'; 
            when 10 => Id (I) := 'J'; 
            when 11 => Id (I) := 'K'; 
            when 12 => Id (I) := 'L'; 
            when 13 => Id (I) := 'M'; 
            when 14 => Id (I) := 'N'; 
            when 15 => Id (I) := 'O'; 
            when 16 => Id (I) := 'P'; 
            when 17 => Id (I) := 'Q'; 
            when 18 => Id (I) := 'R'; 
            when 19 => Id (I) := 'S'; 
            when 20 => Id (I) := 'T'; 
            when 21 => Id (I) := 'U'; 
            when 22 => Id (I) := 'V'; 
            when 23 => Id (I) := 'W'; 
            when 24 => Id (I) := 'X'; 
            when 25 => Id (I) := 'Y'; 
            when 26 => Id (I) := 'Z'; 
            when 27 => Id (I) := '0';
            when 28 => Id (I) := '1';
            when 29 => Id (I) := '2';
            when 30 => Id (I) := '3';
            when 31 => Id (I) := '4';
            when 32 => Id (I) := '5';
            when 33 => Id (I) := '6';
            when 34 => Id (I) := '7';
            when 35 => Id (I) := '8';
            when 36 => Id (I) := '9';
         end case;
               
      end loop;
      
      return Id;
      
   end Get_New_Id;

begin
   
   Randoms.Reset (Generator);
   
end Utility.Ids;
--------------------------------------------------------------------------------
