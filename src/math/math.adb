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

--//////////////////////////////////////////////////////////////////////////////
-- This namespace contains strictly mathematical objects for general purpose.
-- This is a low level namespace containing no references to implementation
-- dependent namespaces.
-- No specific functions/objects are allowed here, they all must be for general
-- purpose. This has to be kept like this for the sake of code clarity and
-- portability.
--//////////////////////////////////////////////////////////////////////////////
package body Math is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Float_Type_Value (Value : String; Default : Float_Type := Default_Value) return Float_Type is

      Number : Float_Type := 0.0;
      Factor : Float_Type := 1.0;
      Start  : Boolean    := False;
      Finish : Boolean    := False;
      Decimal: Boolean    := False;
      Signed : Boolean    := False;

   begin

      for D of reverse Value loop

         case D is

            when ' ' =>

               if Start then
                  Finish := True;
               end if;

            when '.' =>

               if Decimal then
                  return Default;
               else
                  Number  := Number / Factor;
                  Factor  := 1.0;
                  Decimal := True;
               end if;

            when '-' =>

               if Signed then
                  return Default;
               else
                  Signed := True;
               end if;

            when '0'..'9' =>

               Start := True;

               if Finish then
                  return Default;
               end if;

               case D is
                  when '1' => Number := Number + 1.0 * Factor;
                  when '2' => Number := Number + 2.0 * Factor;
                  when '3' => Number := Number + 3.0 * Factor;
                  when '4' => Number := Number + 4.0 * Factor;
                  when '5' => Number := Number + 5.0 * Factor;
                  when '6' => Number := Number + 6.0 * Factor;
                  when '7' => Number := Number + 7.0 * Factor;
                  when '8' => Number := Number + 8.0 * Factor;
                  when '9' => Number := Number + 9.0 * Factor;
                  when others => null;
               end case;

               Factor := 10.0 * Factor;

            when others =>

               return Default;

         end case;

      end loop;

      if Signed then
         return -Number;
      else
         return  Number;
      end if;

   end Float_Type_Value;
   -----------------------------------------------------------------------------

end Math;
--------------------------------------------------------------------------------
