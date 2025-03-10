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
-- AdaWebPack
with Interfaces;
with System;
with Web.Strings;
use  Web.Strings;
with Web.Strings.WASM_Helpers;
--
with Utility.Strings;


--//////////////////////////////////////////////////////////////////////////////
-- 
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Storage is

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Set_Item (Key, Value : String) is
      
      use Utility.Strings;
      
      procedure Imported (Key_Address : System.Address;
                          Key_Size    : Interfaces.Unsigned_32;
                          Val_Address : System.Address;
                          Val_Size    : Interfaces.Unsigned_32)
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__storage__setItem";

      Key_A : System.Address;
      Key_S : Interfaces.Unsigned_32;

      Val_A : System.Address;
      Val_S : Interfaces.Unsigned_32;

      Key_W : constant Web.Strings.Web_String := To_Web_String (To_Wide_Wide_String (Key));
      Val_W : constant Web.Strings.Web_String := To_Web_String (To_Wide_Wide_String (Value));
      
   begin

      Web.Strings.WASM_Helpers.To_JS (Key_W, Key_A, Key_S);
      Web.Strings.WASM_Helpers.To_JS (Val_W, Val_A, Val_S);
                                        
      Imported (Key_A, Key_S, Val_A, Val_S);
      
   end Set_Item;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   function Get_Item (Key : String) return String is
      
      use Interfaces;
      use Utility.Strings;
      
      function Imported (Key_Address : System.Address;
                         Key_Size    : Unsigned_32) return System.Address
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__storage__getItem";

      Key_A : System.Address;
      Key_S : Unsigned_32;

      Key_W : constant Web.Strings.Web_String := To_Web_String (To_Wide_Wide_String (Key));
      
   begin
      
      Web.Strings.WASM_Helpers.To_JS (Key_W, Key_A, Key_S);
      
      if Key_S <= 0 then
         
         return "";
         
      else
         
         declare         
            Value : constant Web.Strings.Web_String := Web.Strings.WASM_Helpers.To_Ada (Imported (Key_A, Key_S));
         begin
            return To_String (To_Wide_Wide_String (Value));
         end;
         
      end if;
      
   end Get_Item;
   -----------------------------------------------------------------------------
   
end Utility.Storage;
--------------------------------------------------------------------------------
