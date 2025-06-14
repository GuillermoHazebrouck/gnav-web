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
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Utility is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A variable length string
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype Dynamic_String is Unbounded_String;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- An empty dynamic string
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Empty_String : constant Dynamic_String := Null_Unbounded_String;
   
   --===========================================================================
   -- To dynamic string
   --===========================================================================
   function "+" (Text : String) return Dynamic_String renames To_Unbounded_String;
   
   --===========================================================================
   -- To static string
   --===========================================================================
   function "-" (Text : Dynamic_String) return String renames To_String;
   
   --===========================================================================
   -- To static string
   --===========================================================================
   function "&" (Left, Right : Dynamic_String) return Dynamic_String renames Ada.Strings.Unbounded."&";
   
   --===========================================================================
   -- To dynamic string
   --===========================================================================
   function "&" (Left : Dynamic_String; Right : String) return Dynamic_String;
   
   --===========================================================================
   -- To static string
   --===========================================================================
   function Length (Text : Dynamic_String) return Natural renames Ada.Strings.Unbounded.Length;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type String_Buffer (Buffer_Size : Positive) is tagged private;
   
   --===========================================================================
   -- Loads the string into the buffer of the given object
   --===========================================================================
   procedure Load (Object : in out String_Buffer; Data : String);
   
   --===========================================================================
   -- Reads the next item from the current cursor position
   --===========================================================================
   function Read_Next (Object    : in out String_Buffer; 
                       Separator : Character := ' ';
                       Multiple  : Boolean := False) return String;
   
   --===========================================================================
   -- Moves the cursor to the next instance of the given character
   --===========================================================================
   procedure Move_To_Next (Object    : in out String_Buffer; 
                           Separator : Character := ' ');
      
   --===========================================================================
   -- Indicates if the end of the stream has been reached
   --===========================================================================
   function End_Of_Stream (Object : in out String_Buffer) return Boolean;
   
   --===========================================================================
   -- Writes the content of the buffer in the standard output
   --===========================================================================
   procedure Dump_Content (Object : in out String_Buffer);
   
   --===========================================================================
   --
   --===========================================================================
   function Current (Object : String_Buffer) return Character;
   
   --===========================================================================
   -- Returns the text in upper case characters
   --===========================================================================
   function Get_Upper_Case (Text : String) return String;
   
   --===========================================================================
   -- Returns the text in lower case characters
   --===========================================================================
   function Get_Lower_Case (Text : String) return String;
   
   --===========================================================================
   -- Writes the Text_2 into Text_1 and completes the eventual trailing gap
   -- with spaces.
   --===========================================================================
   procedure Override (Text_1: in out String; Text_2 : String; Fill : Character := ' '; Reversed : Boolean := False);
   
   --===========================================================================
   -- Indicates if the text contains the given pattern
   --===========================================================================
   function Contains (Text : String; Pattern : String) return Boolean;
   
   --===========================================================================
   -- Indicates if the text contains only numbers
   --===========================================================================
   function Is_Numerical (Text : String) return Boolean;
   
   --===========================================================================
   -- Trims the text by removing the leading and trailing spaces
   --===========================================================================
   function Trim (Text : String) return String;
       
   --===========================================================================
   -- Returns the text after replacing the given character
   --===========================================================================
   function Replace (Text : String; Original : Character; Target : Character) return String;
   
   --===========================================================================
   -- Returns the image of the integer value
   --===========================================================================
   function Integer_Image (Value : Integer) return String;
   
   --===========================================================================
   -- Returns the image of the floating point value
   --===========================================================================
   function Float_Image (Value : Float; Decimals : Natural) return String;
   
   --===========================================================================
   -- Transforms the 8 hexa characters into a 4 bytes decimal
   --===========================================================================
   function Parse_Hex_Id (Value : String) return Natural;
   
private
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type String_Buffer (Buffer_Size : Positive) is tagged record
     
      Buffer : String (1..Buffer_Size);

      Offset : Positive := 1;

      Length : Positive := 1;

      Loaded : Boolean  := False;
      
   end record;
   -----------------------------------------------------------------------------
   
end Utility;
--------------------------------------------------------------------------------
