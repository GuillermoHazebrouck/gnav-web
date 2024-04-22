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
with Utility.Calendar;
use  Utility.Calendar;

-- Gnav
with Math;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Utility.Strings is
    
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype String_12 is String (1..12);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype String_20 is String (1..20);
   
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
                       Separator : Character := ' ') return String;
   
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
   -- Returns the image of the floating point value (own implementation)
   --===========================================================================
   function Float_Image (Value : Float; Decimals : Natural) return String;
   
   --===========================================================================
   --
   --===========================================================================
   function Float_Value is new Math.Float_Type_Value (Float_Type    => Float, 
                                                      Default_Value => Math.No_Float);
   
   --===========================================================================
   --
   --===========================================================================
   function Long_Float_Value is new Math.Float_Type_Value (Float_Type    => Long_Float, 
                                                           Default_Value => Math.No_Long_Float);
   
   --===========================================================================
   -- Returns the date component of the given time (Eg. DD/MM/YYYY)
   --===========================================================================
   function Times_Image (Value : Times) return String;
   
   --===========================================================================
   -- Returns a date from a date/time representation (DD/MM/YYYY, HH:MM)
   --===========================================================================
   function Times_Value (Date : String; Hour : String) return Times;
   
   --===========================================================================
   -- Returns a duration in number of hours and minutes (HH:MM)
   --===========================================================================
   function Day_Lapse_Value (Span : String) return Day_Lapse;
   
   --===========================================================================
   -- Returns the image of the clock (HH:MM)
   --===========================================================================
   function Day_Lapse_Image (Value : Day_Lapse) return String;
   
   --===========================================================================
   -- Returns the image of the clock (HH:MM)
   --===========================================================================
   function Day_Lapse_Image (Value : Times) return String;
   
   --===========================================================================
   -- Returns the image of the hour component
   --===========================================================================
   function Hour_Image (Value : Day_Lapse) return String;
   
   --===========================================================================
   -- Returns the image of the minute component
   --===========================================================================
   function Minute_Image (Value : Day_Lapse) return String;
   
   --===========================================================================
   -- Returns the image of the second component of the clock
   --===========================================================================
   function Second_Image (Value : Day_Lapse) return String;
  
   --===========================================================================
   -- Converts a limmited set of charactes to UTF16
   --===========================================================================
   function To_Wide_String (Value : String) return Wide_String;
   
   --===========================================================================
   -- Converts a limmited set of charactes to UTF32
   --===========================================================================
   function To_Wide_Wide_String (Value : String) return Wide_Wide_String;
   
   --===========================================================================
   -- Converts a limmited set of charactes to UTF8
   --===========================================================================
   function To_String (Value : Wide_Wide_String) return String;
   
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
   
end Utility.Strings;
--------------------------------------------------------------------------------
