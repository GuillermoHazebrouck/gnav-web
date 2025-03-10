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
with Ada.Streams;
use  Ada.Streams;
-- Standard
with Utility.Calendar;
use  Utility.Calendar;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package Utility.Streams is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The two tipes of endiannes
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Endianness_Types is (Little_Endian, Big_Endian);

   Buffer : access Stream_Element_Array := null;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Allows a sequential reading of a memory buffer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Buffer_Type is new Root_Stream_Type with record

      Cursor : Stream_Element_Offset := Stream_Element_Offset'First;

      Endianness_Type : Endianness_Types := Little_Endian;

      End_Off_Buffer  : Boolean := False;

   end record;
   -----------------------------------------------------------------------------

   type Stream_Buffer_Access is not null access Stream_Buffer_Type;

   --===========================================================================
   -- Reads on the stream at the current position
   --===========================================================================
   procedure Set_Source (This   : in out Stream_Buffer_Type;
                         Source : access Stream_Element_Array);

   --===========================================================================
   -- Reads on the stream at the current position
   --===========================================================================
   procedure Read (This : in out Stream_Buffer_Type;
                   Item : out Stream_Element_Array;
                   Last : out Stream_Element_Offset);

   --===========================================================================
   -- Loads the stream from another stream
   --===========================================================================
   procedure Write (This : in out Stream_Buffer_Type;
                    Item : Stream_Element_Array);

   --===========================================================================
   --
   --===========================================================================
   procedure Advance_Cursor (This : in out Stream_Buffer_Type;
                             Step : Stream_Element_Offset);
   -----------------------------------------------------------------------------

   --===========================================================================
   --
   --===========================================================================
   procedure Move_Cursor (This   : in out Stream_Buffer_Type;
                          Cursor : Stream_Element_Offset);

   --===========================================================================
   --
   --===========================================================================
   function Get_Length (This   : Stream_Buffer_Type) return Natural;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A sequential stream reader
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Reader is tagged record

      Cursor : Stream_Element_Offset := Stream_Element_Offset'First;

      Endianness_Type : Endianness_Types := Little_Endian;

   end record;

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Integer (This   : in out Stream_Reader;
                                Source : Stream_Element_Array) return Short_Integer;

   --===========================================================================
   --
   --===========================================================================
   function Read_Integer (This   : in out Stream_Reader;
                          Source : Stream_Element_Array) return Integer;

   --===========================================================================
   --
   --===========================================================================
   function Read_Natural (This   : in out Stream_Reader;
                          Source : Stream_Element_Array) return Natural;

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Float (This   : in out Stream_Reader;
                              Source : Stream_Element_Array) return Short_Float;

   --===========================================================================
   --
   --===========================================================================
   function Read_Float (This   : in out Stream_Reader;
                        Source : Stream_Element_Array) return Float;

   --===========================================================================
   --
   --===========================================================================
   function Read_Long_Float (This   : in out Stream_Reader;
                             Source : Stream_Element_Array) return Long_Float;

end Utility.Streams;
--------------------------------------------------------------------------------
