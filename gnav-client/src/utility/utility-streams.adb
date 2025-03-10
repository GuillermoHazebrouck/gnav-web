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
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Utility.Streams is

   --===========================================================================
   -- Reads on the stream at the current position
   --===========================================================================
   procedure Set_Source (This   : in out Stream_Buffer_Type;
                         Source : access Stream_Element_Array) is
   begin

      Buffer := Source;
      This.Cursor := Source'First;
      This.End_Off_Buffer := Source'Length = 0;

   end Set_Source;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Read (This : in out Stream_Buffer_Type;
                   Item : out Stream_Element_Array;
                   Last : out Stream_Element_Offset) is

      J : Stream_Element_Offset := 0;

   begin

      if This.Cursor + Item'Length - 1 in Buffer'Range then

         case This.Endianness_Type is

            when Little_Endian =>

               for I in Item'Range loop

                  Item (I) := Buffer (This.Cursor + J);

                  J := J + 1;

               end loop;

            when Big_Endian =>

               for I in reverse Item'Range loop

                  Item (I) := Buffer (This.Cursor + J);

                  J := J + 1;

               end loop;

         end case;

         This.Cursor := This.Cursor + Item'Length;

         Last := Item'Last;

         if This.Cursor > Buffer'Last then

            This.End_Off_Buffer := True;

         end if;

      else

         This.End_Off_Buffer := True;

         This.Cursor := Buffer'Last + 1;

         Item := (others => 0);

         Last := Item'Last;

         Utility.Log.Put_Message ("buffer out of range");

      end if;

   end Read;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Write (This : in out Stream_Buffer_Type;
                    Item : Stream_Element_Array) is

      J : Stream_Element_Offset := 0;

   begin

      if not (This.Cursor + Item'Length in Buffer'Range) then

         This.End_Off_Buffer := True;

         This.Cursor := Buffer'Last;

         Utility.Log.Put_Message ("end of buffer");

         return;

      end if;

      case This.Endianness_Type is

         when Little_Endian =>

            for I in Item'Range loop

               Buffer (This.Cursor + J) := Item (I);

               J := J + 1;

            end loop;

         when Big_Endian =>

            for I in reverse Item'Range loop

               Buffer (This.Cursor + J) := Item (I);

               J := J + 1;

            end loop;

      end case;

      This.Cursor := This.Cursor + Item'Length;

   end Write;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Advance_Cursor (This : in out Stream_Buffer_Type;
                             Step : Stream_Element_Offset) is
   begin

      This.Cursor := This.Cursor + Step;

      if not (This.Cursor + Step in Buffer'Range) then

         This.End_Off_Buffer := True;

         This.Cursor := Buffer'Last;

         Utility.Log.Put_Message ("end of buffer");

      end if;

   end Advance_Cursor;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Move_Cursor (This   : in out Stream_Buffer_Type;
                          Cursor : Stream_Element_Offset) is
   begin

      This.Cursor := Cursor;

      if not (This.Cursor in Buffer'Range) then

         This.End_Off_Buffer := True;

         This.Cursor := Buffer'Last;

         Utility.Log.Put_Message ("end of buffer");

      end if;

   end Move_Cursor;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   function Get_Length (This   : Stream_Buffer_Type) return Natural is
   begin

      return Buffer'Length;

   end Get_Length;
   -----------------------------------------------------------------------------


   --///////////////////////////////////////////////////////////////////////////

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Integer (This   : in out Stream_Reader;
                                Source : Stream_Element_Array) return Short_Integer is
      Size : constant Stream_Element_Offset := Short_Integer'Max_Size_In_Storage_Elements;
   begin

      if Source'Last < This.Cursor + Size then
         declare
            V : Short_Integer; for V'Address use Source (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Short_Integer;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Read_Integer (This   : in out Stream_Reader;
                          Source : Stream_Element_Array) return Integer is
      Size : constant Stream_Element_Offset := Integer'Max_Size_In_Storage_Elements;
   begin

      if Source'Last < This.Cursor + Size then
         declare
            V : Integer; for V'Address use Source (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Integer;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Read_Natural (This   : in out Stream_Reader;
                          Source : Stream_Element_Array) return Natural is
      Size : constant Stream_Element_Offset := Natural'Max_Size_In_Storage_Elements;
   begin

      if Source'Last < This.Cursor + Size then
         declare
            V : Natural; for V'Address use Source (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Natural;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Float (This   : in out Stream_Reader;
                              Source : Stream_Element_Array) return Short_Float is
      Size : constant Stream_Element_Offset := Short_Float'Max_Size_In_Storage_Elements;
   begin

      if Source'Last < This.Cursor + Size then
         declare
            V : Short_Float; for V'Address use Source (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Size;
            return V;
         end;

      else
         return 0.0;
      end if;

   end Read_Short_Float;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Read_Float (This   : in out Stream_Reader;
                        Source : Stream_Element_Array) return Float is
      Size : constant Stream_Element_Offset := Float'Max_Size_In_Storage_Elements;
   begin

      if Source'Last < This.Cursor + Size then
         declare
            V : Float; for V'Address use Source (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Size;
            return V;
         end;

      else
         return 0.0;
      end if;

   end Read_Float;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Read_Long_Float (This   : in out Stream_Reader;
                             Source : Stream_Element_Array) return Long_Float is
      Size : constant Stream_Element_Offset := Long_Float'Max_Size_In_Storage_Elements;
   begin

      if Source'Last < This.Cursor + Size then
         declare
            V : Long_Float; for V'Address use Source (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Size;
            return V;
         end;

      else
         return 0.0;
      end if;

   end Read_Long_Float;
   -----------------------------------------------------------------------------


end Utility.Streams;
--------------------------------------------------------------------------------
