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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Streams;
use  Ada.Streams;
with Interfaces;
with System.Storage_Elements;

with WASM.Attributes;
with WASM.Console;
with WASM.Classes;
with WASM.Methods;
with WASM.Objects.Attributes;
with WASM.Objects.Constructors;
with WASM.Objects.Methods;

with Web.Strings;
with Web.Strings.WASM_Helpers;

--//////////////////////////////////////////////////////////////////////////////
package body Utility.Requests is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Dummy : Stream_Element_Buffer (1);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- We allocate memory blocks in full pages
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Wasm_Page_Size : constant Interfaces.Unsigned_32 := 65_536;

   --===========================================================================
   -- TODO: check if it is indeed necessary to allocate at least one page
   -- (WASM allocates pages of 64KB)
   -- NOTE: if for some reason size is out of range, try to mitigate that by
   --       returning a 1 byte [0] buffer.
   --===========================================================================
  function Allocate_Stream_Element_Buffer (Size : Interfaces.Unsigned_32) return System.Address
     with
       Export     => True,
       Convention => C,
       Link_Name  => "__adawebpack__core__allocate_stream_element_buffer";

   function Allocate_Stream_Element_Buffer (Size : Interfaces.Unsigned_32) return System.Address is
      use Interfaces;
   begin

      if Size'Valid then

         declare
            Block_Size : Unsigned_32 := ((Size / Wasm_Page_Size) + 1) * Wasm_Page_Size;
            Buffer     : Stream_Element_Buffer_Access;
         begin

            Buffer      := new Stream_Element_Buffer (Ada.Streams.Stream_Element_Offset (Block_Size-1));
            Buffer.Size := Ada.Streams.Stream_Element_Offset (Size);

            return Buffer.all.Data'Address;

         end;

      else

         declare
            Aux : constant Stream_Element_Buffer_Access := new Stream_Element_Buffer (0);
         begin

            Aux.Size     := 0;
            Aux.Data (0) := 0;

            return Aux.all.Data'Address;

         end;

      end if;

   end Allocate_Stream_Element_Buffer;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function To_Data is new Ada.Unchecked_Conversion (System.Address,
                                                     Stream_Element_Buffer_Access);




   --===========================================================================
   --
   --===========================================================================
   procedure Free is new Ada.Unchecked_Deallocation (Stream_Element_Buffer,
                                                     Stream_Element_Buffer_Access);




   --===========================================================================
   --
   --===========================================================================
   function To_Ada (Item : System.Address) return Ada.Streams.Stream_Element_Array
   is

      use type Ada.Streams.Stream_Element_Offset;
      use type System.Address;
      use type System.Storage_Elements.Storage_Offset;

      Aux : Stream_Element_Buffer_Access := null;

   begin

      if Item /= System.Null_Address then

         Aux := To_Data (Item - Dummy.Data'Position);

         return
           Result : constant Ada.Streams.Stream_Element_Array := Aux.Data (0 .. Aux.Size - 1)
         do
            Free (Aux);
         end return;

      else
         return Result : Ada.Streams.Stream_Element_Array (1 .. 0);
      end if;

   end To_Ada;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Returns the response stream
   --===========================================================================
   function Get_Response_Data (Self : XML_Http_Request'Class) return Stream_Element_Buffer_Access is

      use type Ada.Streams.Stream_Element_Offset;
      use type System.Address;
      use type System.Storage_Elements.Storage_Offset;

      function Imported (Identifier : WASM.Objects.Object_Identifier) return System.Address
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__xhr__XMLHttpRequest__response_getter_stream_element_buffer";

      Aux : System.Address := Imported (Self.Identifier);

   begin

      if Aux /= System.Null_Address then

         return To_Data (Aux - Dummy.Data'Position);

      else
         return null;

      end if;

   end Get_Response_Data;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Add_Event_Listener --
   --===========================================================================
   overriding procedure Add_Event_Listener (Self     : in out XML_Http_Request;
                                            Name     : Web.Strings.Web_String;
                                            Callback : not null Web.DOM.Event_Listeners.Event_Listener_Access;
                                            Capture  : Boolean := False) is

      procedure Imported (Identifier   : WASM.Objects.Object_Identifier;
                          Name_Address : System.Address;
                          Name_Size    : Interfaces.Unsigned_32;
                          Callback     : System.Address;
                          Capture      : Interfaces.Unsigned_32)
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__dom__Node__addEventListener";

      A : System.Address;
      S : Interfaces.Unsigned_32;

   begin

      Web.Strings.WASM_Helpers.To_JS (Name, A, S);

      Imported (Self.Identifier, A, S, Callback.all'Address, (if Capture then 1 else 0));

   end Add_Event_Listener;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   package body Constructors is

      function New_XML_Http_Request return XML_Http_Request is
      begin
         return
           Utility.Requests.Instantiate
             (WASM.Objects.Constructors.New_Object
                (WASM.Classes.XML_Http_Request));
      end New_XML_Http_Request;

   end Constructors;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Ready_State (Self : XML_Http_Request'Class) return State is

      function Imported (Identifier : WASM.Objects.Object_Identifier) return Interfaces.Unsigned_32
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__xhr__XMLHttpRequest__readyState_getter";

   begin

      return State (Imported (Self.Identifier));

   end Get_Ready_State;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Status (Self : XML_Http_Request'Class) return Web.DOM_Unsigned_Short
   is
      function Imported (Identifier : WASM.Objects.Object_Identifier)
                         return Interfaces.Unsigned_32
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__xhr__XMLHttpRequest__status_getter";

   begin

      return Web.DOM_Unsigned_Short (Imported (Self.Identifier));

   end Get_Status;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Set_Response_To_Array_Buffer (Self : in out XML_Http_Request'Class) is

      procedure Imported (Identifier : WASM.Objects.Object_Identifier;
                          Address    : System.Address;
                          Size       : Interfaces.Unsigned_32)
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__xhr__XMLHttpRequest__responseType_setter";

      A : System.Address;
      S : Interfaces.Unsigned_32;

   begin

      Web.Strings.WASM_Helpers.To_JS (Web.Strings.To_Web_String ("arraybuffer"), A, S);

      Imported (Self.Identifier, A, S);

   end Set_Response_To_Array_Buffer;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Open (Self     : in out XML_Http_Request'Class;
                   Method   : Web.Strings.Web_String;
                   URL      : Web.Strings.Web_String;
                   Async    : Boolean := True;
                   Username : Web.Strings.Web_String := Web.Strings.Empty_Web_String;
                   Password : Web.Strings.Web_String := Web.Strings.Empty_Web_String) is

      procedure Imported (Identifier       : WASM.Objects.Object_Identifier;
                          Method_Address   : System.Address;
                          Method_Size      : Interfaces.Unsigned_32;
                          URL_Address      : System.Address;
                          URL_Size         : Interfaces.Unsigned_32;
                          Async            : Interfaces.Unsigned_32;
                          Username_Address : System.Address;
                          Username_Size    : Interfaces.Unsigned_32;
                          Password_Address : System.Address;
                          Password_Size    : Interfaces.Unsigned_32)
          with Import     => True,
               Convention => C,
               Link_Name  => "__adawebpack__xhr__XMLHttpRequest__open";

      Method_Address   : System.Address;
      Method_Size      : Interfaces.Unsigned_32;
      URL_Address      : System.Address;
      URL_Size         : Interfaces.Unsigned_32;
      Username_Address : System.Address;
      Username_Size    : Interfaces.Unsigned_32;
      Password_Address : System.Address;
      Password_Size    : Interfaces.Unsigned_32;

   begin
      Web.Strings.WASM_Helpers.To_JS (Method, Method_Address, Method_Size);
      Web.Strings.WASM_Helpers.To_JS (URL, URL_Address, URL_Size);
      Web.Strings.WASM_Helpers.To_JS (Username, Username_Address, Username_Size);
      Web.Strings.WASM_Helpers.To_JS (Password, Password_Address, Password_Size);

      Imported (Self.Identifier,
                Method_Address,
                Method_Size,
                URL_Address,
                URL_Size,
                (if Async then 1 else 0),
                Username_Address,
                Username_Size,
                Password_Address,
                Password_Size);

      Self.Set_Response_To_Array_Buffer;

   end Open;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Send (Self : XML_Http_Request'Class;
                   Data : Ada.Streams.Stream_Element_Array) is

      procedure Imported (Identifier   : WASM.Objects.Object_Identifier;
                          Data_Address : System.Address;
                          Data_Size    : Interfaces.Unsigned_32)
        with Import => True,
             Convention => C,
             Link_Name => "__adawebpack__xhr__XMLHttpRequest__send";

   begin

      Imported (Self.Identifier, Data'Address, Data'Length);

   end Send;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Send (Self : XML_Http_Request'Class; Data : Web.Strings.Web_String) is
   begin

      WASM.Objects.Methods.Call_Void_String (Self, WASM.Methods.Send, Data);

   end Send;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   procedure Set_Request_Header (Self   : XML_Http_Request'Class;
                                 Header : Web.Strings.Web_String;
                                 Value  : Web.Strings.Web_String) is

      procedure Imported (Identifier     : WASM.Objects.Object_Identifier;
                          Header_Address : System.Address;
                          Header_Size    : Interfaces.Unsigned_32;
                          Value_Address  : System.Address;
                          Value_Size     : Interfaces.Unsigned_32)
        with
          Import     => True,
          Convention => C,
          Link_Name  => "__adawebpack__xhr__XMLHttpRequest__setRequestHeader";

      Header_Address : System.Address;
      Header_Size    : Interfaces.Unsigned_32;
      Value_Address  : System.Address;
      Value_Size     : Interfaces.Unsigned_32;

   begin

      Web.Strings.WASM_Helpers.To_JS (Header, Header_Address, Header_Size);
      Web.Strings.WASM_Helpers.To_JS (Value, Value_Address, Value_Size);

      Imported (Self.Identifier,
                Header_Address,
                Header_Size,
                Value_Address,
                Value_Size);

   end Set_Request_Header;
   -----------------------------------------------------------------------------




   --===========================================================================
   --  Can be set to change the timeout
   --===========================================================================
   procedure Set_Timeout (Self : in out XML_Http_Request'Class; Value : Natural) is

      procedure Imported (Identifier : WASM.Objects.Object_Identifier;
                          Value      : Interfaces.Unsigned_32)
        with
          Import => True,
          Convention => C,
          Link_Name  => "__adawebpack__xhr__XMLHttpRequest__timeout_setter";

   begin

      if Value > 0 then

         Imported (Self.Identifier, Interfaces.Unsigned_32 (Value));

      end if;

   end Set_Timeout;
   -----------------------------------------------------------------------------





   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Get_Response (Self : XML_Http_Request'Class; Reader : in out Stream_Reader_Type) is
   begin

      Reader.Buffer := Self.Get_Response_Data;

      if Reader.Buffer /= null then

         Reader.Cursor := Reader.Buffer.Data'First;

      end if;

   end Get_Response;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Is_Empty (This : Stream_Reader_Type) return Boolean is
   begin

      return This.Buffer = null or else This.Get_Size = 0;

   end Is_Empty;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   function Get_Size (This : Stream_Reader_Type) return Natural is
      Last_Natural : constant Stream_Element_Offset := Stream_Element_Offset (Natural'Last);
   begin

      if This.Buffer /= null and then This.Buffer.Size in 1..Last_Natural then
         return Natural (This.Buffer.Size);
      else
         return 0;
      end if;

   end Get_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Get_Remaining_Size (This : Stream_Reader_Type) return Natural is
      Last_Natural : constant Stream_Element_Offset := Stream_Element_Offset (Natural'Last);
   begin

      if This.Buffer /= null and then This.Buffer.Size >= This.Cursor then
         return Natural (This.Buffer.Size - This.Cursor);
      else
         return 0;
      end if;

   end Get_Remaining_Size;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Initialization
   --===========================================================================
   overriding procedure Initialize (This : in out Stream_Reader_Type) is
   begin

      This.Buffer := null;
      This.Cursor := 0;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Finalization
   --===========================================================================
   overriding procedure Finalize (This : in out Stream_Reader_Type) is
   begin

      if This.Buffer /= null then

         Free (This.Buffer);

         This.Buffer := null;

      end if;

   end Finalize;
   -----------------------------------------------------------------------------




   Short_Short_Integer_Size : constant Stream_Element_Offset := Short_Short_Integer'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Short_Integer (This : in out Stream_Reader_Type) return Short_Short_Integer is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Short_Short_Integer_Size - 1 then
         declare
            V : Short_Short_Integer; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Short_Short_Integer_Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Short_Short_Integer;
   -----------------------------------------------------------------------------




   Short_Integer_Size : constant Stream_Element_Offset := Short_Integer'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Integer (This : in out Stream_Reader_Type) return Short_Integer is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Short_Integer_Size - 1 then
         declare
            V : Short_Integer; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Short_Integer_Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Short_Integer;
   -----------------------------------------------------------------------------



   Integer_Size : constant Stream_Element_Offset := Integer'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Integer (This   : in out Stream_Reader_Type) return Integer is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Integer_Size - 1 then
         declare
            V : Integer; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Integer_Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Integer;
   -----------------------------------------------------------------------------



   Short_Natural_Size : constant Stream_Element_Offset := Short_Natural'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Natural (This : in out Stream_Reader_Type) return Short_Natural is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Short_Natural_Size - 1 then
         declare
            V : Short_Natural; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Short_Natural_Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Short_Natural;
   -----------------------------------------------------------------------------



   Short_Short_Natural_Size : constant Stream_Element_Offset := Short_Short_Natural'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Short_Natural (This : in out Stream_Reader_Type) return Short_Short_Natural is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Short_Short_Natural_Size - 1 then
         declare
            V : Short_Short_Natural; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Short_Short_Natural_Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Short_Short_Natural;
   -----------------------------------------------------------------------------



   Natural_Size : constant Stream_Element_Offset := Natural'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Natural (This : in out Stream_Reader_Type) return Natural is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Natural_Size - 1 then
         declare
            V : Natural; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Natural_Size;
            return V;
         end;

      else
         return 0;
      end if;

   end Read_Natural;
   -----------------------------------------------------------------------------



   Short_Float_Size : constant Stream_Element_Offset := Short_Float'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Float (This   : in out Stream_Reader_Type) return Short_Float is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Short_Float_Size - 1 then
         declare
            V : Short_Float; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Short_Float_Size;
            return V;
         end;

      else
         return 0.0;
      end if;

   end Read_Short_Float;
   -----------------------------------------------------------------------------



   Float_Size : constant Stream_Element_Offset := Float'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Float (This : in out Stream_Reader_Type) return Float is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Float_Size - 1 then
         declare
            V : Float; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Float_Size;
            return V;
         end;

      else
         return 0.0;
      end if;

   end Read_Float;
   -----------------------------------------------------------------------------



   Long_Float_Size : constant Stream_Element_Offset := Long_Float'Max_Size_In_Storage_Elements;
   --===========================================================================
   --
   --===========================================================================
   function Read_Long_Float (This : in out Stream_Reader_Type) return Long_Float is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Long_Float_Size - 1 then
         declare
            V : Long_Float; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Long_Float_Size;
            return V;
         end;

      else
         return 0.0;
      end if;

   end Read_Long_Float;
   -----------------------------------------------------------------------------





   --===========================================================================
   --
   --===========================================================================
   function Read_Character (This : in out Stream_Reader_Type) return Character is
   begin

      if This.Buffer.Data'Last >= This.Cursor then
         declare
            V : Character; for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + 1;
            return V;
         end;

      else
         return ' ';
      end if;

   end Read_Character;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   function Read_String (This : in out Stream_Reader_Type; Size : Positive) return String is
   begin

      if This.Buffer.Data'Last >= This.Cursor + Stream_Element_Offset (Size) - 1 then
         declare
            V : String (1..Size); for V'Address use This.Buffer.Data (This.Cursor)'Address;
         begin
            This.Cursor := This.Cursor + Stream_Element_Offset (Size);
            return V;
         end;

      else
         declare
            No_Data : constant String (1..Size) := (others => ' ');
         begin
            return No_Data;
         end;
      end if;

   end Read_String;
   -----------------------------------------------------------------------------


end Utility.Requests;
--------------------------------------------------------------------------------
