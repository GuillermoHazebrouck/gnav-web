------------------------------------------------------------------------------
--                                                                          --
--                            Matreshka Project                             --
--                                                                          --
--                               Web Framework                              --
--                                                                          --
--                            Web API Definition                            --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2020-2022, Vadim Godunko <vgodunko@gmail.com>                --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Streams;
use  Ada.Streams;
with Interfaces;
with System.Storage_Elements;

with WASM.Attributes;
with WASM.Classes;
with WASM.Methods;
with WASM.Objects.Attributes;
with WASM.Objects.Constructors;
with WASM.Objects.Methods;

with Web.Strings.WASM_Helpers;

package body Utility.Requests is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Dummy : Stream_Element_Buffer (1);

   --===========================================================================
   -- TODO: check if it is indeed necessary to allocate at least one page
   -- (WASM allocates pages of 64KB)
   --===========================================================================
  function Allocate_Stream_Element_Buffer (Size : Interfaces.Unsigned_32) return System.Address
     with
       Export     => True,
       Convention => C,
       Link_Name  => "__adawebpack__core__allocate_stream_element_buffer";

   function Allocate_Stream_Element_Buffer (Size : Interfaces.Unsigned_32) return System.Address is
      use Interfaces;
      Wasm_Page_Size : constant Unsigned_32 := 64_536;
      Block_Size : Unsigned_32 := ((Size / Wasm_Page_Size) + 1) * Wasm_Page_Size;
      Aux : constant Stream_Element_Buffer_Access := new Stream_Element_Buffer (Ada.Streams.Stream_Element_Offset (Block_Size));
   begin
      Aux.Size := Ada.Streams.Stream_Element_Offset (Size);

      return Aux.all.Data'Address;
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

         return Result : constant Ada.Streams.Stream_Element_Array := Aux.Data (0 .. Aux.Size - 1)
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
   function Get_Response (Self : XML_Http_Request'Class) return Stream_Element_Buffer_Access is

      use type Ada.Streams.Stream_Element_Offset;
      use type System.Address;
      use type System.Storage_Elements.Storage_Offset;

      function Imported
       (Identifier : WASM.Objects.Object_Identifier)
          return System.Address
            with Import     => True,
                 Convention => C,
                 Link_Name  =>
                   "__adawebpack__xhr__XMLHttpRequest__response_getter_stream_element_buffer";

      Aux : System.Address := Imported (Self.Identifier);

   begin

      if Aux /= System.Null_Address then

         return To_Data (Aux - Dummy.Data'Position);

      else
         return null;

      end if;

   end Get_Response;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Frees the response stream
   --===========================================================================
   procedure Free_Response (Data : in out Stream_Element_Buffer_Access) is
   begin

      Free (Data);

      Data := null;

   end Free_Response;
   -----------------------------------------------------------------------------



   --===========================================================================
   --
   --===========================================================================
   function "+" (Item : Wide_Wide_String) return Web.Strings.Web_String
     renames Web.Strings.To_Web_String;

   --===========================================================================
   -- Add_Event_Listener --
   --===========================================================================
   overriding procedure Add_Event_Listener
    (Self     : in out XML_Http_Request;
     Name     : Web.Strings.Web_String;
     Callback : not null Web.DOM.Event_Listeners.Event_Listener_Access;
     Capture  : Boolean := False)
   is
      procedure Imported
       (Identifier   : WASM.Objects.Object_Identifier;
        Name_Address : System.Address;
        Name_Size    : Interfaces.Unsigned_32;
        Callback     : System.Address;
        Capture      : Interfaces.Unsigned_32)
          with Import     => True,
               Convention => C,
               Link_Name  => "__adawebpack__dom__Node__addEventListener";

      A : System.Address;
      S : Interfaces.Unsigned_32;

   begin
      Web.Strings.WASM_Helpers.To_JS (Name, A, S);
      Imported
       (Self.Identifier, A, S, Callback.all'Address,
        (if Capture then 1 else 0));
   end Add_Event_Listener;
   -----------------------------------------------------------------------------

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      --------------------------
      -- New_XML_Http_Request --
      --------------------------

      function New_XML_Http_Request return XML_Http_Request is
      begin
         return
           Utility.Requests.Instantiate
            (WASM.Objects.Constructors.New_Object
              (WASM.Classes.XML_Http_Request));
      end New_XML_Http_Request;

   end Constructors;

   ---------------------
   -- Get_Ready_State --
   ---------------------

   function Get_Ready_State (Self : XML_Http_Request'Class) return State is
      function Imported
       (Identifier : WASM.Objects.Object_Identifier)
          return Interfaces.Unsigned_32
            with Import     => True,
                 Convention => C,
                 Link_Name  =>
                   "__adawebpack__xhr__XMLHttpRequest__readyState_getter";

   begin
      return State (Imported (Self.Identifier));
   end Get_Ready_State;

   ------------------
   -- Get_Response --
   ------------------

   --function Get_Response (Self : XML_Http_Request'Class) return Ada.Streams.Stream_Element_Array
   --is
   --   function Imported
   --    (Identifier : WASM.Objects.Object_Identifier)
   --       return System.Address
   --         with Import     => True,
   --              Convention => C,
   --              Link_Name  =>
   --                "__adawebpack__xhr__XMLHttpRequest__response_getter_stream_element_buffer";
   --   Aux : System.Address := Imported (Self.Identifier);
   --begin
   --   return To_Ada (Aux);
   --end Get_Response;

   -----------------------
   -- Get_Response_Text --
   -----------------------

   function Get_Response_Text (Self : XML_Http_Request'Class) return Web.Strings.Web_String is
   begin
      return
        WASM.Objects.Attributes.Get_String
          (Self, WASM.Attributes.Response_Text);
   end Get_Response_Text;

   -----------------------
   -- Get_Response_Type --
   -----------------------

   function Get_Response_Type
    (Self : XML_Http_Request'Class) return Response_Type_Kind
   is
      use type Web.Strings.Web_String;

      function Imported
       (Identifier : WASM.Objects.Object_Identifier)
          return System.Address
            with Import     => True,
                 Convention => C,
                 Link_Name  =>
                   "__adawebpack__xhr__XMLHttpRequest__responseType_getter";

      Aux : constant Web.Strings.Web_String
        := Web.Strings.WASM_Helpers.To_Ada (Imported (Self.Identifier));

   begin
      if Aux = +"" then
         return Default;

      elsif Aux = +"arraybuffer" then
         return Array_Buffer;

      elsif Aux = +"blob" then
         return Blob;

      elsif Aux = +"document" then
         return Document;

      elsif Aux = +"json" then
         return JSON;

      elsif Aux = +"text" then
         return Text;

      else
         raise Program_Error;
      end if;
   end Get_Response_Type;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
    (Self : XML_Http_Request'Class) return Web.DOM_Unsigned_Short
   is
      function Imported
       (Identifier : WASM.Objects.Object_Identifier)
          return Interfaces.Unsigned_32
            with Import     => True,
                 Convention => C,
                 Link_Name  =>
                   "__adawebpack__xhr__XMLHttpRequest__status_getter";

   begin
      return Web.DOM_Unsigned_Short (Imported (Self.Identifier));
   end Get_Status;

   ----------
   -- Open --
   ----------

   procedure Open
     (Self     : XML_Http_Request'Class;
      Method   : Web.Strings.Web_String;
      URL      : Web.Strings.Web_String;
      Async    : Boolean := True;
      Username : Web.Strings.Web_String := Web.Strings.Empty_Web_String;
      Password : Web.Strings.Web_String := Web.Strings.Empty_Web_String)
   is
      procedure Imported
       (Identifier       : WASM.Objects.Object_Identifier;
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
      Web.Strings.WASM_Helpers.To_JS
       (Username, Username_Address, Username_Size);
      Web.Strings.WASM_Helpers.To_JS
       (Password, Password_Address, Password_Size);

      Imported
       (Self.Identifier,
        Method_Address,
        Method_Size,
        URL_Address,
        URL_Size,
        (if Async then 1 else 0),
        Username_Address,
        Username_Size,
        Password_Address,
        Password_Size);
   end Open;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self : XML_Http_Request'Class;
      Data : Ada.Streams.Stream_Element_Array)
   is
      procedure Imported
       (Identifier   : WASM.Objects.Object_Identifier;
        Data_Address : System.Address;
        Data_Size    : Interfaces.Unsigned_32)
          with Import => True,
               Convention => C,
               Link_Name => "__adawebpack__xhr__XMLHttpRequest__send";

   begin
      Imported (Self.Identifier, Data'Address, Data'Length);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
    (Self : XML_Http_Request'Class;
     Data : Web.Strings.Web_String) is
   begin
      WASM.Objects.Methods.Call_Void_String (Self, WASM.Methods.Send, Data);
   end Send;

   ------------------------
   -- Set_Request_Header --
   ------------------------

   procedure Set_Request_Header
    (Self   : XML_Http_Request'Class;
     Header : Web.Strings.Web_String;
     Value  : Web.Strings.Web_String)
   is
      procedure Imported
       (Identifier     : WASM.Objects.Object_Identifier;
        Header_Address : System.Address;
        Header_Size    : Interfaces.Unsigned_32;
        Value_Address  : System.Address;
        Value_Size     : Interfaces.Unsigned_32)
          with Import     => True,
               Convention => C,
               Link_Name  =>
                 "__adawebpack__xhr__XMLHttpRequest__setRequestHeader";

      Header_Address : System.Address;
      Header_Size    : Interfaces.Unsigned_32;
      Value_Address  : System.Address;
      Value_Size     : Interfaces.Unsigned_32;

   begin
      Web.Strings.WASM_Helpers.To_JS (Header, Header_Address, Header_Size);
      Web.Strings.WASM_Helpers.To_JS (Value, Value_Address, Value_Size);
      Imported
       (Self.Identifier,
        Header_Address,
        Header_Size,
        Value_Address,
        Value_Size);
   end Set_Request_Header;

   -----------------------
   -- Set_Response_Type --
   -----------------------

   procedure Set_Response_Type
    (Self : in out XML_Http_Request'Class;
     To   : Response_Type_Kind)
   is
      procedure Imported
       (Identifier : WASM.Objects.Object_Identifier;
        Address    : System.Address;
        Size       : Interfaces.Unsigned_32)
          with Import => True,
               Convention => C,
               Link_Name  =>
                 "__adawebpack__xhr__XMLHttpRequest__responseType_setter";

      Value : constant Web.Strings.Web_String
        := (case To is
            when Default => +"",
            when Array_Buffer => +"arraybuffer",
            when Blob => +"blob",
            when Document => +"document",
            when JSON => +"json",
            when Text => +"text");
      A     : System.Address;
      S     : Interfaces.Unsigned_32;

   begin
      Web.Strings.WASM_Helpers.To_JS (Value, A, S);
      Imported (Self.Identifier, A, S);
   end Set_Response_Type;
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
   function Get_Response (Self : XML_Http_Request'Class) return Stream_Reader_Type is

      Reader : Stream_Reader_Type;

   begin

      Reader.Buffer := Self.Get_Response;

      if Reader.Buffer /= null then

         Reader.Cursor := Reader.Buffer.Data'First;

      end if;

      return Reader;

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
   -- Initialization.
   --===========================================================================
   overriding procedure Initialize (This : in out Stream_Reader_Type) is
   begin

      This.Buffer := null;
      This.Cursor := 0;

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Adjustment.
   --===========================================================================
   overriding procedure Adjust (This : in out Stream_Reader_Type) is
   begin

      null;

   end Adjust;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Finalization.
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
