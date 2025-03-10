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
-- Copyright © 2016-2022, Vadim Godunko <vgodunko@gmail.com>                --
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
with Ada.Finalization;
with Ada.Streams;
--
with WASM.Objects;
with Web.DOM.Event_Listeners;
with Web.DOM.Event_Targets;
with Web.Strings;

--//////////////////////////////////////////////////////////////////////////////
-- This is a new wrapper on XHR requests that provides methods for direct
-- access on the data.
-- All modifications made for the G-NAV project made by Guillermo Hazebrouck.
--//////////////////////////////////////////////////////////////////////////////
package Utility.Requests is

   pragma Preelaborate;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   subtype State is Web.DOM_Unsigned_Short range 0 .. 4;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- State values
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   UNSENT           : constant State := 0;
   OPENED           : constant State := 1;
   HEADERS_RECEIVED : constant State := 2;
   LOADING          : constant State := 3;
   DONE             : constant State := 4;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Response_Type_Kind is (Default,
                               Array_Buffer,
                               Blob,
                               Document,
                               JSON,
                               Text);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- An XHR request
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type XML_Http_Request is new WASM.Objects.Object_Reference and Web.DOM.Event_Targets.Event_Target with null record;

   --===========================================================================
   -- The XMLHttpRequest object can be in several states. The readyState
   -- attribute must return the current state, which must be one of the
   -- following values:
   --===========================================================================
   function Get_Ready_State (Self : XML_Http_Request'Class) return State;

   --===========================================================================
   -- Sets the request method, request URL, and synchronous flag.
   --===========================================================================
   procedure Open (Self     : XML_Http_Request'Class;
                   Method   : Web.Strings.Web_String;
                   URL      : Web.Strings.Web_String;
                   Async    : Boolean := True;
                   Username : Web.Strings.Web_String := Web.Strings.Empty_Web_String;
                   Password : Web.Strings.Web_String := Web.Strings.Empty_Web_String);

   --===========================================================================
   --  Appends an header to the list of author request headers, or if header is
   --  already in the list of author request headers, combines its value with
   --  value.
   --===========================================================================
   procedure Set_Request_Header (Self   : XML_Http_Request'Class;
                                 Header : Web.Strings.Web_String;
                                 Value  : Web.Strings.Web_String);

   --===========================================================================
   --
   --===========================================================================
   procedure Send (Self : XML_Http_Request'Class; Data : Ada.Streams.Stream_Element_Array);

   --===========================================================================
   --
   --===========================================================================
   procedure Send (Self : XML_Http_Request'Class; Data : Web.Strings.Web_String);

   --===========================================================================
   -- Returns the HTTP status code.
   --===========================================================================
   function Get_Status (Self : XML_Http_Request'Class) return Web.DOM_Unsigned_Short;

   --===========================================================================
   -- Returns the response type.
   --===========================================================================
   function Get_Response_Type (Self : XML_Http_Request'Class) return Response_Type_Kind;

   --===========================================================================
   --  Can be set to change the response type. Values are: the empty string
   --  (default), "arraybuffer", "blob", "document", "json", and "text".
   --===========================================================================
   procedure Set_Response_Type (Self : in out XML_Http_Request'Class; To : Response_Type_Kind);

   --===========================================================================
   --  Can be set to change the timeout
   --===========================================================================
   procedure Set_Timeout (Self : in out XML_Http_Request'Class; Value : Natural);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Reader_Type is new Ada.Finalization.Controlled with private;

   --===========================================================================
   -- Indicates if the stream is empty
   --===========================================================================
   function Is_Empty (This : Stream_Reader_Type) return Boolean;

   --===========================================================================
   -- Returns the total size of the stream
   --===========================================================================
   function Get_Size (This : Stream_Reader_Type) return Natural;

   --===========================================================================
   -- Returns the number of bytes that can be read
   --===========================================================================
   function Get_Remaining_Size (This : Stream_Reader_Type) return Natural;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 2 bytes natural
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Short_Natural is range 0..2 ** 16 - 1;
   for Short_Natural'Size use Short_Integer'Size;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- 1 byte natural
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Short_Short_Natural is range 0..2 ** 8 - 1;
   for Short_Short_Natural'Size use Short_Short_Integer'Size;

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Short_Integer (This : in out Stream_Reader_Type) return Short_Short_Integer;

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Integer (This : in out Stream_Reader_Type) return Short_Integer;

   --===========================================================================
   --
   --===========================================================================
   function Read_Integer (This : in out Stream_Reader_Type) return Integer;

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Short_Natural (This : in out Stream_Reader_Type) return Short_Short_Natural;

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Natural (This : in out Stream_Reader_Type) return Short_Natural;

   --===========================================================================
   --
   --===========================================================================
   function Read_Natural (This : in out Stream_Reader_Type) return Natural;

   --===========================================================================
   --
   --===========================================================================
   function Read_Short_Float (This : in out Stream_Reader_Type) return Short_Float;

   --===========================================================================
   --
   --===========================================================================
   function Read_Float (This : in out Stream_Reader_Type) return Float;

   --===========================================================================
   --
   --===========================================================================
   function Read_Long_Float (This : in out Stream_Reader_Type) return Long_Float;

   --===========================================================================
   --
   --===========================================================================
   function Read_Character (This : in out Stream_Reader_Type) return Character;

   --===========================================================================
   --
   --===========================================================================
   function Read_String (This : in out Stream_Reader_Type; Size : Positive) return String;

   --===========================================================================
   -- Initialization.
   --===========================================================================
   overriding procedure Initialize (This : in out Stream_Reader_Type);

   --===========================================================================
   -- Adjustment.
   --===========================================================================
   overriding procedure Adjust (This : in out Stream_Reader_Type);

   --===========================================================================
   -- Finalization.
   --===========================================================================
   overriding procedure Finalize (This : in out Stream_Reader_Type);

   --===========================================================================
   -- Returns the response stream
   --===========================================================================
   function Get_Response (Self : XML_Http_Request'Class) return Stream_Reader_Type;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Stream_Reader : constant Stream_Reader_Type;

   --===========================================================================
   -- Returns the text response entity body
   --===========================================================================
   function Get_Response_Text (Self : XML_Http_Request'Class) return Web.Strings.Web_String;

   --***************************************************************************
   --
   --***************************************************************************
   package Constructors is

      function New_XML_Http_Request return XML_Http_Request;

   end Constructors;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A streamed element that can be read directly
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Element_Buffer (Capacity : Ada.Streams.Stream_Element_Offset) is tagged record

      Size : Ada.Streams.Stream_Element_Offset;

      Data : Ada.Streams.Stream_Element_Array (0 .. Capacity);

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Element_Buffer_Access is access all Stream_Element_Buffer;

   --===========================================================================
   -- Returns the response stream
   --===========================================================================
   function Get_Response (Self : XML_Http_Request'Class) return Stream_Element_Buffer_Access;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Reader_Type is new Ada.Finalization.Controlled with record

      Buffer : Stream_Element_Buffer_Access;

      Cursor : Ada.Streams.Stream_Element_Offset := 0;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   No_Stream_Reader : constant Stream_Reader_Type := (Ada.Finalization.Controlled with
                                                      Buffer => null,
                                                      Cursor => 0);

   --===========================================================================
   --
   --===========================================================================
   overriding procedure Add_Event_Listener (Self     : in out XML_Http_Request;
                                            Name     : Web.Strings.Web_String;
                                            Callback : not null Web.DOM.Event_Listeners.Event_Listener_Access;
                                            Capture  : Boolean := False);

end Utility.Requests;
