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
with Ada.Finalization;
with Ada.Streams;
--
with WASM.Objects;
with Web.DOM.Event_Listeners;
with Web.DOM.Event_Targets;
with Web.Strings;

--//////////////////////////////////////////////////////////////////////////////
-- This is a new wrapper on XHR requests that provides methods for direct
-- access on the data. The original code comes from AdaWebPack.
-- All modifications made for the G-NAV project made by Guillermo Hazebrouck.
-- This package only handles array buffer request types.
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
   procedure Open (Self     : in out XML_Http_Request'Class;
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
   --  Can be set to change the timeout
   --===========================================================================
   procedure Set_Timeout (Self : in out XML_Http_Request'Class; Value : Natural);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Reader_Type is new Ada.Finalization.Limited_Controlled with private;

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
   -- Returns the response stream
   --===========================================================================
   procedure Get_Response (Self : XML_Http_Request'Class; Reader : in out Stream_Reader_Type);

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
   function Get_Response_Data (Self : XML_Http_Request'Class) return Stream_Element_Buffer_Access;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Stream_Reader_Type is new Ada.Finalization.Limited_Controlled with record

      Buffer : Stream_Element_Buffer_Access;

      Cursor : Ada.Streams.Stream_Element_Offset := 0;

   end record;

   --===========================================================================
   -- Initialization.
   --===========================================================================
   overriding procedure Initialize (This : in out Stream_Reader_Type);

   --===========================================================================
   -- Finalization.
   --===========================================================================
   overriding procedure Finalize (This : in out Stream_Reader_Type);

   --===========================================================================
   --
   --===========================================================================
   overriding procedure Add_Event_Listener (Self     : in out XML_Http_Request;
                                            Name     : Web.Strings.Web_String;
                                            Callback : not null Web.DOM.Event_Listeners.Event_Listener_Access;
                                            Capture  : Boolean := False);

end Utility.Requests;
