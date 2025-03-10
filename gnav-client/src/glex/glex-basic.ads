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
-- AdaWebPack
with Web.Gl.Buffers;
-- Glex
with Glex.Colors;
use  Glex.Colors;

pragma Warnings (Off); 
--//////////////////////////////////////////////////////////////////////////////
-- This package provides an interface to update GPU resources. The buffers are
-- encapsulated and managed by this unit.
--//////////////////////////////////////////////////////////////////////////////
package Glex.Basic is
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The basic modes for drawing the loaded nodes
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Basic_Modes is (Lines, Line_Strip, Line_Loop, Triangles, Triangle_Strip, Triangle_Fan);
   
   --===========================================================================
   -- Initializes the unit
   --===========================================================================
   procedure Initialize;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The buffer (can only be created here to ensure correct size)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Type (Size : Positive) is tagged private;
   
   --===========================================================================
   -- Returns a buffer for building a line of the given number of nodes
   --===========================================================================
   function New_Buffer (Nodes : Positive) return Buffer_Type;
   
   --===========================================================================
   -- Preloads a node in the buffer.
   --===========================================================================
   procedure Load_Node (This : in out Buffer_Type'Class; X, Y : Float);
   
   --===========================================================================
   -- Preloads a copy of the last node.
   --===========================================================================
   procedure Copy_Node (This : in out Buffer_Type'Class);
   
   --===========================================================================
   -- Preloads a copy of a previous node.
   --===========================================================================
   procedure Copy_Node (This : in out Buffer_Type'Class; I : Positive);
   
   --===========================================================================
   -- Resets the buffer.
   --===========================================================================
   procedure Reset (This : in out Buffer_Type'Class);
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A basic resource
   -- NOTE: this type is limited to protect the actual resource
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Resource_Type is tagged limited private;
   
   --===========================================================================
   -- Completes the line and loads it into a GPU resource
   --===========================================================================
   procedure Load (This : in out Resource_Type'Class; Buffer : Buffer_Type);
   
   --===========================================================================
   -- Draws the line for the given resource
   --===========================================================================
   procedure Draw (This : Resource_Type'Class; Color : Color_Record; Mode : Basic_Modes);
   
private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Type (Size : Positive) is tagged record
      
      B : GLfloat_Vector (1..Size) := (others => 0.0);
      
      N : Natural := 0;
      
   end record;
   -----------------------------------------------------------------------------
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Resource_Type is tagged limited record
      
      Buffer : Web.Gl.Buffers.WebGL_Buffer;
           
      Loaded : Boolean := False;
      
      Nodes  : Natural := 0;
      
   end record;
   --------------------------------------------------------------------------------
   
end Glex.Basic;
--------------------------------------------------------------------------------
