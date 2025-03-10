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

--//////////////////////////////////////////////////////////////////////////////
-- This package provides an interface to update GPU resources. The buffers are
-- encapsulated and managed by this unit.
--//////////////////////////////////////////////////////////////////////////////
package Glex.Colormap is
   
   pragma Warnings (Off); 
   
   --===========================================================================
   -- Initializes the unit
   --===========================================================================
   procedure Initialize;
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The buffer (can only be created here to ensure correct size)
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Type (Nn, Ne : Positive) is tagged private;
   
   --===========================================================================
   -- Returns a buffer for building a line of the given number of nodes
   --===========================================================================
   function New_Buffer (Nodes : Positive; Elements : Positive) return Buffer_Type;
   
   --===========================================================================
   -- Preloads a node in the buffer.
   --===========================================================================
   procedure Load_Node (This : in out Buffer_Type'Class; X, Y, R, G, B : Float);
   
   --===========================================================================
   -- Preloads an element in the buffer.
   --===========================================================================
   procedure Load_Element (This : in out Buffer_Type'Class; N1, N2, N3 : Natural);
   
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
   procedure Draw (This : Resource_Type'Class);
   
private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Type (Nn, Ne : Positive) is tagged record
      
      B : GLfloat_Vector (1..Nn) := (others => 0.0);
      
      N : Natural := 0;
      
      Q : GLushort_Vector  (1..Ne) := (others => 0);
      
      E : Natural := 0;
      
   end record;
   -----------------------------------------------------------------------------
   
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Resource_Type is tagged limited record
        
      Loaded         : Boolean := False;
      
      Node_Buffer    : Web.Gl.Buffers.WebGL_Buffer;
          
      Nodes          : Natural := 0;
      
      Element_Buffer : Web.Gl.Buffers.WebGL_Buffer;
        
      Elements       : Natural := 0;
      
   end record;
   --------------------------------------------------------------------------------
   
end Glex.Colormap;
--------------------------------------------------------------------------------
