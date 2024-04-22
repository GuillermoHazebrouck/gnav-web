--------------------------------------------------------------------------------
-- LIBRARY_UNIT_NAME : Glex
--
-- DESIGNER          : Guillermo HAZEBROUCK
--
-- CREATION_DATE     : 17 Nov 2023
--
--------------------------------------------------------------------------------
-- AdaWebPack
with Web.Gl.Buffers;
-- Glex
with Glex.Colors;
use  Glex.Colors;

--******************************************************************************
-- This module of Glex provides variable-thickness smooth lines with different
-- dash patterns.
-- USAGE:
-- declare
--    Resource : Resource_Type;
--    Lines    : Buffer_Type (2);
-- begin
--    Lines.Load (1, 0.0, 0.0);
--    Lines.Load (2, 1.0, 1.0);
--    Resource.Load (Lines);
--    Resource.Draw (Line_List, Line_Dashed);
--    Resource.Remove;
-- end;
--******************************************************************************
package Glex.Lines is

   pragma Warnings (Off);

   --===========================================================================
   -- Initializes the package for the active context
   --===========================================================================
   procedure Initialize;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A buffer containing the a geometric construction of primitives
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Type (Size : Positive) is tagged private;

   --===========================================================================
   -- Returns a buffer for the given number of segments in the line. The line
   -- might be continuous or broken (see Move_To/Line_To).
   -- TODO: split this in New_Strip_Buffer and New_Lines_Buffer.
   --===========================================================================
   function New_Buffer (Segments : Positive) return Buffer_Type;

   --===========================================================================
   -- Starts a new line
   --===========================================================================
   procedure Move_To (This : in out Buffer_Type'Class; X, Y : Float);

   --===========================================================================
   -- Extends the current line. For the first not, this is the same as Move_To.
   --===========================================================================
   procedure Line_To (This : in out Buffer_Type'Class; X, Y : Float);

   --===========================================================================
   -- Finishes the last line with a cap.
   --===========================================================================
   procedure Finish (This : in out Buffer_Type'Class);

   --===========================================================================
   -- Closes the last line.
   --===========================================================================
   procedure Close (This : in out Buffer_Type'Class);

   --===========================================================================
   -- Restarts the buffer
   --===========================================================================
   procedure Reset (This : in out Buffer_Type'Class);

   --===========================================================================
   -- Indicates how many segments have been loaded
   --===========================================================================
   function Line_Count (This : in out Buffer_Type'Class) return Natural;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The resouce for the associated buffer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Resource_Type is tagged limited private;

   --===========================================================================
   -- Clears the resource
   --===========================================================================
   procedure Clear (This : in out Resource_Type'Class);

   --===========================================================================
   -- Indicates if the resource is loaded in the active context
   --===========================================================================
   function Loaded (This : Resource_Type'Class) return Boolean;

   --===========================================================================
   -- Loads the provided buffer to the resource
   --===========================================================================
   procedure Load (This : in out Resource_Type'Class; Buffer : Buffer_Type);

   --===========================================================================
   -- Draws the line
   --===========================================================================
   procedure Draw (This   : Resource_Type'Class;
                   Color  : Color_Record;
                   Width  : Float;
                   Aspect : Float   := 1.0;
                   Closed : Boolean := False);

   --===========================================================================
   -- Draws a doubled line
   --===========================================================================
   procedure Draw (This       : Resource_Type'Class;
                   Color      : Line_Color_Record;
                   Width_Glow : Float;
                   Width_Fore : Float;
                   Aspect     : Float   := 1.0;
                   Closed     : Boolean := False);

   --===========================================================================
   -- Marks the resource for removal
   --===========================================================================
   procedure Remove (This : in out Resource_Type'Class);

   --===========================================================================
   -- Draws the line
   --===========================================================================
   procedure Draw_Example;

private

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Internal representation of the data in the buffer
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Vertex_Record is record

      P : Vector_Record;

      N : Vector_Record;

      A : GLfloat;

   end record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Internal buffer structure
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Buffer_Type (Size : Positive) is tagged record

      -- Current node count
      --------------------------------
      N : Natural := 0;

      -- Current line count
      --------------------------------
      L : Natural := 0;

      -- Maximum number of lines
      --------------------------------
      M : Natural := 0;

      -- The temporal buffer
      --------------------------------
      B : aliased GLfloat_Vector (1..Size) := (others => 0.0);

      -- Create start cap
      --------------------------------
      Start : Boolean := False;

      -- Create end cap
      --------------------------------
      Close : Boolean := False;

   end record;
   -----------------------------------------------------------------------------




   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Resource data for a particular context
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Resource_Type is tagged limited record

      -- The buffer
      ----------------------------------------------
      Buffer : Web.Gl.Buffers.WebGL_Buffer;

      -- Indicates if the buffer has been requested
      ----------------------------------------------
      Loaded : Boolean := False;

      -- The number of nodes representing the lines
      ----------------------------------------------
      Nodes  : Natural := 0;

   end record;
   -----------------------------------------------------------------------------



end Glex.Lines;
--------------------------------------------------------------------------------
