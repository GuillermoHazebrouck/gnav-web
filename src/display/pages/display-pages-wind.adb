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
with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
-- Gnav
with Flight.Aircraft;
with Flight.Plan;
with Flight.Wind;
use  Flight.Wind;
with Glex.Colors;
use  Glex.Colors;
with Glex.Basic;
with Glex.Fonts;
with Glex.Lines;
with Math;
with Math.Vector2;
with Utility.Calendar;
with Utility.Log;
with Utility.Strings;
with Utility.Units;
use  Utility.Units;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Wind is

   -- Speed adaptation
   ---------------------------
   
   Btn_Plus   : Button_Record;
   
   Btn_Less   : Button_Record;
   
   Btn_Manual : Button_Record;
   
   Btn_Auto   : Button_Record;
   
   -- Arrows
   ---------------------------

   North_Arrow : Glex.Basic.Resource_Type;

   Wind_Arrow  : Glex.Basic.Resource_Type;

   -- Quadrant
   ---------------------------

   Quadrant    : Glex.Lines.Resource_Type;

   Rose        : Glex.Lines.Resource_Type;

   Allocation  : Allocation_Record;

   Quadrant_Size : constant Natural := 36;

   -- Fonts
   ---------------------------------
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.020,
                                             Height    => 0.060,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);
   
   -- Fonts
   ---------------------------------
   Font_2 : Glex.Fonts.Font_Style_Record := (Width     => 0.010,
                                             Height    => 0.040,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is

      use Ada.Numerics;
      use Ada.Numerics.Elementary_Functions;

      S : constant Float := 2.0 * Pi / Float (Quadrant_Size);

      Quadrant_Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (Quadrant_Size + 1);

      Rose_Buffer     : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (10);

      North_Buffer    : Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (3);

      Wind_Buffer     : Glex.Basic.Buffer_Type := Glex.Basic.New_Buffer (4);

      Angle           : Float := 0.0;

      A               : Allocation_Record;

   begin

      -- Quadrant
      --------------------------------------------------------------------------

      for I in 0..Quadrant_Size loop

         Quadrant_Buffer.Line_To (0.5 * Cos (Angle), 0.5 * Sin (Angle));

         Angle := Angle + S;

      end loop;

      Quadrant.Load (Quadrant_Buffer);

      -- Rosetta (NESW deshes)
      --------------------------------------------------------------------------

      Rose_Buffer.Move_To ( 0.55, 0.00);
      Rose_Buffer.Line_To ( 0.40, 0.00);

      Rose_Buffer.Move_To (-0.55, 0.00);
      Rose_Buffer.Line_To (-0.40, 0.00);

      Rose_Buffer.Move_To ( 0.00, 0.55);
      Rose_Buffer.Line_To ( 0.00, 0.40);

      Rose_Buffer.Move_To ( 0.00,-0.55);
      Rose_Buffer.Line_To ( 0.00,-0.40);

      Rose.Load (Rose_Buffer);

      -- North arrow
      --------------------------------------------------------------------------

      North_Buffer.Load_Node ( 0.00, 0.60);
      North_Buffer.Load_Node ( 0.10, 0.45);
      North_Buffer.Load_Node (-0.10, 0.45);

      North_Arrow.Load (North_Buffer);

      -- Wind arrow buffer
      --------------------------------------------------------------------------

      Wind_Buffer.Load_Node (-0.20, 0.00);
      Wind_Buffer.Load_Node (-0.50, 0.08);
      Wind_Buffer.Load_Node (-0.45, 0.00);
      Wind_Buffer.Load_Node (-0.50,-0.08);

      Wind_Arrow.Load (Wind_Buffer);

      -- Buttons
      
      Btn_Plus.Set_Label ("+");

      Btn_Plus.Set_Style (Button_Action);
      
      Btn_Plus.Set_Font_Size (0.5, 0.5);

      A.X := 0.80;
      A.W := 0.10;
      A.Y := 0.57;
      A.H := 0.16;

      Btn_Plus.Set_Allocation (A);

      --

      Btn_Less.Set_Label ("-");

      Btn_Less.Set_Style (Button_Action);
      
      Btn_Less.Set_Font_Size (0.5, 0.5);

      A.Y := A.Y - 0.3;

      Btn_Less.Set_Allocation (A);
      
      --

      Btn_Manual.Set_Label ("MANUAL");

      Btn_Manual.Set_Background_Color (Color_Green);

      Btn_Manual.Set_Border_Color (Color_Black);

      Btn_Manual.Set_Label_Color (Color_Green);

      Btn_Manual.Set_Font_Size (0.3, 0.25);

      A.W := 0.20;      
      A.X := 0.05;      
      A.Y := 0.57;

      Btn_Manual.Set_Allocation (A);
      
      --

      Btn_Auto.Set_Label ("AUTO");

      Btn_Auto.Set_Background_Color (Color_Green);

      Btn_Auto.Set_Border_Color (Color_Black);

      Btn_Auto.Set_Label_Color (Color_Green);

      Btn_Auto.Set_Font_Size (0.3, 0.25);

      A.Y := A.Y - 0.3;

      Btn_Auto.Set_Allocation (A);
      
   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Draw is

      use Ada.Numerics;
      use Glex.Basic;
      use Glex.Fonts;
      use Utility.Calendar;
      use Utility.Strings;
      
      X      : Float := 0.5;      
      Y      : Float := 0.5;      
      Size   : Float := 0.3;
      Aspect : Float renames Glex.Aspect;
      Angle  : Float;
      Matrix : Glex.Transform_Record := Glex.Get_Transform.all;
      Wind   : Float := Convert (Float (Flight.Data.Wind.Norm2), 
                                 Unit_Meter_Second,
                                 Unit_Kilometer_Hour);
      
   begin

      Glex.Get_Transform.Translate (X, Y);

      Glex.Get_Transform.Scale     (Size, Size * Aspect);

      Quadrant.Draw    (Line_White, 0.010, 0.005);

      Rose.Draw        (Line_White, 0.010, 0.005);
      
      North_Arrow.Draw (Color_White, Triangles);
      
      Angle := Float (Flight.Data.Wind.Orientation * 180.0 / Pi);
      
      if Angle < 0.0 then
         
         Angle := 360.0 + Angle;
         
      end if;
      
      Angle := Angle * Pi / 180.0;

      Glex.Get_Transform.Rotate (Angle);

      Wind_Arrow.Draw (Color_Cyan, Triangle_Fan);

      -- Course indicator
      --------------------------------------------------------------------------

      Glex.Get_Transform.Copy (Matrix);
      
      Angle := 270.0 - 180.0 * Float (Flight.Data.Wind.Bearing / Math.Pi);

      if Angle < 0.0 then

         Angle := 360.0 + Angle;

      end if;

      Glex.Fonts.Draw (Utility.Strings.Float_Image (Angle, 0),
                       X,
                       Y,
                       Font_1,
                       Line_Cyan,
                       Alignment_CC);
      
      -- Meassurement data (when it was taken and how)
      --------------------------------------------------------------------------
      
      X := 0.50;
      Y := 0.08;
      
      if Flight.Data.Is_Valid (Flight.Field_Wind) then
         
         declare
            Wind_Age         : Lapses  renames Flight.Data.Age (Flight.Field_Wind);
            Wind_Age_Minutes : Natural := Natural (Seconds (Wind_Age) / 60.0);
            Mode_Text        : String (1..8);
         begin
         
            case Flight.Wind.Get_Source is 
            
               when Wind_Source_Manual =>            
                  Mode_Text := "ENTERED ";
               
               when Wind_Source_Computation =>
                  Mode_Text := "MEASURED";
            
               when Wind_Source_Stream =>
                  Mode_Text := "RECEIVED";
               
            end case;
         
            if Wind_Age_Minutes = 1 then
            
               Glex.Fonts.Draw ("LAST " & Trim (Mode_Text) & Integer_Image (Wind_Age_Minutes) & " MINUTE AGO",
                                X,
                                Y,
                                Font_2,
                                Line_Green,
                                Alignment_CC);
            
            elsif Wind_Age_Minutes > 1 then
            
               Glex.Fonts.Draw ("LAST " & Trim (Mode_Text) & Integer_Image (Wind_Age_Minutes) & " MINUTES AGO",
                                X,
                                Y,
                                Font_2,
                                Line_Green,
                                Alignment_CC);
               
            else
            
               Glex.Fonts.Draw (Trim (Mode_Text) & " NOW",
                                X,
                                Y,
                                Font_2,
                                Line_Green,
                                Alignment_CC);
            
            end if;

         end;
         
      else
         
         Glex.Fonts.Draw ("WIND NOT SET YET",
                          X,
                          Y,
                          Font_2,
                          Line_Red,
                          Alignment_CC);
         
      end if;
      
      -- Allocation
      --------------------------------------------------------------------------

      Allocation.X := X - 0.5 * Size;
      Allocation.W := Size;
      Allocation.Y := Y - 0.5 * Size * Aspect;
      Allocation.H := Size * Aspect;

      -- Auto/manual buttons
      --------------------------------------------------------------------------
      
      case Flight.Wind.Get_Source is
         
         when Wind_Source_Manual => 
            
            Btn_Manual.Set_Style (Button_Enabled);
            
            Btn_Manual.Draw;
      
            Btn_Auto.Set_Style (Button_Disabled);
            
            Btn_Auto.Draw;
            
            -- +/- buttons
            --------------------------------------------------------------------
      
            Btn_Plus.Draw;
      
            Btn_Less.Draw;
      
         when Wind_Source_Computation =>
            
            Btn_Auto.Set_Style (Button_Enabled);
            
            Btn_Auto.Draw;
      
            Btn_Manual.Set_Style (Button_Disabled);
            
            Btn_Manual.Draw;
            
         when Wind_Source_Stream =>
            
            null;
            
      end case;
      
      Glex.Fonts.Draw (Float_Image (Wind, 0),
                       0.85,
                       0.50,
                       Font_1,
                       Line_Cyan,
                       Alignment_CC);

      Glex.Fonts.Draw ("KM/H",
                       0.94,
                       0.50,
                       Font_2,
                       Line_Grass,
                       Alignment_CC);

   end Draw;
   -----------------------------------------------------------------------------

   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
      
      use Math.Vector2;
      
      Step   : constant Long_Float := 1.0 / 3.6;
      Wind   : Vector2_Record := Flight.Data.Wind;
      Norm   : Long_Float     := Flight.Data.Wind.Norm2;    
      Aspect : Long_Float     := Long_Float (Glex.Aspect);
      Manual : Boolean        := False;
      
   begin
      
      if Btn_Manual.Contains (X, Y) then
         
         Flight.Wind.Set_Source (Wind_Source_Manual);
        
         Utility.Log.Put_Message ("wind set to manual");
            
         Refresh := True;
      
      elsif Btn_Auto.Contains (X, Y) then
         
         Flight.Wind.Set_Source (Wind_Source_Computation);
          
         Refresh := True;
      
      elsif Flight.Wind.Get_Source = Wind_Source_Manual then
      
         if Btn_Less.Contains (X, Y) then
         
            if Norm > Step then
            
               Wind.Normalize;
      
               Wind.Scale (Norm - Step);
         
            else
            
               Wind.Set (0.0, 0.0);
            
            end if;
         
            Manual := True;
            
         elsif Btn_Plus.Contains (X, Y) then
         
            if Norm > 0.0 then
            
               Wind.Normalize;
      
               Wind.Scale (Norm + Step);
         
            else
            
               Wind.Set (0.0,-Step);
            
            end if;
         
            Manual := True;
            
         elsif Norm > 0.0 then
                       
            declare
               Wx : Long_Float := Long_Float (0.5 - X) * Aspect;
               Wy : Long_Float := Long_Float (0.5 - Y);
            begin
               
               if Wx * Wx + Wy * Wy < 0.12 then
                  
                  -- Recompute wind
                  ------------------------------------------------------
      
                  Wind.Set (Wx, Wy);
      
                  Wind.Normalize;
      
                  Wind.Scale (Norm);
                  
                  Manual := True;
            
               end if;
               
            end;
            
         end if;
         
         if Manual then
            
            Flight.Wind.Set_Manual_Wind (Wind);
      
            Refresh := True;
            
         end if;
            
      end if;
         
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
      
   
      
   --===========================================================================
   -- Rotates the wind by a given angle
   --===========================================================================
   procedure Rotate_Wind (Angle : Float) is
       
      use Math.Vector2;
      
      Wind : Vector2_Record := Flight.Data.Wind;
      
   begin
      
      if Flight.Wind.Get_Source = Wind_Source_Manual then  
         
         Wind.Rotate (Long_Float (Angle) * Math.Pi / 180.0);
         
         Flight.Wind.Set_Manual_Wind (Wind);
                    
         Refresh := True;
            
      end if;            
      
   end Rotate_Wind;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Handles a key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
      
      Step : constant Float := 2.0;
      
   begin
      
      case Key is
         
         when Panel_Wheel_Left  =>
               
            Rotate_Wind (+Step);
            
         when Panel_Wheel_Right  =>
               
            Rotate_Wind (-  Step);
            
         when Panel_Wheel_Button  =>
               
            null;
            
         when others  =>
              
            null;
            
      end case;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Pages.Wind;
--------------------------------------------------------------------------------
