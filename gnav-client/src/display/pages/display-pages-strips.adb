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
with Display.Pages.Route_Edition;
with Display.Panels.Gauges;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Glex.Symbols;
with Flight.Plan;
use  Flight.Plan;
with Utility.Strings;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Widget;
use  Widgets.Widget;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Pages.Strips is
   
   -- Indicates if edition is on
   ---------------------------------
   Edition_Mode : Boolean := False;
   
   -- Font for the strips
   ---------------------------------
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.014, 
                                             Height    => 0.036, 
                                             Space     => 0.006,
                                             Rendering => Glex.Fonts.Font_Glow,
                                             Thickness => Glex.Fonts.Font_Regular);

   -- Font for the leg info
   ---------------------------------
   Font_2 : Glex.Fonts.Font_Style_Record := Font_1;
      
   -- Font for the units
   ---------------------------------
   Font_3 : Glex.Fonts.Font_Style_Record := Font_1;
      
   --///////////////////////////////////////////////////////////////////////////
   -- Actions
   --///////////////////////////////////////////////////////////////////////////
   
   Btn_Name : Button_Record;
   
   Btn_Up   : Button_Record;
   
   Btn_Down : Button_Record;
  
   Btn_Back : Button_Record;
   
   --///////////////////////////////////////////////////////////////////////////
   -- Task strips
   --///////////////////////////////////////////////////////////////////////////
   
   Frm_Waypoint : array (Waypoint_Range) of Widget_Record;
   
   Frm_Task     : Widget_Record;
      
   Frm_Progress : Widget_Record;
      
   Frm_Bar      : Widget_Record;
   
   First_Index  : Waypoint_Range := Waypoint_Range'First;
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Configure_Back_Button_Color is
   begin
      
      if Flight.Plan.Flight_Plan.Go_Back then
            
         Btn_Back.Set_Style (Button_Enabled);
            
      else
            
         Btn_Back.Set_Style (Button_Disabled);
            
      end if;
      
   end Configure_Back_Button_Color;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize is
      
      use Utility.Strings;
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.06;
      W : constant Dimension_Float := 0.22;
      
      Allocation : Allocation_Record;
      
   begin

      -- Fonts
      -------------------------------------------------

      -- Leg font
      -----------------------
      Font_2.Height := 0.8 * Font_1.Height;      
      Font_2.Width  := 0.8 * Font_1.Width;
            
      -- Units font
      -----------------------
      Font_3.Height := 0.6 * Font_1.Height;      
      Font_3.Width  := 0.6 * Font_1.Width;
             
      -- Flight plan name
      -------------------------------------------------
      
      Allocation.X := 0.165;
      
      Allocation.Y := 0.89;
      
      Allocation.W := 0.27;
      
      Allocation.H := 0.10;
      
      Btn_Name.Set_Allocation (Allocation);
      
      Btn_Name.Set_Label ("-R> " & Trim (Flight_Plan.Name));
      
      Btn_Name.Set_Style (Button_Normal); 
      
      Btn_Name.Set_Font_Size (0.4, 0.3);
        
      -- Change active waypoint one position upwards
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Allocation.W := 0.10;
      
      Btn_Up.Set_Allocation (Allocation);
      
      Btn_Up.Set_Symbol (Glex.Symbols.Triangle_Up);
      
      Btn_Up.Set_Style (Button_Action);     
      
      -- Change active waypoint one position downwards
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Btn_Down.Set_Allocation (Allocation);
      
      Btn_Down.Set_Symbol (Glex.Symbols.Triangle_Down);
      
      Btn_Down.Set_Style (Button_Action);      
      
      -- Back route button
      -------------------------------------------------
      
      Allocation.X := Allocation.X + Allocation.W + M;
      
      Btn_Back.Set_Allocation (Allocation);
      
      Btn_Back.Set_Label ("BACK");
      
      Configure_Back_Button_Color;
      
      Btn_Back.Set_Font_Size (0.4, 0.3);
      
      -- Frames for each waypoint/task pair
      -------------------------------------------------
      
      Allocation.X := 0.06;
      
      Allocation.W := 0.70;
      
      Allocation.H := 2.0 * Font_1.Height;
      
      for I in Waypoint_Range loop
         
         Frm_Waypoint (I).Set_Allocation (Allocation);
      
         Frm_Waypoint (I).Set_Border_Color (Color_Magenta);
      
         Frm_Waypoint (I).Set_Background_Color (Color_Gray_5);
         
      end loop;
      
      Allocation.W := 0.25;
      
      Allocation.H := 2.5 * Font_2.Height;
      
      Frm_Task.Set_Allocation (Allocation);
      
      Frm_Task.Set_Background_Color (Color_Sky);
      
      Frm_Task.Set_Border_Color (Color_Magenta);
      
      Frm_Progress.Set_Background_Color (Color_Red);
      
      Frm_Progress.Set_Border_Color (Color_Magenta);
             
      -- Flight data panel
      ------------------------------------------------------  
      
      Display.Panels.Gauges.Initialize;
     
      -- Route edition
      ------------------------------------------------------  
      
      Display.Pages.Route_Edition.Initialize;
      
   end Initialize;
   -----------------------------------------------------------------------------
   
        
   
   
   --===========================================================================
   -- Draws the strips
   --===========================================================================
   procedure Draw_Strips is 
      
      use Utility.Strings;
      use Glex.Fonts;

      -- Allocation variables
      M : Float := 0.040;
      Y : Float := 0.820;
      X : Float := 0.000;
      A : Allocation_Record;
      K : Natural := 0;
      Is_Active   : Boolean := False;
      Is_Previous : Boolean := False;
      Is_Home     : Boolean := False;
         
      -- Font colors
      Color_Name      : Line_Color_Record;
      Color_Elevation : Line_Color_Record;
      Color_Margin    : Line_Color_Record;
      Color_Units     : Line_Color_Record;
      Color_Task      : Line_Color_Record;
      
      use Maps;
      
   begin

      for I in Waypoint_Range loop
            	   
         exit when not Flight_Plan.Waypoints(I).Is_Loaded;
         
         if Flight_Plan.Waypoints(I).Is_Active then
         
            if Flight_Plan.Go_Back then
               
               if I < First_Index then
              
                  First_Index := I;
               
               elsif I > First_Index + 5 then
            
                  First_Index := I - 5;
               
               end if;
               
            else
               
               if I < First_Index then
              
                  First_Index := I;
               
               elsif I > First_Index + 5 then
            
                  First_Index := I - 5;
               
               end if;
               
            end if;
            
            exit;
                      
         end if;
               
      end loop;
                 
      -- Flight panel
      -----------------------
      Display.Panels.Gauges.Draw;
      
      -- Buttons
      -----------------------

      Btn_Name.Set_Label (Trim (Flight_Plan.Name));
      
      Btn_Name.Draw;

      Btn_Down.Draw;
      
      Btn_Up.Draw;
      
      Btn_Back.Draw;
      
      --Table header
      -----------------------
      Y := Btn_Name.Get_Allocation.Y - 0.025;
     
      for I in Waypoint_Range loop
            	
         exit when not Flight_Plan.Waypoints (I).Is_Loaded;
         
         if I >= First_Index then
         
            K := K + 1;
            
            Y := Y - (Font_1.Height + M);
            
            Is_Active   := Flight_Plan.Waypoints (I).Is_Active;
            
            Is_Previous := (not Flight_Plan.Go_Back and then I < Waypoint_Range'Last  and then Flight_Plan.Waypoints (I + 1).Is_Active) or
                           (    Flight_Plan.Go_Back and then I > Waypoint_Range'First and then Flight_Plan.Waypoints (I - 1).Is_Active);
                          
            Is_Home     := Is_Active and I = Waypoint_Range'First;
            
            -- Select font according to status
            ----------------------------------------------
            if Is_Active then
               
               Color_Name      := Line_Magenta;                      
               Color_Elevation := Line_Brown;
               Color_Units     := Line_Gray;
               
            elsif Is_Previous then
            
               Color_Name.Fore := Color_Gray_8;   
               Color_Name.Glow := Color_Black;                       
               Color_Elevation := Line_Brown;
               Color_Units     := Line_Gray;
               
            else
            
               Color_Name.Fore := Color_Gray_4;   
               Color_Name.Glow := Color_Gray_1;               
               Color_Elevation := Color_Name;
               Color_Units     := Color_Name;
               
            end if;
            
            A := Frm_Waypoint (I).Get_Allocation;
            
            A.Y := Y - 0.50 * Font_1.Height;
            
            Frm_Waypoint (I).Set_Allocation (A);
            
            if Is_Active then
            
               Frm_Waypoint (I).Set_Border_Color (Color_Magenta);
      
               Frm_Waypoint (I).Set_Background_Color (Color_White);
               
            elsif Is_Previous then
                              
               Frm_Waypoint (I).Set_Border_Color (Color_Gray_5);
      
               Frm_Waypoint (I).Set_Background_Color (Color_White);
               
            else
                                
               Frm_Waypoint (I).Set_Border_Color (Color_Gray_2);
      
               Frm_Waypoint (I).Set_Background_Color (Color_Gray_1);
               
            end if;
                    
            Frm_Waypoint (I).Draw;
            
            X := Frm_Waypoint (I).Get_Allocation.X;
            
            Glex.Fonts.Draw (Trim (Waypoint_Range'Image (I)),
                             0.03, 
                             Y,
                             Font_1,
                             Color_Name,
                             Alignment_LC);
            
            -- Blinking name when in proximity
            ----------------------------------------------
            if not Flight_Plan.Waypoints (I).In_Proximity or else Blink then
           
               -- Waypoint name
               ----------------------------------------------
               Glex.Fonts.Draw (Trim (Flight_Plan.Waypoints (I).Name),
                                X + 0.02, 
                                Y,
                                Font_1,
                                Color_Name,
                                Alignment_LL);
                      
            end if;
         
            -- Waypoint distance
            ----------------------------------------------	 	
            Glex.Fonts.Draw (Float_Image (Flight_Plan.Waypoints (I).Distance, 1),
                             X + 0.25, 
                             Y,
                             Font_1,
                             Color_Name,
                             Alignment_LR);
         
            Glex.Fonts.Draw ("KM",
                             X + 0.26, 
                             Y,
                             Font_3,
                             Color_Units,
                             Alignment_LL);
            
            -- Waypoint bearing
            ----------------------------------------------	 	
            Glex.Fonts.Draw (Float_Image (Flight_Plan.Waypoints (I).Bearing, 0),
                             X + 0.37, 
                             Y,
                             Font_1,
                             Color_Name,
                             Alignment_LR);
         
            Glex.Fonts.Draw ("*",
                             X + 0.38, 
                             Y + 0.01,
                             Font_3,
                             Color_Units,
                             Alignment_LL);
            
            -- Waypoint elevation
            ----------------------------------------------
            Glex.Fonts.Draw (Float_Image (Flight_Plan.Waypoints (I).Elevation, 0),
                             X + 0.51, 
                             Y,
                             Font_1,
                             Color_Elevation,
                             Alignment_LR);
     
            Glex.Fonts.Draw ("M",
                             X + 0.52,
                             Y,
                             Font_3,
                             Color_Units,
                             Alignment_LL);
            
            -- Waypoint altitude margin
            ----------------------------------------------
            
            if Is_Active or Is_Previous then              
               
               case Flight_Plan.Waypoints (I).In_Range is
               
                  when Range_Safe        => Color_Margin := Line_Grass;
                  when Range_Unsafe      => Color_Margin := Line_Yellow;
                  when Range_Unreachable => Color_Margin := Line_Red;
                  
               end case;
              
            else
                   
               -- Inactive
               Color_Margin := Color_Name;
                  
            end if;
            
            Glex.Fonts.Draw (Flight_Plan.Waypoints (I).Get_Margin,
                             X + 0.65, 
                             Y,
                             Font_1,
                             Color_Margin,
                             Alignment_LR);
            
            Glex.Fonts.Draw ("M",
                             X + 0.66,
                             Y,
                             Font_3,
                             Color_Units,
                             Alignment_LL);
            
            -- Exit when reaching the bottom
            ----------------------------------------------
            exit when K = 6;
              
            -- Leg info to the next waypoint
            ----------------------------------------------
            if Flight_Plan.Tasks (I).Is_Loaded then
            
               Y := Y - (Font_2.Height + M);
              
               if Flight_Plan.Tasks (I).Is_Active then
                  
                  Color_Task := Line_Purple;
                  
                  -- Task highlight
                  --------------------------
                     
                  A := Frm_Task.Get_Allocation;
                  
                  A.Y := Y - 0.75 * Font_2.Height;
                  
                  Frm_Task.Set_Allocation (A);
                  
                  -- Progress bar
                  --------------------------
                     
                  A.H := 0.5 * A.H;
                     
                  A.X := A.X + A.W;
                     
                  A.W := Float'Max (0.0, Frm_Waypoint (I).Get_Allocation.X + Frm_Waypoint (I).Get_Allocation.W - A.X);
                     
                  A.W := Flight_Plan.Tasks (I).Progress * A.W;
                     
                  Frm_Progress.Set_Allocation (A);
                     
               else
                  
                  Color_Task.Fore := Color_Gray_4;
                  Color_Task.Glow := Color_Gray_1;
                  
               end if;
                          
               if Flight_Plan.Tasks (I).Is_Active then
                  
                  if Flight_Plan.Go_Back then
                     
                     Glex.Fonts.Draw ("}",
                                      0.12,
                                      Y,
                                      Font_1,
                                      Color_Task,
                                      Alignment_LR);
                     
                  else
                     
                     Glex.Fonts.Draw ("{",
                                      0.12,
                                      Y,
                                      Font_1,
                                      Color_Task,
                                      Alignment_LR);
                     
                  end if;
                  
               end if;
                                
               Glex.Fonts.Draw (Float_Image (Flight_Plan.Tasks (I).Length, 0),
                                0.17,
                                Y,
                                Font_2,
                                Color_Task,
                                Alignment_LR);
         
               Glex.Fonts.Draw ("KM",
                                0.18, 
                                Y,
                                Font_3,
                                Color_Units,
                                Alignment_LL);
               
               Glex.Fonts.Draw (Float_Image (Flight_Plan.Tasks (I).Course, 0),
                                0.27,
                                Y,
                                Font_2,
                                Color_Task,
                                Alignment_LR);
         
               Glex.Fonts.Draw ("*",
                                0.28, 
                                Y,
                                Font_3,
                                Color_Units,
                                Alignment_LL);
               
            end if;
            
         end if;
                     
      end loop;
            
   end Draw_Strips;
   -----------------------------------------------------------------------------
      
   
   
   --===========================================================================
   -- Draw
   --===========================================================================
   procedure Draw is 
   begin
      
      if Edition_Mode then
         
         Display.Pages.Route_Edition.Draw;
         
      else
         
         Draw_Strips;
         
      end if;
       
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   function Back_From_Edition_Mode return Boolean is
   begin
      
      if Edition_Mode then
      
         Refresh := True;
      
         Edition_Mode := False;
         
         Flight.Plan.Jump_In_Proximity := True;
                    
         return True;
      
      else
         return False;
      
      end if;
      
   end Back_From_Edition_Mode;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
              
      if Edition_Mode then
         
         Display.Pages.Route_Edition.Screen_Pressed (X, Y);
         
      else
               
         Display.Panels.Gauges.Screen_Pressed (X, Y);
      
         if Btn_Up.Contains (X, Y) then
         
            Flight.Plan.Goto_Previous_Waypoint;
         
            Flight.Plan.Update_Flight_Plan;
         
            Refresh := True;
         
         elsif Btn_Down.Contains (X, Y) then
         
            Flight.Plan.Goto_Next_Waypoint;
         
            Flight.Plan.Update_Flight_Plan;
         
            Refresh := True;
         
         elsif Btn_Back.Contains (X, Y) then
         
            Flight.Plan.Toggle_Go_Back;
         
            Configure_Back_Button_Color;
         
            Refresh := True;
            
         elsif Btn_Name.Contains (X, Y) then
           
            Refresh := True;
            
            Edition_Mode := True;
           
            Flight.Plan.Jump_In_Proximity := False;
           
         end if;
         
      end if;
         
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Handles a panel key press
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      -- Normal mode
      --------------------------------------------------------------------------
      case Key is
         
         when Panel_Wheel_Left =>
            
            Flight.Plan.Goto_Previous_Waypoint;
         
            Flight.Plan.Update_Flight_Plan;
         
            Refresh := True;
            
         when Panel_Wheel_Right =>
            
            Flight.Plan.Goto_Next_Waypoint;
         
            Flight.Plan.Update_Flight_Plan;
         
            Refresh := True;
            
         when Panel_Wheel_Button =>
            
            null;
            
         when Panel_Button_Right =>
            
            null;
                        
         when Panel_Button_Left =>
              
            null;
            
         when others =>
            
            null;
            
      end case;
                   
   end Key_Changed;
   -----------------------------------------------------------------------------
   
end Display.Pages.Strips;
--------------------------------------------------------------------------------
