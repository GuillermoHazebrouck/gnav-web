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
with Display.Panels.Gauges;
with Flight;
with Flight.Register;
with Glex.Colors;
use  Glex.Colors;
with Glex.Fonts;
with Maps;
with Widgets.Button;
use  Widgets.Button;
with Widgets.Panel;
use  Widgets.Panel;
with Widgets.Widget;
use  Widgets.Widget;
with Utility.Calendar;
with Utility.Strings;
with Utility.Units;
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Display.Panels.Vario is
        
   Font_1 : Glex.Fonts.Font_Style_Record := (Width     => 0.007,
                                             Height    => 0.026,
                                             Space     => 0.008,
                                             Rendering => Glex.Fonts.Font_Simple,
                                             Thickness => Glex.Fonts.Font_Regular);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Pnl_Log   : Widget_Record;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   --
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Block : Widget_Record;
   
   --===========================================================================
   --
   --===========================================================================
   procedure Initialize (Allocation : Allocation_Record) is
      
      M : constant Dimension_Float := 0.01;
      H : constant Dimension_Float := 0.10;
      
   begin

      Pnl_Log.Set_Allocation (Allocation);

      Pnl_Log.Set_Background_Color (Color_Gray_1);

      Block.Set_Show_Border (False);
      
   end Initialize;
   -----------------------------------------------------------------------------

   Mx      : constant Float := 0.05;
   Scale_X : constant Float := 600.0;  -- (s)   time range
   Scale_V : constant Float := 6.0;    -- (m/s) vario range
   
   Altitude_Size  : constant Float := 0.8;    -- relative to the height of the panel
   Altitude_Range : constant Float := 800.0;  -- (m) altitude range
      
   --===========================================================================
   -- Represents the altitude as a timeline
   --===========================================================================
   procedure Draw_Altitude is
      
      use Flight;
      use Glex.Fonts;
      use Utility.Calendar;
      use Utility.Strings;

      History : Flight_Data_Record;
      T       : Times := Cached_Time;
      X, Y, P : Float;
      S       : constant Float := Pnl_Log.Get_Allocation.X + Pnl_Log.Get_Allocation.W - Mx;
      Scale_A : constant Float := Pnl_Log.Get_Allocation.W * Altitude_Size / Altitude_Range;
      
   begin
      
      Font_1.Rendering := Glex.Fonts.Font_Simple;
      Font_1.Thickness := Glex.Fonts.Font_Bold;
      
      P := Mx + Pnl_Log.Get_Allocation.X;
      
      for H in Flight.History_Range loop
         
         History := Get_History (H);
         
         if History.Is_Update (Field_Altitude) then
            
            exit when History.Timestamp = No_Time;
            
            X := Seconds (T - History.Timestamp) / Scale_X + Mx + Pnl_Log.Get_Allocation.X;
         
            exit when X > S;
         
            if abs (X - P) > 0.01 then
                  
               -- Vario
               --------------------------------------------------------------------
               if History.Is_Update (Field_Altitude) then
               
                  Y := (History.Altitude - Data.Altitude) * Scale_A;
               
                  Y := Y + Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H;
               
                  Glex.Fonts.Draw ("-",
                                   X, Y,
                                   Font_1,
                                   Line_Green, Alignment_CC);     
                  
                  P := X;
               
               end if;
               
            end if;
 
         end if;
         
      end loop;
      
   end Draw_Altitude;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   -- Represents the register as a timeline
   --===========================================================================
   procedure Draw is
      
      use all type Flight.Flight_Mode_Kinds;
      use Flight.Register;
      use Glex.Fonts;
      use Utility.Calendar;
      use Utility.Strings;
      
      Register   : access Register_Record;
      Allocation : Allocation_Record;
      T          : Times := Cached_Time;
      T1, T2     : Float;
      X, Y       : Float;
      S          : constant Float := Pnl_Log.Get_Allocation.X + Pnl_Log.Get_Allocation.W - 2.5 * Mx;
            
   begin
      
      -- Background panel
      
      Pnl_Log.Draw;

      -- Register blocks
      --------------------------------------------------------------------------
      for R in Register_Range loop
         
         Register := Get_Register (R);
         
         if Register.Active then
            
            T1 := Seconds (T - Register.Start) / Scale_X;
            T2 := Seconds (T - Register.Stop)  / Scale_X;
         
            Allocation.X := T1 + Mx + Pnl_Log.Get_Allocation.X;
            Allocation.W := T2 - T1;
            
            exit when Allocation.X > S;
                  
            if Allocation.X + Allocation.W > S then
            
               Allocation.W := S - Allocation.X;
               
            end if;
               
            X := Allocation.X + 0.5 * Allocation.W;
            
            Font_1.Rendering := Glex.Fonts.Font_Simple;
            Font_1.Thickness := Glex.Fonts.Font_Regular;
            
            if Register.Lift > 0.0 then
               
               Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H;
               Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * Register.Lift / Scale_V;
            
               Block.Set_Allocation       (Allocation);
               Block.Set_Background_Color (Color_Gray_5);
               Block.Draw;
               
            end if;
            
            if Register.Sink < 0.0 then
               
               Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * abs Register.Sink / Scale_V;
               Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H - Allocation.H;
               
               Block.Set_Allocation       (Allocation);
               Block.Set_Background_Color (Color_Gray_5);
               Block.Draw;
          
            end if;
            
            -- Vario
            --------------------------------------------------------------------
            if Register.Vario > 0.0 then
               
               Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H;
               Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * Register.Vario / Scale_V;
            
               Block.Set_Allocation       (Allocation);
               Block.Set_Background_Color (Color_Cyan);
               Block.Draw;

            elsif Register.Vario < 0.0 then
               
               Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * abs Register.Vario / Scale_V;
               Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H - Allocation.H;
               
               Block.Set_Allocation       (Allocation);
               Block.Set_Background_Color (Color_Orange);
               Block.Draw;
     
            end if;
                        
         end if;
          
      end loop;
               
      -- Current variometer bar
      --------------------------------------------------------------------------
      if Flight.Data.Is_Recent (Flight.Field_Vario, 6.0) then
           
         -- Background
           
         Allocation.X := Pnl_Log.Get_Allocation.X + 0.3 * Mx;
         Allocation.W := 0.6 * Mx;         
         Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.1 * Pnl_Log.Get_Allocation.H;
         Allocation.H := 0.8 * Pnl_Log.Get_Allocation.H;
            
         Block.Set_Allocation       (Allocation);
         Block.Set_Background_Color (Color_Gray_5);
         Block.Draw;
         
         -- Current value
         
         Allocation.X := Pnl_Log.Get_Allocation.X + 0.4 * Mx;
         Allocation.W := 0.4 * Mx;
         
         X := Allocation.X + 0.5 * Allocation.W;
         
         if Flight.Data.Vario > 0.0 then
               
            Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H;
            Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * Flight.Data.Vario / Scale_V;
            
            Block.Set_Allocation       (Allocation);
            Block.Set_Background_Color (Color_Green);
            Block.Draw;

            Y := Allocation.Y + Allocation.H + 0.5 * Font_1.Height;
               
         elsif Flight.Data.Vario < 0.0 then
               
            Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * abs Flight.Data.Vario / Scale_V;
            Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H - Allocation.H;
               
            Block.Set_Allocation       (Allocation);
            Block.Set_Background_Color (Color_Red);
            Block.Draw;
     
            Y := Allocation.Y + Allocation.H - 0.5 * Font_1.Height;
               
         end if;
         
      end if;
         
      -- 1 m/s reference lines
      --------------------------------------------------------------------------
      Block.Set_Background_Color (Pnl_Log.Get_Background_Color);
      
      Allocation.X := Pnl_Log.Get_Allocation.X;
      Allocation.W := Pnl_Log.Get_Allocation.W;
      Allocation.H := 0.004;
      
      for I in -5..5 loop
        
         Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H * (1.0 + Float (I) / Scale_V) - 0.002;
                     
         Block.Set_Allocation (Allocation);
         Block.Draw;
         
      end loop;
      
      -- Altitude stairs
      --------------------------------------------------------------------------
      
      --Draw_Altitude;
      
      -- Labels
      --------------------------------------------------------------------------
      for R in Register_Range loop
         
         Register := Get_Register (R);
         
         if Register.Active then
            
            T1 := Seconds (T - Register.Start) / Scale_X;
            T2 := Seconds (T - Register.Stop ) / Scale_X;
         
            Allocation.X := T1 + Mx + Pnl_Log.Get_Allocation.X;
            Allocation.W := T2 - T1;            
            Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H;
            
            exit when Allocation.X > S;
                  
            if Allocation.X + Allocation.W > S then
            
               Allocation.W := S - Allocation.X;
               
            end if;
               
            X := Allocation.X + 0.5 * Allocation.W;
                         
            -- Average sink and lift
            --------------------------------------------------------------------
            if Allocation.W > 3.0 * (Font_1.Width + Font_1.Space) then
                  
               Font_1.Rendering := Glex.Fonts.Font_Simple;
               Font_1.Thickness := Glex.Fonts.Font_Regular;
            
               if Register.Lift > 0.0 then
               
                  if Register.Lift > 4.0 then
                     
                     Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * 4.0 / Scale_V;
            
                     Y := Allocation.Y + Allocation.H + 0.5 * Font_1.Height;
               
                     Glex.Fonts.Draw ("}4.0",
                                      X, Y,
                                      Font_1,
                                      Line_Cyan, Alignment_LC);     
                     
                  else
                     
                     Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * Register.Lift / Scale_V;
            
                     Y := Allocation.Y + Allocation.H + 0.5 * Font_1.Height;
               
                     Glex.Fonts.Draw (Float_Image (Register.Lift, 1),
                                      X, Y,
                                      Font_1,
                                      Line_Cyan, Alignment_LC);     
                     
                  end if;
                     
               end if;
               
               if Register.Sink < 0.0 then
               
                  if Register.Sink < -4.0 then
                     
                     Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * (-4.0) / Scale_V;
                     Allocation.Y := Allocation.Y - Allocation.H;
               
                     Y := Allocation.Y - 0.5 * Font_1.Height;
               
                     Glex.Fonts.Draw ("{-4.0",
                                      X, Y,
                                      Font_1,
                                      Line_Orange, Alignment_TC);     
                     
                  else
                     
                     Allocation.H := 0.5 * Pnl_Log.Get_Allocation.H * abs Register.Sink / Scale_V;
                     Allocation.Y := Allocation.Y - Allocation.H;
               
                     Y := Allocation.Y - 0.5 * Font_1.Height;
               
                     Glex.Fonts.Draw (Float_Image (abs Register.Sink, 1),
                                      X, Y,
                                      Font_1,
                                      Line_Orange, Alignment_TC);     
                     
                  end if;
                     
               end if;
                     
            end if;
                           
            -- Mode
            --------------------------------------------------------------------
            
            if Allocation.W > 2.0 * (Font_1.Width + Font_1.Space) then
               
               Font_1.Rendering := Glex.Fonts.Font_Glow;
               Font_1.Thickness := Glex.Fonts.Font_Bold;
            
               Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H * (1.0 + 0.5 * Register.Vario / Scale_V);
            
               if Register.Mode = Mode_Straight then
               
                  Glex.Fonts.Draw ("<<",
                                   X, Y,
                                   Font_1,
                                   Line_Yellow, Alignment_CC);
               
               elsif Register.Mode = Mode_Circling then
               
                  Glex.Fonts.Draw ("C",
                                   X, Y,
                                   Font_1,
                                   Line_Yellow, Alignment_CC);
               
               end if;
                
            end if;
            
         end if;
          
      end loop;
      
      -- Center line
      --------------------------------------------------------------------------
      
      Allocation.X := Pnl_Log.Get_Allocation.X + Mx;
      Allocation.W := Pnl_Log.Get_Allocation.W - 3.5 * Mx;      
      Allocation.Y := Pnl_Log.Get_Allocation.Y + 0.5 * Pnl_Log.Get_Allocation.H - 0.002;
      Allocation.H := 0.004;
            
      Block.Set_Allocation       (Allocation);
      Block.Set_Background_Color (Color_Yellow);
      Block.Draw;
      
   end Draw;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Screen_Pressed (X, Y : Float) is
   begin
      
      null;
      
   end Screen_Pressed;
   -----------------------------------------------------------------------------
   
   
   
   
   --===========================================================================
   --
   --===========================================================================
   procedure Key_Changed (Key : Front_Panel_Keys) is
   begin
      
      null;
      
   end Key_Changed;
   -----------------------------------------------------------------------------
          
     
     
end Display.Panels.Vario;
--------------------------------------------------------------------------------
