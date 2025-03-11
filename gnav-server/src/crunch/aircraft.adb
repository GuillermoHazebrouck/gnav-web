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
with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
-- Gnav
with Utility;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Aircraft is
   
   Aircraft_Count : Natural := 0;
   Negative_Polar : Boolean := False;
   Oscillation_CL : Float   := 0.0;
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Check_Consistency is
   begin
      Ada.Text_Io.Put_Line ("checkig data quality");
      
      for Aircraft of Aircrafts loop
         
         if Aircraft.Valid then
            
            if Aircraft.Cl_Max <= Aircraft.Cl_Min then
               
               Ada.Text_Io.Put_Line ("error: invalid CL range for " & Aircraft.Model);
               Aircraft.Valid := False;
                              
            elsif Aircraft.Aspect_Ratio <= 0.0 then
               
               Ada.Text_Io.Put_Line ("error: invalid aspect ratio for " & Aircraft.Model);
               Aircraft.Valid := False;
                                 
            elsif Aircraft.Wing_Area <= 0.0 then
               
               Ada.Text_Io.Put_Line ("error: invalid wing area for " & Aircraft.Model);
               Aircraft.Valid := False;
                           
            elsif
              Aircraft.L0 = 0.0 and then
              Aircraft.L1 = 0.0 and then
              Aircraft.L2 = 0.0 and then
              Aircraft.L3 = 0.0 and then
              Aircraft.L4 = 0.0
            then
               
               Ada.Text_Io.Put_Line ("error: invalid clean drag for " & Aircraft.Model);
               Aircraft.Valid := False;
                            
            elsif
              Aircraft.T0 = 0.0 and then
              Aircraft.T1 = 0.0 and then
              Aircraft.T2 = 0.0 and then
              Aircraft.T3 = 0.0 and then
              Aircraft.T4 = 0.0
            then
               
               Ada.Text_Io.Put_Line ("error: invalid rough drag for " & Aircraft.Model);
               Aircraft.Valid := False;
               
            end if;
                        
            -- Check if the polar has positive values along the range
            ---------------------------------------------------------            

            --Ada.Text_Io.Put_Line (Aircraft.Model); 
                       
            declare
               A0        : Long_Float := Long_Float (Aircraft.L0);
               A1        : Long_Float := Long_Float (Aircraft.L1);
               A2        : Long_Float := Long_Float (Aircraft.L2 + 1.0 / (3.141593 * Aircraft.Aspect_Ratio));
               A3        : Long_Float := Long_Float (Aircraft.L3);
               A4        : Long_Float := Long_Float (Aircraft.L4);
               CL_Range  : Long_Float := Long_Float (Aircraft.Cl_Max - Aircraft.Cl_Min);
               CL        : Long_Float := 0.0;
               CD        : Long_Float := 0.0;
               Percentage: Float      := 0.0;
            begin
               
               for I in 0..40 loop
                  
                  CL := Long_Float (Aircraft.Cl_Min) + Long_Float (I) / 40.0 * CL_Range;                
                  CD := A0 + A1 * CL + A2 * CL * CL + A3 * CL * CL * CL + A4 * CL * CL * CL * CL;
                  
                  --Ada.Text_Io.Put_Line (Utility.Float_Image (Float (CL), 4) & " " & Utility.Float_Image (Float (CD), 4)); 
                       
                  if CD <= 0.0 then

                     Aircraft.Valid := False;
               
                     Percentage := 100.0 * (Float (CL) - Aircraft.Cl_Min) / Float (CL_Range);
                     
                     Ada.Text_Io.Put_Line ("error: polar curve is negative for " & Aircraft.Model & " at " & Utility.Float_Image (Percentage, 0) & "% CL range");
               
                  end if;
                
               end loop;
              
            end;
            
            -- Check if the polar has positive curvature
            ------------------------------------------------------            

            declare
               A2        : Long_Float := Long_Float (Aircraft.L2 + 1.0 / (3.141593 * Aircraft.Aspect_Ratio));
               A3        : Long_Float := Long_Float (Aircraft.L3);
               A4        : Long_Float := Long_Float (Aircraft.L4);
               CL_Range  : Long_Float := Long_Float (Aircraft.Cl_Max - Aircraft.Cl_Min);
               CL        : Long_Float := 0.0;
               C1, C2    : Long_Float := 0.0;
               Percentage: Float      := 0.0;
            begin
               
               for I in 0..40 loop
                  
                  CL := Long_Float (Aircraft.Cl_Min) + Long_Float (I) / 40.0 * CL_Range;
                  C1 := C2;                  
                  C2 := 2.0 * A2 + 6.0 * A3 * CL + 12.0 * A4 * CL * CL;
                  
                  if (C1 > 0.0 and C2 < 0.0) or else (C1 < 0.0 and C2 > 0.0) then

                     Percentage := 100.0 * (Float (CL) - Aircraft.Cl_Min) / Float (CL_Range);
                     
                     Ada.Text_Io.Put_Line ("warning: polar curve inflection for " & Utility.Trim (Aircraft.Model) & " at " & Utility.Float_Image (Percentage, 0) & "% CL range");
               
                     Ada.Text_Io.Put_Line ("CL = " & Utility.Float_Image (Float (CL), 4)); 
                       
                     Ada.Text_Io.Put_Line ("V  = " & Utility.Float_Image (3.6 * Sqrt (9.8 * 2.0 * Aircraft.Maximum_Mass / (1.225 * Aircraft.Wing_Area * Float (CL))), 1) & "km/h"); 
                       
                  end if;
                
               end loop;
              
            end;
            
            -- Check if the polar slope has positive curvature
            ------------------------------------------------------            

            declare
               A0        : Long_Float := Long_Float (Aircraft.L0);
               A3        : Long_Float := Long_Float (Aircraft.L3);
               A4        : Long_Float := Long_Float (Aircraft.L4);
               CL_Range  : Long_Float := Long_Float (Aircraft.Cl_Max - Aircraft.Cl_Min);
               CL        : Long_Float := 0.0;
               C1, C2    : Long_Float := 0.0;
               Percentage: Float      := 0.0;
            begin
                  
               for I in 0..40 loop
                  
                  CL := Long_Float (Aircraft.Cl_Min) + Long_Float (I) / 40.0 * CL_Range;                  
                  C1 := C2;
                  C2 := 2.0 * A0 / (CL * CL * CL) + 2.0 * A3 + 6.0 * A4 * CL;
                  
                  if (C1 > 0.0 and C2 <= 0.0) or else (C1 < 0.0 and C2 >= 0.0) then

                     Percentage := 100.0 * (Float (CL) - Aircraft.Cl_Min) / Float (CL_Range);
                     
                     Ada.Text_Io.Put_Line ("warning: polar slope inflection for " & Aircraft.Model & " at " & Utility.Float_Image (Percentage, 0) & "% CL range");
               
                  end if;
                
               end loop;
                
            end;
            
         end if;
                  
      end loop;
      
   end Check_Consistency;
   -----------------------------------------------------------------------------
   
   
   

   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compile_Data is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;
      Stream  : Stream_Access;

   begin
      
         Check_Consistency;
         
      if Aircraft_Count > 0 then

         Ada.Text_Io.Put_Line ("compiling aircraft" & Natural'Image (Aircraft_Count));

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "aircraft.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Natural'Write (Stream, Aircraft_Count); --  4 bytes

         for Aircraft of Aircrafts loop
         
            if Aircraft.Valid then
         
               Ada.Text_Io.Put_Line (Aircraft.Model);
               
               -- Basic info
               --------------------------
               Aircraft_Names'Write (Stream, Aircraft.Model);
               
               -- Characteristics
               --------------------------
               
               Float'Write (Stream, Aircraft.Wing_Area);
               Float'Write (Stream, Aircraft.Aspect_Ratio);
               Float'Write (Stream, Aircraft.Empty_Mass);
               Float'Write (Stream, Aircraft.Maximum_Mass);
               Float'Write (Stream, Aircraft.V_WL);
               Float'Write (Stream, Aircraft.V_NE);
               Float'Write (Stream, Aircraft.V_NO);
               
               -- Mass points
               --------------------------
               Ada.Text_Io.Put_Line ("mass points");
               Natural'Write (Stream, Aircraft.Mass_Count); --  4 bytes
               
               for Point of Aircraft.Mass_Points loop
                  if Point.Active then
                     Aircraft_Names'Write (Stream, Point.Label);
                     Float'Write (Stream, Point.Position);
                     Float'Write (Stream, Point.Mass); -- (default value)
                     Float'Write (Stream, Point.Mass_Max);
                     Float'Write (Stream, Point.Mass_Min);
                     Ada.Text_Io.Put_Line ("*");
                  end if;         
               end loop;
                               
               -- Polar curve
               --------------------------   
               Float'Write (Stream, Aircraft.Cl_Min);
               Float'Write (Stream, Aircraft.Cl_Max);
               
               Float'Write (Stream, Aircraft.L0);          
               Float'Write (Stream, Aircraft.L1); 
               Float'Write (Stream, Aircraft.L2); 
               Float'Write (Stream, Aircraft.L3); 
               Float'Write (Stream, Aircraft.L4); 
                             
               Float'Write (Stream, Aircraft.T0);          
               Float'Write (Stream, Aircraft.T1); 
               Float'Write (Stream, Aircraft.T2);
               Float'Write (Stream, Aircraft.T3); 
               Float'Write (Stream, Aircraft.T4);
               
            end if;
         
         end loop;
      
         Close (File_Id);
         
      end if;

   exception
      when E : others => 
         
         Ada.Text_IO.Put_Line ("error while writing aircraft setup");

         Close (File_Id);
         
   end Compile_Data;
   ----------------------------------------------------------------------------- 
   
   
   
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Load_Aircraft_Data is
      
      use Ada.Text_IO;
      use Utility;
            
      File_Name    : constant String := "aircraft.dat";

      File_Id      : File_Type;
 
      Line_Reader  : String_Buffer (1000);

      Value_Reader : String_Buffer (100);
      
      Mass_Index   : Mass_Point_Range := Mass_Point_Range'First;
      
      Index        : Aircraft_Range   := Aircraft_Range'First;
      
      Aircraft     : access Aircraft_Record := Aircrafts (1)'Access;
      
   begin
      
      Aircraft_Count := 0;
      
      if Ada.Directories.Exists (File_Name) then

         Open (File_Id, In_File, File_Name);

         Ada.Text_IO.Put_Line ("loading aircraft data");

         while not End_Of_File (File_Id) loop
            
            Line_Reader.Load (Ada.Text_IO.Get_Line (File_Id));

            declare
               Key   : String := Line_Reader.Read_Next ('=');
               Value : String := Line_Reader.Read_Next ('=');
            begin
               
               if Key = "#" then
               
                  if Index < Aircraft_Range'Last then
                     
                     -- New Aircraft
                     ------------------------
                     Index := Index + 1;
                  
                     Mass_Index := Mass_Point_Range'First;
                  
                     Aircraft := Aircrafts (Index)'Access;
                  
                     Aircraft.Valid := False;
                     
                     Aircraft_Count := Aircraft_Count + 1;
                      
                  else
                     Ada.Text_IO.Put_Line ("impossible to load all aircraft, stack is full");
                     return;
                     
                  end if;
                     
               elsif Key = "MODEL" then
                  
                  Override (Aircraft.Model, Value);
                  
                  Aircraft.Valid := True;
                            
               elsif Key = "WING_AREA" then
                  
                  Aircraft.Wing_Area := Float'Value (Value);
                            
               elsif Key = "ASPECT_RATIO" then
                  
                  Aircraft.Aspect_Ratio := Float'Value (Value);
                            
               elsif Key = "MASS_EMPTY" then
                  
                  Aircraft.Empty_Mass := Float'Value (Value);
                  
               elsif Key = "MASS_MAX" then
                  
                  Aircraft.Maximum_Mass := Float'Value (Value);
                    
               elsif Key = "VWL" then
                  
                  Aircraft.V_WL := Float'Value (Value);
                  
               elsif Key = "VNE" then
                  
                  Aircraft.V_NE := Float'Value (Value);
                  
               elsif Key = "VNO" then
                  
                  Aircraft.V_NO := Float'Value (Value);
                  
               elsif Key = "MASS_POINT" then
                  
                  if Aircraft.Mass_Count < Mass_Point_Range'Range_Length then
                     
                     Aircraft.Mass_Count := Aircraft.Mass_Count + 1;
                  
                     Aircraft.Mass_Points (Mass_Index).Active := True;
                  
                     Value_Reader.Load (Value);
                  
                     Override (Aircraft.Mass_Points (Mass_Index).Label, Value_Reader.Read_Next ('@'));
                  
                     Aircraft.Mass_Points (Mass_Index).Mass     := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     Aircraft.Mass_Points (Mass_Index).Mass_Min := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     Aircraft.Mass_Points (Mass_Index).Mass_Max := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     Aircraft.Mass_Points (Mass_Index).Position := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     if Mass_Index < Mass_Point_Range'Last then
                  
                        Mass_Index := Mass_Index + 1;
                  
                     end if;
                     
                  end if;
                  
               elsif Key = "CLEAN" then
                  
                  Value_Reader.Load (Value);
                  
                  Aircraft.L0 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.L1 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.L2 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.L3 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.L4 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                       
               elsif Key = "ROUGH" then
                             
                  Value_Reader.Load (Value);
                    
                  Aircraft.T0 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.T1 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.T2 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.T3 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  Aircraft.T4 := Float'Value (Trim (Value_Reader.Read_Next (',')));
                  
               elsif Key = "CL_MIN" then
                               
                  Aircraft.Cl_Min := Float'Value (Value);
  
               elsif Key = "CL_MAX" then
                               
                  Aircraft.Cl_Max := Float'Value (Value);
  
               end if;
               
            exception
               when others =>
                  Ada.Text_IO.Put_Line ("error: could not parse key " & Key & " with " & Value);
            end;
                                   
         end loop;
         
         Close (File_Id);
         
      end if;
      
   end Load_Aircraft_Data;
   -----------------------------------------------------------------------------
    

end Aircraft;
--------------------------------------------------------------------------------
