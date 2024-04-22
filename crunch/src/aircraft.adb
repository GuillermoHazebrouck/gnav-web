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
   
   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Compile_Data is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;
      Stream  : Stream_Access;

   begin
      
      if Aircraft_Count > 0 then

         Ada.Text_Io.Put_Line ("compiling aircraft" & Natural'Image (Aircraft_Count));

         -- Start writing
         -----------------------------------------------------------------------

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "files/aircraft.bin");

         Stream := Ada.Streams.Stream_IO.Stream (File_Id);

         Natural'Write (Stream, Aircraft_Count); --  4 bytes

         for Aircraft of Aircrafts loop
         
            if Aircraft.Valid then
         
               Ada.Text_Io.Put_Line (Aircraft.Model & "/" & Aircraft.Registration);
               
               -- Basic info
               --------------------------
               Aircraft_Names'Write (Stream, Aircraft.Model);
               Aircraft_Names'Write (Stream, Aircraft.Registration);
               
               -- Characteristics
               --------------------------
               
               Float'Write (Stream, Aircraft.Area);
               Float'Write (Stream, Aircraft.Empty_Mass);
               Float'Write (Stream, Aircraft.Maximum_Mass);
               Float'Write (Stream, Aircraft.V_NE);
               Float'Write (Stream, Aircraft.V_NO);
               
               -- Mass points
               --------------------------
               Ada.Text_Io.Put_Line ("mass points");
               Natural'Write (Stream, Aircraft.Mass_Count); --  4 bytes
               
               for Point of Aircraft.Mass_Points loop
                  if Point.Active then
                     Aircraft_Names'Write (Stream, Point.Label);
                     Float'Write (Stream, Point.Arm);
                     Float'Write (Stream, Point.Mass); -- (default value)
                     Float'Write (Stream, Point.Mass_Max);
                     Float'Write (Stream, Point.Mass_Min);
                     Ada.Text_Io.Put_Line ("*");
                  end if;         
               end loop;
                               
               -- Polar curve
               --------------------------
               Natural'Write (Stream, Aircraft.Polar_Count); --  4 bytes
               
               for P in 1..Aircraft.Polar_Count loop
                  Float'Write (Stream, Aircraft.Polar (P).Cd);
                  Float'Write (Stream, Aircraft.Polar (P).Cl);          
               end loop;
                    
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
            
      File_Name    : constant String := "files/aircraft.dat";

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
                            
               elsif Key = "REGISTRATION" then
                  
            	  Override (Aircraft.Registration, Value);
                            
               elsif Key = "WING_AREA" then
                  
                  Aircraft.Area := Float'Value (Value);
                           
               elsif Key = "EMPTY_MASS" then
                  
                  Aircraft.Empty_Mass := Float'Value (Value);
                  
               elsif Key = "MASS_MAX" then
                  
                  Aircraft.Maximum_Mass := Float'Value (Value);
                    
               elsif Key = "VNE" then
                  
                  Aircraft.V_NE := Float'Value (Value);
                  
               elsif Key = "VNO" then
                  
                  Aircraft.V_NO := Float'Value (Value);
                  
               elsif Key = "MASS_POINT" then
                  
                  Value_Reader.Load (Value);
                  
                  if Mass_Index < Mass_Point_Range'Last then
                  
                     Mass_Index := Mass_Index + 1;
                  
                     Aircraft.Mass_Points (Mass_Index).Active := True;
                  
                     Override (Aircraft.Mass_Points (Mass_Index).Label, Value_Reader.Read_Next ('@'));
                  
                     Aircraft.Mass_Points (Mass_Index).Arm      := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     Aircraft.Mass_Points (Mass_Index).Mass     := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     Aircraft.Mass_Points (Mass_Index).Mass_Min := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     Aircraft.Mass_Points (Mass_Index).Mass_Max := Float'Value (Trim (Value_Reader.Read_Next ('@')));
                  
                     Aircraft.Mass_Count := Aircraft.Mass_Count + 1;
                     
                  end if;
                     
               elsif Key = "POLAR_NAME" then
                  
                  null;
                  --Override (This_Aircraft.Polar_Name, Value);
                  
               elsif Key = "POLAR_POINT" then
                  
                  if Aircraft.Polar_Count < Aircraft.Polar'Last then
                     
                     Aircraft.Polar_Count := Aircraft.Polar_Count + 1;
                     
                     Value_Reader.Load (Value);
                  
                     Aircraft.Polar (Aircraft.Polar_Count).Cl := Float'Value (Trim (Value_Reader.Read_Next (';')));
                     
                     Aircraft.Polar (Aircraft.Polar_Count).Cd := Float'Value (Trim (Value_Reader.Read_Next (';')));
                     
                  end if;
                  
               end if;
               
            end;
                                   
         end loop;
         
         Close (File_Id);
         
      end if;
      
   end Load_Aircraft_Data;
   -----------------------------------------------------------------------------
    

end Aircraft;
--------------------------------------------------------------------------------
