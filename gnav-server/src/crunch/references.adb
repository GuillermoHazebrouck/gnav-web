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
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_Io;
with Ada.Streams;
with Ada.Streams.Stream_IO;
-- Gnav
with Utility;
use  Utility;
with Utility.Log;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body References is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of airfields
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   A : Natural := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of references
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   R : Natural := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The number of frequencies
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   F : Natural := 0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The area bounds
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   N, E, S, W : Long_Float := 0.0;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The minimum populatio for a citiy to be included
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Minimum_Population : Natural := 10000;

   --===========================================================================
   -- Specific function for airspace position data (Eg. 50:51:30N 003:11:10E)
   --===========================================================================
   function Sexagecimal_Value (Data : String) return Long_Float is

      Sign   : Long_Float := 1.0;
      Result : Long_Float := 0.0;
      Image  : String := Trim (Data);
      F      : constant Natural := Image'First - 1;

   begin

      if Image'Length > 0 then

         case Image (Image'Last) is

            when 'N' | 'S' =>

               if Image'Length > 8 then

                  Result := Long_Float'Value (Image (F + 1..F + 2)) +
                            Long_Float'Value (Image (F + 4..F + 5)) / 60.0 +
                            Long_Float'Value (Image (F + 7..F + 8)) / 3600.0;

                  if Image (Image'Last) = 'S' then
                     Sign := -1.0;
                  end if;

                  if not (Result in 0.0..90.0) then
                     Ada.Text_IO.Put_Line ("warning: wrong lat value " & Image);
                  end if;

               else
                  Ada.Text_IO.Put_Line ("warning: wrong lat format " & Image);

               end if;

            when 'E' | 'O' =>

               if Image'Length > 9 then

                  Result := Long_Float'Value (Image (F + 1..F + 3)) +
                            Long_Float'Value (Image (F + 5..F + 6)) / 60.0 +
                            Long_Float'Value (Image (F + 8..F + 9)) / 3600.0;

                  if Image (Image'Last) = 'O' then
                     Sign := -1.0;
                  end if;

                  if not (Result in 0.0..180.0) then
                     Ada.Text_IO.Put_Line ("warning: wrong lat value " & Image);
                  end if;

               else
                  Ada.Text_IO.Put_Line ("warning: wrong lat format " & Image);

               end if;

            when others =>

               Ada.Text_IO.Put_Line ("warning: unexpected cardinal direction found " & Image (Image'Last) & " in " & Image);

         end case;

         return Sign * Result;

      else

         Ada.Text_IO.Put_Line ("warning: invalid lat/long");
         return 0.0;

      end if;

   end Sexagecimal_Value;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Setups the bounds using the command line options
   --===========================================================================
   procedure Setup_Configuration is
   begin

      -- Read command line arguments
      --------------------------------------------------------------------------
      for I in 1..Ada.Command_Line.Argument_Count loop

         declare
            Argument : String_Buffer (100);
         begin

            Argument.Load (Ada.Command_Line.Argument (I));

            declare
               Key : String := Argument.Read_Next ('=');
               Val : String := Argument.Read_Next ('=');
            begin

               if    Key = "N" then

                  N := Sexagecimal_Value (Val);

               elsif Key = "E" then

                  E := Sexagecimal_Value (Val);

               elsif Key = "S" then

                  S := Sexagecimal_Value (Val);

               elsif Key = "W" then

                  W := Sexagecimal_Value (Val);

               elsif Key = "MIN_POPULATION" then

                  Minimum_Population := Natural'Value (Val);

               end if;

            end;

         end;

      end loop;

   end Setup_Configuration;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads all of the airfields within the local region
   --===========================================================================
   procedure Load_Airfields is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Name : String := "airports.csv";
      File_Id   : File_Type;
      Reader    : Utility.String_Buffer (500);

      --========================================================================
      --
      --========================================================================
      function Remove_String_Tokens (Value : String) return String is
         I : Natural := Value'First;
         J : Natural := Value'Last;
      begin

         if Value (I) = '"' then
            I := I + 1;
         end if;
         if Value (J) = '"' then
            J := J - 1;
         end if;

         if J >= I then
            return Value (I..J);
         else
            return "";
         end if;

      end;
      --------------------------------------------------------------------------

   begin

      Ada.Text_IO.Put_Line ("loading airfields");

      A := 0;

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         if not Is_Open (File_Id) then
            Ada.Text_IO.Put_Line ("warning: could not open file");
            return;
         end if;

         while A < Airfields'Last loop

            exit when End_Of_File (File_Id);

            Reader.Load (Trim (Get_Line (File_Id)));

            declare
               Id       : String := Remove_String_Tokens (Trim (Reader.Read_Next (',')));
               Ident    : String := Remove_String_Tokens (Trim (Reader.Read_Next (',')));
               Kind     : String := Remove_String_Tokens (Trim (Reader.Read_Next (',')));
               Name     : String := Remove_String_Tokens (Trim (Reader.Read_Next (',')));
               Lat      : String := Remove_String_Tokens (Trim (Reader.Read_Next (',')));
               Lon      : String := Remove_String_Tokens (Trim (Reader.Read_Next (',')));
               Position : Position_Record;
            begin

               if
                 Kind = "small_airport"  or else
                 Kind = "medium_airport" or else
                 Kind = "large_airport"
               then

                  begin

                     Position := (Lat => Long_Float'Value (Lat),
                                  Lon => Long_Float'Value (Lon));

                     if
                       Position.Lat > S and then
                       Position.Lon > W and then
                       Position.Lat < N and then
                       Position.Lon < E
                     then

                        A := A + 1;

                        Override (Airfields (A).Id, Utility.Get_Upper_Case (Ident (Ident'First..Ident'First+3)));

                        for C of Airfields (A).Id loop
                           if C = '-' then
                              Airfields (A).Id := "-AF-"; -- (unknown airfield)
                              exit;
                           end if;
                        end loop;

                        Override (Airfields (A).Name, Utility.Get_Upper_Case (Trim (Name)));

                        Airfields (A).Position := Position;

                        Airfields (A).Is_Loaded := True;

                     end if;

                  exception
                     when others =>
                        Ada.Text_IO.Put_Line ("warning: wrong position " & Lat & " " & Lon & " for " & Id);
                  end;

               end if;

            end;

         end loop;

         Close (File_Id);

      end if;

      Ada.Text_IO.Put_Line ("done");

   end Load_Airfields;
   -----------------------------------------------------------------------------




   --===========================================================================
   --
   --===========================================================================
   procedure Correlate_Airfield_Radio_Stations is
   begin

      for I in 1..F loop

         if
           Radio_Stations (I).Id /= No_Reference_Id and then
           Radio_Stations (I).Frequency /= No_Frequency
         then

            for J in 1..A loop

               if Airfields (J).Id = Radio_Stations (I).Id then

                  Radio_Stations (I).Airfield := Short_Natural (J);

                  if Airfields (J).Name = No_Reference_Name then
                     Airfields (J).Name := Radio_Stations (I).Name;
                  end if;

                  for Station of Airfields (J).Stations loop
                     if Station = 0 then
                        Station := Short_Natural (I);
                        exit;
                     end if;
                  end loop;

                  exit;

               end if;

            end loop;

         end if;

      end loop;

   end Correlate_Airfield_Radio_Stations;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Loads all of the cities within the local region
   --===========================================================================
   procedure Load_Cities is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Name  : String := "cities.csv";
      File_Id    : File_Type;
      Reader     : Utility.String_Buffer (2000);
      Data       : Utility.String_Buffer (50);
      Index_Name : constant Positive := 3;
      Index_Pos  : constant Positive := 20;
      Index_Pop  : constant Positive := 14;
      Position   : Position_Record;
      Name       : Reference_Name;
      Population : Natural;
      C          : Natural := 0;

   begin

      Ada.Text_IO.Put_Line ("loading cities");

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         if not Is_Open (File_Id) then
            Ada.Text_IO.Put_Line ("warning: could not open file");
            return;
         end if;

         while R < References'Last loop

            exit when End_Of_File (File_Id);

            declare
               Line : String := Trim (Get_Line (File_Id));
            begin

               Reader.Load (Line);

               Position   := No_Position_Record;
               Population := 0;
               Name       := (others => ' ');

               for I in 1..21 loop

                  declare
                     Value : String := Trim (Reader.Read_Next (';'));
                  begin

                     if Value'Length > 0 then

                        case I is
                           when Index_Name =>
                              Override (Name, Utility.Get_Upper_Case (Value));
                           when Index_Pop =>
                              Population := Natural'Value (Value);
                           when Index_Pos =>
                              Data.Load (Value);
                              Position.Lat := Long_Float'Value (Trim (Data.Read_Next (',')));
                              Position.Lon := Long_Float'Value (Trim (Data.Read_Next (ASCII.CR)));
                           when others =>
                              null;
                        end case;

                     end if;

                  end;

               end loop;

               ----------------------------------------------------------------------------
               -- Template:
               --      1;          2;          3;          4;5;  6; 7;      8;9; 10; 11;12;
               --2798700;Erembodegem;Erembodegem;Erembodegem;P;PPL;BE;Belgium; ;VLG;VOV;41;
               --   13;   14;15;16;             17;        18;     19;      20
               --41002;12152;  ;15;Europe/Brussels;2020-04-05;Belgium;50.91905, 4.05041
               ----------------------------------------------------------------------------
               if
                 Population   > Minimum_Population and then
                 Position.Lat > S and then
                 Position.Lon > W and then
                 Position.Lat < N and then
                 Position.Lon < E
               then

                  R := R + 1;
                  C := C + 1;

                  if Population > 35000 then
                     References (R).Kind := Reference_City;
                  else
                     References (R).Kind := Reference_Village;
                  end if;

                  Override (References (R).Id, Name);

                  References (R).Name := Name;

                  References (R).Position := Position;

                  References (R).Is_Loaded := True;

               end if;

            exception
               when E : others =>
                  Ada.Text_IO.Put_Line ("warning: error while reading city " & Name & " ->" & Ada.Exceptions.Exception_Message (E));
            end;

         end loop;

         if Is_Open (File_Id) then
            Close (File_Id);
         end if;

         Ada.Text_IO.Put_Line ("found " & Integer_Image (C) & " cities in the region");

      end if;

   end Load_Cities;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See referencec file)
   --===========================================================================
   procedure Load_Frequencies is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Name : String := "radio.dat";
      File_Id   : File_Type;
      Reader    : Utility.String_Buffer (50);

   begin

      Ada.Text_IO.Put_Line ("loading radio stations");

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         if not Is_Open (File_Id) then
            Ada.Text_IO.Put_Line ("warning: could not open file");
            return;
         end if;

         while F < Radio_Stations'Last loop

            exit when End_Of_File (File_Id);

            declare
               Line : String := Trim (Get_Line (File_Id));
            begin

               if Line'Length > 0 then

                  if Line (Line'First) /= '#' then

                     Reader.Load (Line);

                     F := F + 1;

                     Override (Radio_Stations (F).Name,      Trim (Reader.Read_Next ('@')));

                     Override (Radio_Stations (F).Frequency, Trim (Reader.Read_Next ('@')));

                     Override (Radio_Stations (F).Id,        Trim (Reader.Read_Next ('@')));

                  end if;

               end if;

            end;

         end loop;

         Close (File_Id);

      end if;

   end Load_Frequencies;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See referencec file)
   --===========================================================================
   procedure Load_References is

      use Ada.Directories;
      use Ada.Text_IO;

      File_Name : String := "reference.dat";
      File_Id   : File_Type;
      Reader    : Utility.String_Buffer (200);
      Kind      : Reference_Types := Reference_City;
      C         : Natural := 0;

   begin

      Ada.Text_IO.Put_Line ("loading custom reference points");

      if Ada.Directories.Exists (File_Name) then

         -- Start reading
         -----------------------------------------------------------------------
         Open (File_Id, In_File, File_Name);

         if not Is_Open (File_Id) then
            Ada.Text_IO.Put_Line ("warning: could not open file");
            return;
         end if;

         while R < References'Last loop

            exit when End_Of_File (File_Id);

            declare
               Line : String := Trim (Get_Line (File_Id));
            begin

               if Line'Length > 0 then

                  if Line (Line'First) = '#' then

                     if Line = "#AIRFIELDS" then

                        Kind := Reference_Airfield;

                     elsif Line = "#LANDMARKS" then

                        Kind := Reference_Landmark;

                     elsif Line = "#WATER" then

                        Kind := Reference_Water;

                     elsif Line = "#CITIES" then

                        Kind := Reference_City;

                     elsif Line = "#VILLAGES" then

                        Kind := Reference_Village;

                     elsif Line = "#WOODS" then

                        Kind := Reference_Woods;

                     elsif Line = "#TURBINES" then

                        Kind := Reference_Turbine;

                     elsif Line = "#LANDOUTS" then

                        Kind := Reference_Landouts;

                     end if;

                  else

                     Reader.Load (Line);

                     R := R + 1;
                     C := C + 1;

                     References (R).Kind := Kind;

                     Override (References (R).Id,   Trim (Reader.Read_Next ('@')));

                     Override (References (R).Name, Trim (Reader.Read_Next ('@')));

                     References (R).Position := Utility.Maps.Value (Trim (Reader.Read_Next ('@')));

                     References (R).Is_Loaded := True;

                  end if;

               end if;

            end;

         end loop;

         Close (File_Id);

         Ada.Text_IO.Put_Line ("found " & Integer_Image (C) & " custom reference points");

      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("while reading reference file");

         Close (File_Id);

   end Load_References;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Saves the chart in binary format
   --===========================================================================
   procedure Save_References_Binary is

      use Ada.Streams.Stream_IO;

      File_Id : File_Type;
      Stream  : Stream_Access;

   begin

      Ada.Text_IO.Put_Line ("writing references");

      if not Ada.Directories.Exists ("reference.bin") then

         Create (File => File_Id,
                 Mode => Out_File,
                 Name => "reference.bin");

      else
         Open  (File => File_Id,
                Mode => Out_File,
                Name => "reference.bin");

      end if;

      if not Is_Open (File_Id) then
         Ada.Text_IO.Put_Line ("warning: could not open file");
         return;
      end if;

      Stream := Ada.Streams.Stream_IO.Stream (File_Id);

      --------------------------------------------------------------------------
      -- Annonimous references
      --------------------------------------------------------------------------

      R := 0;
      for Reference of References loop
         if
           Reference.Is_Loaded and then
           Reference.Id = No_Reference_Id
         then
            R := R + 1;
         end if;
      end loop;

      Ada.Text_IO.Put_Line (Integer_Image (R) & " annonimous references (" & Integer_Image (R * 100 / 750) & "%)");

      Natural'Write (Stream, R); -- 4 bytes

      if R > 0 then

         for Reference of References loop

            if
              Reference.Is_Loaded and then
              Reference.Id = No_Reference_Id
            then

               Character'Write  (Stream, Reference_Kind_Key (Reference.Kind)); -- 1 byte
               Long_Float'Write (Stream, Reference.Position.Lat);              -- 8 bytes
               Long_Float'Write (Stream, Reference.Position.Lon);              -- 8 bytes

            end if;

         end loop;

      end if;

      --------------------------------------------------------------------------
      -- Named references
      --------------------------------------------------------------------------

      R := 0;
      for Reference of References loop
         if
           Reference.Is_Loaded and then
           Reference.Id /= No_Reference_Id
         then
            R := R + 1;
         end if;
      end loop;

      Ada.Text_IO.Put_Line (Integer_Image (R) & " named references (" & Integer_Image (R * 100 / 750) & "%)");

      Natural'Write (Stream, R); -- 4 bytes

      if R > 0 then

         for Reference of References loop

            if
              Reference.Is_Loaded  and then
              Reference.Id /= No_Reference_Id
            then

               Character'Write      (Stream, Reference_Kind_Key (Reference.Kind)); -- 1 byte
               Reference_Id'Write   (Stream, Reference.Id);                        -- 4 bytes
               Reference_Name'Write (Stream, Reference.Name);                      --14 bytes
               Long_Float'Write     (Stream, Reference.Position.Lat);              -- 8 bytes
               Long_Float'Write     (Stream, Reference.Position.Lon);              -- 8 bytes

            end if;

         end loop;

      end if;

      --------------------------------------------------------------------------
      -- Airfields
      --------------------------------------------------------------------------

      Ada.Text_IO.Put_Line (Integer_Image (A) & " airfields (" & Integer_Image (A * 100 / 200) & "%)");

      Natural'Write (Stream, A); -- 4 bytes

      if A > 0 then

         for Airfield of Airfields loop

            if
              Airfield.Is_Loaded and then
              Airfield.Id /= No_Reference_Id
            then

               Reference_Id'Write   (Stream, Airfield.Id);                        -- 4 bytes
               Reference_Name'Write (Stream, Airfield.Name);                      --14 bytes
               Short_Natural'Write  (Stream, Airfield.Stations (1));              -- 2 bytes
               Short_Natural'Write  (Stream, Airfield.Stations (2));              -- 2 bytes
               Short_Natural'Write  (Stream, Airfield.Stations (3));              -- 2 bytes
               Long_Float'Write     (Stream, Airfield.Position.Lat);              -- 8 bytes
               Long_Float'Write     (Stream, Airfield.Position.Lon);              -- 8 bytes

            end if;

         end loop;

      end if;

      --------------------------------------------------------------------------
      -- Frequencies
      --------------------------------------------------------------------------

      Ada.Text_IO.Put_Line (Integer_Image (F) & " frequencies (" & Integer_Image (F * 100 / 200) & "%)");

      Natural'Write (Stream, F); -- 4 bytes

      for I in 1..F loop

         Reference_Name'Write (Stream, Radio_Stations (I).Name);      -- 14 bytes
         Frequency_Type'Write (Stream, Radio_Stations (I).Frequency); --  7 bytes
         Short_Natural'Write  (Stream, Radio_Stations (I).Airfield);  --  2 bytes

      end loop;

      Ada.Text_IO.Put_Line ("done writing references");

      Close (File_Id);

   end Save_References_Binary;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- Saves the chart in binary format
   --===========================================================================
   procedure Save_Binary is
   begin

      Save_References_Binary;

   end Save_Binary;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See referencec file)
   --===========================================================================
   procedure Compile_Data is
   begin
      Utility.Log.Log_On_Files := False;

      -- Clear all
      --------------------------------------------------------------------------

      R := 0;

      References := (others => No_Reference_Record);

      F := 0;

      Radio_Stations := (others => No_Frequency_Record);

      A := 0;

      Airfields := (others => No_Airfield_Record);

      -- Load datasets
      --------------------------------------------------------------------------

      Setup_Configuration;

      Load_Airfields;

      Load_Cities;

      Load_References;

      Load_Frequencies;

      Correlate_Airfield_Radio_Stations;

      -- Write datasets
      --------------------------------------------------------------------------

      Save_Binary;

   end Compile_Data;
   -----------------------------------------------------------------------------

end References;
--------------------------------------------------------------------------------
