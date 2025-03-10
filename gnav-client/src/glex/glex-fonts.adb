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
with Glex;
with Glex.Lines;
use  Glex.Lines;

--//////////////////////////////////////////////////////////////////////////////
--
--//////////////////////////////////////////////////////////////////////////////
package body Glex.Fonts is

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- A list of all possible font data
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   type Character_Resource_Array is array (Character) of Resource_Type;

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The buffer used to create the font lines
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Buffer : Glex.Lines.Buffer_Type := Glex.Lines.New_Buffer (15);

   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- The list of all font data items
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Resources : Character_Resource_Array;

   --===========================================================================
   -- Returns the lines buffer for an specific character
   --===========================================================================
   procedure Load_Character (C : Character) is
   begin

      Buffer.Reset;

      case C is

         when 'A' =>

            Buffer.Move_To (0.00, 0.00);
            Buffer.Line_To (0.50, 1.00);
            Buffer.Line_To (1.00, 0.00);

            Buffer.Move_To (0.15, 0.30);
            Buffer.Line_To (0.85, 0.30);

         when 'B' =>

            Buffer.Move_To (0.00, 0.00);
            Buffer.Line_To (0.00, 1.00);
            Buffer.Line_To (0.80, 1.00);
            Buffer.Line_To (1.00, 0.80);
            Buffer.Line_To (1.00, 0.70);
            Buffer.Line_To (0.80, 0.50);
            Buffer.Line_To (1.00, 0.30);
            Buffer.Line_To (1.00, 0.10);
            Buffer.Line_To (0.80, 0.00);
            Buffer.Line_To (0.00, 0.00);

            Buffer.Move_To (0.80, 0.50);
            Buffer.Line_To (0.00, 0.50);

         when 'C' =>

            Buffer.Move_To (1.00, 0.20);
            Buffer.Line_To (0.80, 0.00);
            Buffer.Line_To (0.20, 0.00);
            Buffer.Line_To (0.00, 0.20);
            Buffer.Line_To (0.00, 0.80);
            Buffer.Line_To (0.20, 1.00);
            Buffer.Line_To (0.80, 1.00);
            Buffer.Line_To (1.00, 0.80);

         when 'D' =>

            Buffer.Move_To (0.00, 0.00);
            Buffer.Line_To (0.00, 1.00);
            Buffer.Line_To (0.70, 1.00);
            Buffer.Line_To (1.00, 0.70);
            Buffer.Line_To (1.00, 0.30);
            Buffer.Line_To (0.70, 0.00);
            Buffer.Line_To (0.00, 0.00);

         when 'E' =>

            Buffer.Move_To (1.00, 0.00);
            Buffer.Line_To (0.00, 0.00);
            Buffer.Line_To (0.00, 1.00);
            Buffer.Line_To (1.00, 1.0);

            Buffer.Move_To (0.00, 0.50);
            Buffer.Line_To (0.60, 0.50);

         when 'F' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (0.0, 1.0);
            Buffer.Line_To (1.0, 1.0);

            Buffer.Move_To (0.0, 0.5);
            Buffer.Line_To (0.6, 0.5);

         when 'G' =>

            Buffer.Move_To (0.5, 0.5);
            Buffer.Line_To (1.0, 0.5);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.0, 0.2);
            Buffer.Line_To (0.0, 0.8);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.8);

         when 'H' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (0.0, 1.0);

            Buffer.Move_To (1.0, 0.0);
            Buffer.Line_To (1.0, 1.0);

            Buffer.Move_To (0.0, 0.5);
            Buffer.Line_To (1.0, 0.5);

         when 'I' =>

            Buffer.Move_To (0.2, 0.0);
            Buffer.Line_To (0.8, 0.0);

            Buffer.Move_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 1.0);

         when 'J' =>

            Buffer.Move_To (0.5, 1.0);
            Buffer.Line_To (1.0, 1.0);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.0, 0.2);

         when 'K' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (0.0, 1.0);

            Buffer.Move_To (0.0, 0.5);
            Buffer.Line_To (1.0, 1.0);

            Buffer.Move_To (0.0, 0.5);
            Buffer.Line_To (1.0, 0.0);

         when 'L' =>

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (0.0, 0.0);
            Buffer.Line_To (0.8, 0.0);

         when 'M' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (0.0, 1.0);
            Buffer.Line_To (0.5, 0.5);
            Buffer.Line_To (1.0, 1.0);
            Buffer.Line_To (1.0, 0.0);

         when 'N' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (0.0, 1.0);
            Buffer.Line_To (1.0, 0.0);
            Buffer.Line_To (1.0, 1.0);

         when 'O' =>

            Buffer.Move_To (0.0, 0.2);
            Buffer.Line_To (0.0, 0.8);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.8);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.0, 0.2);

         when 'P' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (0.0, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.8);
            Buffer.Line_To (1.0, 0.7);
            Buffer.Line_To (0.8, 0.5);
            Buffer.Line_To (0.0, 0.5);

         when 'Q' =>

            Buffer.Move_To (0.0, 0.2);
            Buffer.Line_To (0.0, 0.8);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.8);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.0, 0.2);

            Buffer.Move_To (0.6, 0.4);
            Buffer.Line_To (1.0, 0.0);

         when 'R' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (0.0, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.8);
            Buffer.Line_To (1.0, 0.7);
            Buffer.Line_To (0.8, 0.5);
            Buffer.Line_To (0.0, 0.5);

            Buffer.Move_To (0.3, 0.5);
            Buffer.Line_To (1.0, 0.0);

         when 'S' =>

            Buffer.Move_To (0.0, 0.1);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (1.0, 0.1);
            Buffer.Line_To (1.0, 0.3);
            Buffer.Line_To (0.0, 0.7);
            Buffer.Line_To (0.0, 0.9);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.9);

         when 'T' =>

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 1.0);

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (1.0, 1.0);

         when 'U' =>

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (0.0, 0.2);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (1.0, 1.0);

         when 'V' =>

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (0.5, 0.0);
            Buffer.Line_To (1.0, 1.0);

         when 'W' =>

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (0.1, 0.0);
            Buffer.Line_To (0.5, 0.6);
            Buffer.Line_To (0.9, 0.0);
            Buffer.Line_To (1.0, 1.0);

         when 'X' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (1.0, 1.0);

            Buffer.Move_To (1.0, 0.0);
            Buffer.Line_To (0.0, 1.0);

         when 'Y' =>

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (0.5, 0.5);

            Buffer.Move_To (1.0, 1.0);
            Buffer.Line_To (0.5, 0.5);

            Buffer.Move_To (0.5, 0.5);
            Buffer.Line_To (0.5, 0.0);

         when 'Z' =>

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (1.0, 1.0);
            Buffer.Line_To (0.0, 0.0);
            Buffer.Line_To (1.0, 0.0);

         when '0' =>

            Buffer.Move_To (0.0, 0.2);
            Buffer.Line_To (0.0, 0.8);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.8);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.0, 0.2);

            Buffer.Move_To (0.3, 0.3);
            Buffer.Line_To (0.7, 0.7);

         when '1' =>

            Buffer.Move_To (0.2, 0.0);
            Buffer.Line_To (0.8, 0.0);

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 1.0);
            Buffer.Line_To (0.3, 0.7);

         when '2' =>

            Buffer.Move_To (1.0, 0.0);
            Buffer.Line_To (0.0, 0.0);
            Buffer.Line_To (0.0, 0.2);
            Buffer.Line_To (0.8, 0.6);
            Buffer.Line_To (1.0, 0.7);
            Buffer.Line_To (1.0, 0.9);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.0, 0.9);

         when '3' =>

            Buffer.Move_To (0.0, 0.9);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.9);
            Buffer.Line_To (1.0, 0.6);
            Buffer.Line_To (0.8, 0.5);
            Buffer.Line_To (1.0, 0.4);
            Buffer.Line_To (1.0, 0.1);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.0, 0.1);

            Buffer.Move_To (0.8, 0.5);
            Buffer.Line_To (0.5, 0.5);

         when '4' =>

            Buffer.Move_To (0.8, 0.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (0.0, 0.4);
            Buffer.Line_To (1.0, 0.4);

         when '5' =>

            Buffer.Move_To (1.0, 1.0);
            Buffer.Line_To (0.0, 1.0);
            Buffer.Line_To (0.0, 0.6);
            Buffer.Line_To (0.8, 0.6);
            Buffer.Line_To (1.0, 0.4);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.0, 0.0);

         when '6' =>

            Buffer.Move_To (0.9, 1.0);
            Buffer.Line_To (0.4, 1.0);
            Buffer.Line_To (0.0, 0.6);
            Buffer.Line_To (0.0, 0.2);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (1.0, 0.2);
            Buffer.Line_To (1.0, 0.5);
            Buffer.Line_To (0.8, 0.6);
            Buffer.Line_To (0.2, 0.6);
            Buffer.Line_To (0.0, 0.5);

         when '7' =>

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (1.0, 1.0);
            Buffer.Line_To (0.0, 1.0);

            Resources (C).Load (Buffer);

         when '8' =>

            Buffer.Move_To (0.0, 0.1);
            Buffer.Line_To (0.0, 0.3);
            Buffer.Line_To (0.2, 0.5);
            Buffer.Line_To (0.0, 0.7);
            Buffer.Line_To (0.0, 0.9);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (1.0, 0.9);
            Buffer.Line_To (1.0, 0.7);
            Buffer.Line_To (0.8, 0.5);
            Buffer.Line_To (1.0, 0.3);
            Buffer.Line_To (1.0, 0.1);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (0.2, 0.0);
            Buffer.Line_To (0.0, 0.1);

            Buffer.Move_To (0.2, 0.5);
            Buffer.Line_To (0.8, 0.5);

         when '9' =>

            Buffer.Move_To (0.1, 0.0);
            Buffer.Line_To (0.8, 0.0);
            Buffer.Line_To (1.0, 0.4);
            Buffer.Line_To (1.0, 0.8);
            Buffer.Line_To (0.8, 1.0);
            Buffer.Line_To (0.2, 1.0);
            Buffer.Line_To (0.0, 0.8);
            Buffer.Line_To (0.0, 0.5);
            Buffer.Line_To (0.2, 0.4);
            Buffer.Line_To (0.8, 0.4);
            Buffer.Line_To (1.0, 0.5);

         when '=' =>

            Buffer.Move_To (0.0, 0.3);
            Buffer.Line_To (1.0, 0.3);

            Buffer.Move_To (0.0, 0.7);
            Buffer.Line_To (1.0, 0.7);

         when ':' =>

            Buffer.Move_To (0.4, 0.2);
            Buffer.Line_To (0.5, 0.2);

            Buffer.Move_To (0.4, 0.7);
            Buffer.Line_To (0.5, 0.7);

         when '+' =>

            Buffer.Move_To (0.5, 0.1);
            Buffer.Line_To (0.5, 0.9);

            Buffer.Move_To (0.1, 0.5);
            Buffer.Line_To (0.9, 0.5);

         when '-' =>

            Buffer.Move_To (0.1, 0.5);
            Buffer.Line_To (0.9, 0.5);

         when '_' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (1.0, 0.0);

         when '.' =>

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 0.1);

         when ',' =>

            Buffer.Move_To (0.6, 0.1);
            Buffer.Line_To (0.4,-0.1);

         when '*' | '°' =>

            Buffer.Move_To (0.3, 1.0);
            Buffer.Line_To (0.7, 1.0);
            Buffer.Line_To (0.7, 0.6);
            Buffer.Line_To (0.3, 0.6);
            Buffer.Line_To (0.3, 1.0);

         when ''' =>

            Buffer.Move_To (0.5, 1.0);
            Buffer.Line_To (0.5, 0.7);

         when '"' =>

            Buffer.Move_To (0.3, 1.0);
            Buffer.Line_To (0.3, 0.7);

            Buffer.Move_To (0.7, 1.0);
            Buffer.Line_To (0.7, 0.7);

         when '/' =>

            Buffer.Move_To (0.0, 0.0);
            Buffer.Line_To (1.0, 1.0);

         when '\' =>

            Buffer.Move_To (0.0, 1.0);
            Buffer.Line_To (1.0, 0.0);

         when '|' =>

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 1.0);

         when '!' =>

            Buffer.Move_To (0.5, 0.2);
            Buffer.Line_To (0.5, 1.0);

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 0.1);

         when '~' =>

            Buffer.Move_To (0.00, 0.40);
            Buffer.Line_To (0.25, 0.60);
            Buffer.Line_To (0.75, 0.40);
            Buffer.Line_To (1.00, 0.60);

         when '>' =>

            Buffer.Move_To (0.0, 0.5);
            Buffer.Line_To (1.0, 0.5);

            Buffer.Move_To (0.5, 0.8);
            Buffer.Line_To (1.0, 0.5);
            Buffer.Line_To (0.5, 0.2);

         when '<' =>

            Buffer.Move_To (0.0, 0.5);
            Buffer.Line_To (1.0, 0.5);

            Buffer.Move_To (0.5, 0.8);
            Buffer.Line_To (0.0, 0.5);
            Buffer.Line_To (0.5, 0.2);

         when '}' =>

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 1.0);

            Buffer.Move_To (0.2, 0.5);
            Buffer.Line_To (0.5, 1.0);
            Buffer.Line_To (0.8, 0.5);

         when '{' =>

            Buffer.Move_To (0.5, 0.0);
            Buffer.Line_To (0.5, 1.0);

            Buffer.Move_To (0.2, 0.5);
            Buffer.Line_To (0.5, 0.0);
            Buffer.Line_To (0.8, 0.5);

         when others =>

            Buffer.Move_To (0.0, 0.5);
            Buffer.Line_To (1.0, 0.5);

      end case;

      Buffer.Finish;
      Resources (C).Load (Buffer);

   end Load_Character;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Initialize is

      subtype Upper_Case_Range is Character range 'A'..'Z';

      subtype Lower_Case_Range is Character range 'a'..'z';

      subtype Number_Range is Character range '0'..'9';

   begin

      -- Upper case
      ------------------------------
      for C in Upper_Case_Range loop

         Load_Character (C);

      end loop;

      -- Lower case
      ------------------------------
      for C in Lower_Case_Range loop

         Load_Character (C);

      end loop;

      -- Numbers
      ------------------------------
      for C in Number_Range loop

         Load_Character (C);

      end loop;

      -- Other symbols
      ------------------------------

      Load_Character ('=');
      Load_Character ('+');
      Load_Character ('-');
      Load_Character ('_');
      Load_Character ('.');
      Load_Character (',');
      Load_Character (':');
      Load_Character ('*');
      Load_Character ('°');
      Load_Character (''');
      Load_Character ('"');
      Load_Character ('/');
      Load_Character ('\');
      Load_Character ('|');
      Load_Character ('!');
      Load_Character ('~');
      Load_Character ('>'); -- arrow right
      Load_Character ('<'); -- arrow left
      Load_Character ('}'); -- arrow up
      Load_Character ('{'); -- arrow down

   end Initialize;
   -----------------------------------------------------------------------------




   --===========================================================================
   -- (See specification file)
   --===========================================================================
   procedure Draw (Text      : String;
                   X, Y      : Float;
                   Width     : Font_Float;
                   Height    : Font_Float;
                   Space     : Font_Float;
                   Color     : Color_Record;
                   Thickness : Float;
                   Alignment : Font_Alignment_Types := Alignment_LL) is

      use Glex;

      T      : Transform_Record := Glex.Transform;
      H      : Float   := X;
      V      : Float   := Y;
      W      : Float   := 0.0;
      D      : Float   := Thickness / Float'Min (Width, Height) * (Width + Height);
      A      : Float   := Aspect * Width / Height;
      Length : Float   := Float (Text'Length);

   begin

      if Text'Length = 0 or Width <= 0.0 or Height <= 0.0 then

         return;

      end if;

      case Alignment is

         when Alignment_LC =>

            H := H - 0.5 * (Width * Length + Space * (Length - 1.0));

         when Alignment_LR =>

            H := H - Width * Length - Space * (Length - 1.0);

         when Alignment_TL =>

            V := V - Height;

         when Alignment_TC =>

            H := H - 0.5 * (Width * Length + Space * (Length - 1.0));

            V := V - Height;

         when Alignment_TR =>

            H := H - Width * Length - Space * (Length - 1.0);

            V := V - Height;

         when Alignment_CC =>

            H := H - 0.5 * (Width * Length + Space * (Length - 1.0));

            V := V - 0.5 * Height;

         when Alignment_CR =>

            H := H - Width * Length - Space * (Length - 1.0);

            V := V - 0.5 * Height;

         when Alignment_CL =>

            V := V - 0.5 * Height;

         when others =>

            null;

      end case;

      W := Width + Space;

      for C of Text loop

         Glex.Transform.Copy (T);

         Glex.Transform.Translate (H, V);

         Glex.Transform.Scale (Width, Height);

         Resources (C).Draw (Color, D, A);

         H := H + W;

      end loop;

      Glex.Transform.Copy (T);

   end Draw;
   -----------------------------------------------------------------------------



   --===========================================================================
   -- Draws the given text
   -- X, Y: the position using the current matrix transformation
   --===========================================================================
   procedure Draw (Text      : String;
                   X, Y      : Float;
                   Style     : Font_Style_Record;
                   Color     : Line_Color_Record;
                   Alignment : Font_Alignment_Types := Alignment_LL) is

      Thickness : Float;

   begin

      case Style.Rendering is

         when Font_Simple =>

            case Style.Thickness is

               when Font_Thin =>

                  Thickness := 0.03;

               when Font_Regular =>

                  Thickness := 0.04;

               when Font_Bold =>

                  Thickness := 0.06;

            end case;

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Color.Fore, Thickness, Alignment);

         when Font_Glow =>

            -- Background
            -----------------------------------

            case Style.Thickness is

               when Font_Thin =>

                  Thickness := 0.040;

               when Font_Regular =>

                  Thickness := 0.060;

               when Font_Bold =>

                  Thickness := 0.080;

            end case;

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Color.Glow, Thickness, Alignment);

            -- Foreground
            -----------------------------------

            case Style.Thickness is

               when Font_Thin =>

                  Thickness := 0.020;

               when Font_Regular =>

                  Thickness := 0.030;

               when Font_Bold =>

                  Thickness := 0.040;

            end case;

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Color.Fore, Thickness, Alignment);

         when Font_Extra_Glow =>

            -- Background
            -----------------------------------

            case Style.Thickness is

               when Font_Thin =>

                  Thickness := 0.060;

               when Font_Regular =>

                  Thickness := 0.080;

               when Font_Bold =>

                  Thickness := 0.100;

            end case;

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Color.Glow, Thickness, Alignment);

            -- Foreground
            -----------------------------------

            case Style.Thickness is

               when Font_Thin =>

                  Thickness := 0.020;

               when Font_Regular =>

                  Thickness := 0.030;

               when Font_Bold =>

                  Thickness := 0.040;

            end case;

            Draw (Text, X, Y, Style.Width, Style.Height, Style.Space, Color.Fore, Thickness, Alignment);

      end case;

   end Draw;
   -----------------------------------------------------------------------------



end Glex.Fonts;
--------------------------------------------------------------------------------
