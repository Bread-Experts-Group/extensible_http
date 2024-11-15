with Ada.Containers.Ordered_Maps;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package body Extensible_HTTP.Base64 is

   type Base_64_Alphabet_Array is
     array (Unsigned_32 range 0 .. 63)
     of Character;

   Base_64_Alphabet : Base_64_Alphabet_Array :=
     ['A',
     'B',
     'C',
     'D',
     'E',
     'F',
     'G',
     'H',
     'I',
     'J',
     'K',
     'L',
     'M',
     'N',
     'O',
     'P',
     'Q',
     'R',
     'S',
     'T',
     'U',
     'V',
     'W',
     'X',
     'Y',
     'Z',
     'a',
     'b',
     'c',
     'd',
     'e',
     'f',
     'g',
     'h',
     'i',
     'j',
     'k',
     'l',
     'm',
     'n',
     'o',
     'p',
     'q',
     'r',
     's',
     't',
     'u',
     'v',
     'w',
     'x',
     'y',
     'z',
     '0',
     '1',
     '2',
     '3',
     '4',
     '5',
     '6',
     '7',
     '8',
     '9',
     '+',
     '/'];

   package Base_64_Reverse_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Character, Element_Type => Unsigned_32);
   Reverse_Map_Ready   : Boolean := False;
   Base_64_Reverse_Map : Base_64_Reverse_Maps.Map;

   function Extract_Bits (Data : Unsigned_32; From, To : Natural) return Unsigned_32 is

      Mask : Unsigned_32 := Shift_Left (Shift_Right (Unsigned_32'Last, Unsigned_32'Size - (To - From + 1)), From);

   begin
      return Data and Mask;
   end Extract_Bits;

      -------------------
      -- Encode_Base64 --
      -------------------

   function Encode_Base64 (Data : String) return String is

      Position      : Positive := Data'First;
      Group_Literal : String (1 .. 3);
      To_Write      : Unbounded_String;

   begin
      loop
         Ada.Strings.Fixed.Move (Data (Position .. Data'Last), Group_Literal, Ada.Strings.Right, Pad => ASCII.NUL);

         declare

            First  : Unsigned_32 := Character'Pos (Group_Literal (1));
            Second : Unsigned_32 := Character'Pos (Group_Literal (2));
            Third  : Unsigned_32 := Character'Pos (Group_Literal (3));

         begin
            To_Write.Append (Base_64_Alphabet (Shift_Right (First, 2)));
            To_Write.Append (Base_64_Alphabet (Shift_Left (First and 2#11#, 4) or Shift_Right (Second and 2#1111_0000#, 4)));
            To_Write.Append (if Second = 0 then '=' else Base_64_Alphabet (Shift_Right (Third and 2#1100_0000#, 6) or Shift_Left (Second and 2#0000_1111#, 2)));
            To_Write.Append (if Third = 0 then '=' else Base_64_Alphabet (Third and 2#0011_1111#));
         end;
         Position := @ + 3;
         exit when Position > Data'Length;
      end loop;
      return To_Write.To_String;
   end Encode_Base64;

      -------------------
      -- Decode_Base64 --
      -------------------

   function Decode_Base64 (Data : String) return String is

      Position      : Positive := Data'First;
      Group_Literal : String (1 .. 4);
      To_Write      : Unbounded_String;

   begin
      if not Reverse_Map_Ready
      then
         for I in Base_64_Alphabet'Range
         loop
            Base_64_Reverse_Map.Include (Base_64_Alphabet (I), I);
         end loop;
         Reverse_Map_Ready := True;
      end if;

      loop
         Ada.Strings.Fixed.Move (Data (Position .. Data'Last), Group_Literal, Ada.Strings.Right, Pad => '=');

         declare

            First  : Unsigned_32 := (if Group_Literal (1) /= '=' then Base_64_Reverse_Map.Element (Group_Literal (1)) else 0);
            Second : Unsigned_32 := (if Group_Literal (2) /= '=' then Base_64_Reverse_Map.Element (Group_Literal (2)) else 0);
            Third  : Unsigned_32 := (if Group_Literal (3) /= '=' then Base_64_Reverse_Map.Element (Group_Literal (3)) else 0);
            Fourth : Unsigned_32 := (if Group_Literal (4) /= '=' then Base_64_Reverse_Map.Element (Group_Literal (4)) else 0);

         begin
            To_Write.Append (Character'Val (Shift_Left (First, 2) or Shift_Right (Second, 4)));

            if Group_Literal (3) /= '='
            then
               To_Write.Append (Character'Val (Shift_Left (Second and 2#1111#, 4) or Shift_Right (Third, 2)));

               if Group_Literal (4) /= '='
               then
                  To_Write.Append (Character'Val (Shift_Left (Third and 2#11#, 6) or Fourth));
               end if;
            end if;
         end;
         Position := @ + 4;
         exit when (Position - 4) >= Data'Length;
      end loop;
      return To_Write.To_String;
   end Decode_Base64;

end Extensible_HTTP.Base64;
