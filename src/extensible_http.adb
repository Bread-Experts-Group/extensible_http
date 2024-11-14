with Ada.Containers.Ordered_Maps;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;

with Ada.Text_IO;

package body Extensible_HTTP is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   use Field_Hashed_Maps;

      ----------------
      -- Encode_URL --
      ----------------

   function Encode_URL (URL : String) return String is

      Buffer : Unbounded_String;
      Read   : Character;
      Hex    : String (1 .. 2);

   begin
      for Position in URL'Range
      loop
         Read := URL (Position);

         if Read in 'A' .. 'Z' or else Read in 'a' .. 'z'
         then
            Buffer.Append (Read);

         else
            Ada.Integer_Text_IO.Put (Hex, Character'Pos (Read), 16);
            Buffer.Append ('%' & Hex);
         end if;
      end loop;
      return Buffer.To_String;
   end Encode_URL;

      ----------------
      -- Decode_URL --
      ----------------

   function Decode_URL (URL : String) return String is

      Buffer   : String (1 .. URL'Length);
      Filled   : Natural  := 0;
      Position : Positive := URL'First;

   begin
      while Position in URL'Range
      loop
         Filled := Filled + 1;

         case URL (Position) is
            when '+' =>
               Buffer (Filled) := ' ';
               Position        := Position + 1;

            when '%' =>
               Buffer (Filled) := Character'Val (Natural'Value ("16#" & URL (Position + 1 .. Position + 2) & "#"));
               Position        := Position + 3;

            when others =>
               Buffer (Filled) := URL (Position);
               Position        := Position + 1;
         end case;
      end loop;

      return Buffer (1 .. Filled);
   end Decode_URL;

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
            To_Write.Append (Character'Val (Shift_Left (Second and 2#1111#, 4) or Shift_Right (Third, 2)));
            To_Write.Append (Character'Val (Shift_Left (Third and 2#11#, 6) or Fourth));
         end;
         Position := @ + 4;
         exit when Position > Data'Length;
      end loop;
      Ada.Text_IO.Put_Line (To_Write'Image);
      return To_Write.To_String;
   end Decode_Base64;

      -----------------------------
      -- Read_String_From_Stream --
      -----------------------------

   function Read_String_From_Stream (Stream : not null access Root_Stream_Type'Class; Delimiter : String) return String is

      Read_Into : Unbounded_String;
      Read      : Unbounded_String;

   begin
      loop
         Read.Append (Character'Input (Stream));

         if Read.Length >= Delimiter'Length and then Read.Slice (Read.Length - Delimiter'Length + 1, Read.Length) = Delimiter
         then
            Read_Into.Append (Read.Slice (1, Read.Length - Delimiter'Length));
            exit;
         end if;
      end loop;
      return Read_Into.To_String;
   end Read_String_From_Stream;

   procedure HTTP_11_Status_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : HTTP_11_Status_Code) is

      As_String : constant String := Integer (Value)'Image;

   begin
      Output.Put (As_String (2 .. As_String'Last));
   end HTTP_11_Status_Image;

   ----------------------
   -- BASE / WRITE     --
   -- HTTP/1.1 Message --
   ----------------------

   procedure Write_HTTP_11_Message_Body (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Message) is
   begin
      String'Write (Stream, CRLF);

      for E in Item.Fields.Iterate
      loop
         Token'Write (Stream, Key (E));
         String'Write (Stream, ':' & Element (E) & CRLF);
      end loop;
      String'Write (Stream, CRLF);

      if not Item.Message_Body.Is_Empty
      then
         String'Write (Stream, Item.Message_Body.Element);
      end if;
   end Write_HTTP_11_Message_Body;

      ----------------------
      -- BASE / READ      --
      -- HTTP/1.1 Message --
      ----------------------

   procedure Read_HTTP_11_Message_Body (Stream : not null access Root_Stream_Type'Class; Item : out HTTP_11_Message) is

      CRLF_Tag : String (1 .. 2);

   begin
      String'Read (Stream, CRLF_Tag);

      loop
         String'Read (Stream, CRLF_Tag);

         if CRLF_Tag /= CRLF
         then
            declare

               Read_Token   : constant Token        := Token (CRLF_Tag & Read_String_From_Stream (Stream, ":"));
               Read_Content : String_Holders.Holder := String_Holders.To_Holder (Read_String_From_Stream (Stream, CRLF));
               First        : Character             := Read_Content.Element (1);
               Last         : Character             := Read_Content.Element (Read_Content.Element'Length);

            begin
               if First = ' ' or else First = ASCII.HT
               then
                  Read_Content.Replace_Element (Ada.Strings.Fixed.Tail (Read_Content.Element, Read_Content.Element'Length - 1));
               end if;

               if Last = ' ' or else Last = ASCII.HT
               then
                  Read_Content.Replace_Element (Ada.Strings.Fixed.Head (Read_Content.Element, Read_Content.Element'Length - 1));
               end if;

               Item.Fields.Include (Read_Token, Read_Content.Element);
            end;

         else
            exit;
         end if;
      end loop;

      if Item.Fields.Contains ("Transfer-Encoding")
      then
         raise Program_Error with "TE!!";

      elsif Item.Fields.Contains ("Content-Length")
      then
         declare

            Content_Length : Natural := Natural'Value (Item.Fields.Element ("Content-Length"));
            Message_Body   : Unbounded_String;

         begin
            loop
               exit when Content_Length = 0;

               declare

                  Chunked : String (1 .. Natural'Min (Content_Length, 8_128));

               begin
                  String'Read (Stream, Chunked);
                  Message_Body.Append (Chunked);
                  Content_Length := @ - Chunked'Length;
               end;
            end loop;
            Item.Message_Body.Replace_Element (Message_Body.To_String);
         end;
      end if;
   end Read_HTTP_11_Message_Body;

      ----------------------
      -- REQUEST / WRITE  --
      -- HTTP/1.1 Message --
      ----------------------

   procedure Write_HTTP_11_Request_Message (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Request_Message) is

      RequestLine : constant String := Item.Method'Image & ' ' & Item.Target.Element & " HTTP/1.1";

   begin
      String'Write (Stream, RequestLine);
      Write_HTTP_11_Message_Body (Stream, Item);
   end Write_HTTP_11_Request_Message;

      ----------------------
      -- REQUEST / READ   --
      -- HTTP/1.1 Message --
      ----------------------

   procedure Read_HTTP_11_Request_Message (Stream : not null access Root_Stream_Type'Class; Item : out HTTP_11_Request_Message) is

      discard : String (1 .. 8);

   begin
      Item.Method := HTTP_11_Method_Types'Value (Read_String_From_Stream (Stream, " "));
      Item.Target.Replace_Element (Decode_URL (Read_String_From_Stream (Stream, " ")));
      String'Read (Stream, discard);
      Read_HTTP_11_Message_Body (Stream, Item);
   end Read_HTTP_11_Request_Message;

      ----------------------
      -- RESPONSE / WRITE --
      -- HTTP/1.1 Message --
      ----------------------

   procedure Write_HTTP_11_Response_Message (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Response_Message) is

      StatusLine : constant String := "HTTP/1.1 " & Item.Status'Image & ' ';

   begin
      String'Write (Stream, StatusLine);

      if not Item.Reason.Is_Empty
      then
         String'Write (Stream, Item.Reason.Element);
      end if;
      Write_HTTP_11_Message_Body (Stream, Item);
   end Write_HTTP_11_Response_Message;

   ----------------------
   -- RESPONSE / READ  --
   -- HTTP/1.1 Message --
   ----------------------

   procedure Read_HTTP_11_Response_Message (Stream : not null access Root_Stream_Type'Class; Item : out HTTP_11_Response_Message) is
   begin
      null;
   end Read_HTTP_11_Response_Message;

end Extensible_HTTP;
