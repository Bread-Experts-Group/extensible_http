with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body Extensible_HTTP is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   use Field_Hashed_Maps;

      ----------------
      -- Encode_URL --
      ----------------

   function Encode_URL (URL : in String) return String is

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

   function Decode_URL (URL : in String) return String is

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

               Read_Token   : constant Token := Token (CRLF_Tag & Read_String_From_Stream (Stream, ":"));
               Read_Content : String         := Read_String_From_Stream (Stream, CRLF);

            begin
               if Read_Content (1) = ' ' or else Read_Content (1) = ASCII.HT
               then
                  Item.Fields.Include (Read_Token, Read_Content (2 .. Read_Content'Length));
               else
                  Item.Fields.Include (Read_Token, Read_Content);
               end if;
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
