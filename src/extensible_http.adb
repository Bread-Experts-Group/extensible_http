with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Extensible_HTTP is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   use Field_Hashed_Maps;

   function Read_String_From_Stream
     (Stream : not null access Root_Stream_Type'Class; Delimiter : String)
      return String
   is
      Read_Into : Unbounded_String;
      Read      : Unbounded_String;
   begin
      loop
         Read.Append (Character'Input (Stream));
         if Read.Length >= Delimiter'Length
           and then
             Read.Slice (Read.Length - Delimiter'Length + 1, Read.Length) =
             Delimiter
         then
            Read_Into.Append (Read.Slice (1, Read.Length - Delimiter'Length));
            exit;
         end if;
      end loop;
      return Read_Into.To_String;
   end Read_String_From_Stream;

   procedure HTTP_11_Status_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  :        HTTP_11_Status_Code)
   is
      As_String : constant String := Integer (Value)'Image;
   begin
      Output.Put (As_String (2 .. As_String'Last));
   end HTTP_11_Status_Image;

   ----------------------
   -- BASE / WRITE     --
   -- HTTP/1.1 Message --
   ----------------------

   procedure Write_HTTP_11_Message_Body
     (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Message)
   is
   begin
      String'Write (Stream, CRLF);
      for E in Item.Fields.Iterate loop
         Token'Write (Stream, Key (E));
         String'Write (Stream, ':' & Element (E) & CRLF);
      end loop;
      String'Write (Stream, CRLF);
      if not Item.Message_Body.Is_Empty then
         String'Write (Stream, Item.Message_Body.Element);
      end if;
   end Write_HTTP_11_Message_Body;

   ----------------------
   -- BASE / READ      --
   -- HTTP/1.1 Message --
   ----------------------

   procedure Read_HTTP_11_Message_Body
     (Stream :     not null access Root_Stream_Type'Class;
      Item   : out HTTP_11_Message)
   is
      CRLF_Tag : String (1 .. 2);
   begin
      String'Read (Stream, CRLF_Tag);
      loop
         String'Read (Stream, CRLF_Tag);
         if CRLF_Tag /= CRLF then
            declare
               Read_Token : constant Token :=
                 Token (CRLF_Tag & Read_String_From_Stream (Stream, ":"));
               Read_Content : constant String :=
                 Read_String_From_Stream (Stream, CRLF);
            begin
               Item.Fields.Include (Read_Token, Read_Content);
            end;
         else
            exit;
         end if;
      end loop;
      if Item.Fields.Contains ("Transfer-Encoding") then
         raise Program_Error with "TE!!";
      elsif Item.Fields.Contains ("Content-Length") then
         declare
            Content_Length : Natural :=
              Natural'Value (Item.Fields.Element ("Content-Length"));
            Message_Body : Unbounded_String;
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

   procedure Write_HTTP_11_Request_Message
     (Stream : not null access Root_Stream_Type'Class;
      Item   : HTTP_11_Request_Message)
   is
      RequestLine : constant String :=
        Item.Method'Image & ' ' & Item.Target.Element & " HTTP/1.1";
   begin
      String'Write (Stream, RequestLine);
      Write_HTTP_11_Message_Body (Stream, Item);
   end Write_HTTP_11_Request_Message;

   ----------------------
   -- REQUEST / READ   --
   -- HTTP/1.1 Message --
   ----------------------

   procedure Read_HTTP_11_Request_Message
     (Stream :     not null access Root_Stream_Type'Class;
      Item   : out HTTP_11_Request_Message)
   is
      discard : String (1 .. 8);
   begin
      Item.Method :=
        HTTP_11_Method_Types'Value (Read_String_From_Stream (Stream, " "));
      Item.Target.Replace_Element (Read_String_From_Stream (Stream, " "));
      String'Read (Stream, discard);
      Read_HTTP_11_Message_Body (Stream, Item);
   end Read_HTTP_11_Request_Message;

   ----------------------
   -- RESPONSE / WRITE --
   -- HTTP/1.1 Message --
   ----------------------

   procedure Write_HTTP_11_Response_Message
     (Stream : not null access Root_Stream_Type'Class;
      Item   : HTTP_11_Response_Message)
   is
      StatusLine : constant String := "HTTP/1.1 " & Item.Status'Image & ' ';
   begin
      String'Write (Stream, StatusLine);
      if not Item.Reason.Is_Empty then
         String'Write (Stream, Item.Reason.Element);
      end if;
      Write_HTTP_11_Message_Body (Stream, Item);
   end Write_HTTP_11_Response_Message;

   ----------------------
   -- RESPONSE / READ  --
   -- HTTP/1.1 Message --
   ----------------------

   procedure Read_HTTP_11_Response_Message
     (Stream :     not null access Root_Stream_Type'Class;
      Item   : out HTTP_11_Response_Message)
   is
   begin
      null;
   end Read_HTTP_11_Response_Message;

end Extensible_HTTP;