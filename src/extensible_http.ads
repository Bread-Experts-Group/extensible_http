with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Text_Buffers;

with Ada.Streams; use Ada.Streams;

package Extensible_HTTP is

   type Protocol_Types is (HTTP, HTTPS);

   --  https://datatracker.ietf.org/doc/html/rfc9110#name-methods, Table 4
   --  * Essential methods must always be available.
   --  * Non-essential methods can be disabled.
   --   - Methods that are disabled/unimplemented must:
   --    - Reply with a 501.
   --    - If they are disallowed, reply with a 405.
   --  * Safe methods only do "read-only" operations on the server.
   --   - https://datatracker.ietf.org/doc/html/rfc9110#name-safe-methods
--  * Idempotent methods will have the same effect across identical requests.
   --   - Safe methods are idempotent.
   --   - https://datatracker.ietf.org/doc/html/rfc9110#name-idempotent-methods
   --  @value GET Safe, essential
   --  @value HEAD Safe, essential
   --  @value POST Unsafe, non-idempotent, non-essential
   --  @value PUT Unsafe, idempotent, non-essential
   --  @value DELETE Unsafe, idempotent, non-essential
   --  @value CONNECT Unsafe, non-idempotent, non-essential
   --  @value OPTIONS Safe, non-essential
   --  @value TRACE Safe, non-essential
   type HTTP_11_Method_Types is
     (GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE);

   --  TODO: Token character limits
   type Token is new String;

   --  https://datatracker.ietf.org/doc/html/rfc5234
   --  Defined as "VCHAR"
   type Visible_Character is
     new Character range Character'Val (16#21#) .. Character'Val (16#7E#);

   --  https://datatracker.ietf.org/doc/html/rfc9110#name-field-values
   --  Defined as "obs-text"
   type OBS_Character is
     new Character range Character'Val (16#80#) .. Character'Val (16#FF#);

   --  https://datatracker.ietf.org/doc/html/rfc9110#name-field-values
   --  Defined as "field-vchar"
   type Field_Visible_Character is
     new Character range Character (Visible_Character'First) ..
         Character (OBS_Character'Last) with
      Predicate => Character (Field_Visible_Character) /=
      Character'Val (16#7F#);

      --  https://datatracker.ietf.org/doc/html/rfc9110#name-collected-abnf
      --  Defined as "OWS", "RWS", "BWS" (Optional, Required, Bad) White Space
   type White_Space is new Character range ' ' .. ASCII.HT;

   type String_Access is access constant String;
   type String_Access_Optional is access constant String;

   type HTTP_11_Message_Body is new String;
   type HTTP_11_Message_Body_Access is access constant HTTP_11_Message_Body;

   package Field_Hashed_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Token, Element_Type => String);

   --  https://datatracker.ietf.org/doc/html/rfc9112#name-message
   --  HTTP/1.1 HTTP-message
   type HTTP_11_Message is abstract tagged record
      Fields       : Field_Hashed_Maps.Map;
      Message_Body : HTTP_11_Message_Body_Access;
   end record;

   procedure Write_HTTP_11_Message_Body
     (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Message);

   procedure Read_HTTP_11_Message_Body
     (Stream :     not null access Root_Stream_Type'Class;
      Item   : out HTTP_11_Message);

   --  https://datatracker.ietf.org/doc/html/rfc9112#name-request-line
   --  HTTP/1.1 request-line
   --  @field Target The request target for this request. TODO accompany forms
   type HTTP_11_Request_Message is new HTTP_11_Message with record
      Method : HTTP_11_Method_Types;
      Target : String_Access;
   end record;

   --  https://datatracker.ietf.org/doc/html/rfc9112#name-status-line
   --  HTTP/1.1 status-code; NOTE: The 'Image of this type has no sign space.
   type HTTP_11_Status_Code is range 100 .. 999 with
      Put_Image => HTTP_11_Status_Image;

   procedure HTTP_11_Status_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  :        HTTP_11_Status_Code);

   --  https://datatracker.ietf.org/doc/html/rfc9112#name-status-line
   --  HTTP/1.1 status-line
   --  @field Status The three digit status code in response to the previous
   --  request.
   --  @field Reason A reason phrase associated with the status code.
   --  By default, this is null.
   type HTTP_11_Response_Message is new HTTP_11_Message with record
      Status : HTTP_11_Status_Code;
      Reason : String_Access_Optional := null;
   end record;

private

   procedure Write_HTTP_11_Request_Message
     (Stream : not null access Root_Stream_Type'Class;
      Item   : HTTP_11_Request_Message);

   procedure Read_HTTP_11_Request_Message
     (Stream :     not null access Root_Stream_Type'Class;
      Item   : out HTTP_11_Request_Message);

   for HTTP_11_Request_Message'Read use Read_HTTP_11_Request_Message;
   for HTTP_11_Request_Message'Write use Write_HTTP_11_Request_Message;

   procedure Write_HTTP_11_Response_Message
     (Stream : not null access Root_Stream_Type'Class;
      Item   : HTTP_11_Response_Message);

   procedure Read_HTTP_11_Response_Message
     (Stream :     not null access Root_Stream_Type'Class;
      Item   : out HTTP_11_Response_Message);

   for HTTP_11_Response_Message'Read use Read_HTTP_11_Response_Message;
   for HTTP_11_Response_Message'Write use Write_HTTP_11_Response_Message;

end Extensible_HTTP;
