with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Text_Buffers;

with Ada.Strings.UTF_Encoding;
use Ada.Strings.UTF_Encoding;

with Ada.Streams;
use Ada.Streams;

with Interfaces;
use Interfaces;

package Extensible_HTTP is
   CRLF : constant String := ASCII.CR & ASCII.LF;

     --  https://datatracker.ietf.org/doc/html/rfc9110#name-methods, Table 4 * Essential methods must always be available. * Non-essential methods can be disabled.
     --   - Methods that are disabled/unimplemented must:
     --    - Reply with a 501.
     --    - If they are disallowed, reply with a 405.
     --  * Safe methods only do "read-only" operations on the server.
     --   - https://datatracker.ietf.org/doc/html/rfc9110#name-safe-methods
     --  * Idempotent methods will have the same effect across identical requests.
     --   - Safe methods are idempotent.
     --   - https://datatracker.ietf.org/doc/html/rfc9110#name-idempotent-methods
     --  @value GET Safe, essential @value HEAD Safe, essential @value POST Unsafe, non-idempotent, non-essential @value PUT Unsafe, idempotent, non-essential @value DELETE Unsafe,
     --  idempotent, non-essential @value CONNECT Unsafe, non-idempotent, non-essential @value OPTIONS Safe, non-essential @value TRACE Safe, non-essential

   type HTTP_11_Method_Types is
     (GET,
      HEAD,
      POST,
      PUT,
      DELETE,
      CONNECT,
      OPTIONS,
      TRACE);

   type Token is new String;

   type Visible_Character is new Character range Character'Val (16#21#) .. Character'Val (16#7E#);

   type OBS_Character is new Character range Character'Val (16#80#) .. Character'Val (16#FF#);

   type Field_Visible_Character is new Character range Character (Visible_Character'First) .. Character (OBS_Character'Last) with
     Predicate => Character (Field_Visible_Character) /= Character'Val (16#7F#);

   type White_Space is new Character range ' ' .. ASCII.HT;

   package String_Holders is new Ada.Containers.Indefinite_Holders (String);
   package UTF_String_Holders is new Ada.Containers.Indefinite_Holders (UTF_8_String);

   package Field_Hashed_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type => Token, Element_Type => String);

   function Read_String_From_Stream (Stream : not null access Root_Stream_Type'Class; Delimiter : String) return String;
end Extensible_HTTP;
