with Ada.Containers.Ordered_Maps;

with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Extensible_HTTP.URL is
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

end Extensible_HTTP.URL;
