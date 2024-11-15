with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;

package body Extensible_HTTP is
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

end Extensible_HTTP;
