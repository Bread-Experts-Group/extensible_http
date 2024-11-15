package Extensible_HTTP.Base64 is
   function Extract_Bits (Data : Unsigned_32; From, To : Natural) return Unsigned_32 with
     Pre => From <= To and To < Data'Size, Post => Extract_Bits'Result < (2**(To - From + 1));

   function Encode_Base64 (Data : String) return String;

   function Decode_Base64 (Data : String) return String;
end Extensible_HTTP.Base64;
