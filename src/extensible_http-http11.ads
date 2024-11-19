package Extensible_HTTP.HTTP11 is
   type HTTP_11_Message is
     abstract tagged record
        Fields       : Field_Hashed_Maps.Map;
        Message_Body : UTF_String_Holders.Holder := UTF_String_Holders.Empty_Holder;
     end record;

   procedure Write_HTTP_11_Message (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Message);

   procedure Read_HTTP_11_Message_No_Body (Stream : not null access Root_Stream_Type'Class; Item : out HTTP_11_Message);

   procedure Read_HTTP_11_Message_Body (Stream : not null access Root_Stream_Type'Class; Item : out HTTP_11_Message);

   type HTTP_11_Request_Message is
     new HTTP_11_Message with record
        Method : HTTP_11_Method_Types;
        Target : String_Holders.Holder;
     end record;

   type HTTP_11_Status_Code is range 100 .. 999 with
     Put_Image => HTTP_11_Status_Image;

   procedure HTTP_11_Status_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : HTTP_11_Status_Code);

   type HTTP_11_Response_Message is
     new HTTP_11_Message with record
        Status : HTTP_11_Status_Code;
        Reason : String_Holders.Holder := String_Holders.Empty_Holder;
     end record;

private
   procedure Write_HTTP_11_Request_Message (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Request_Message);

   procedure Read_HTTP_11_Request_Message (Stream : not null access Root_Stream_Type'Class; Item : out HTTP_11_Request_Message);

   for HTTP_11_Request_Message'Read use Read_HTTP_11_Request_Message;
   for HTTP_11_Request_Message'Write use Write_HTTP_11_Request_Message;

   procedure Write_HTTP_11_Response_Message (Stream : not null access Root_Stream_Type'Class; Item : HTTP_11_Response_Message);

   procedure Read_HTTP_11_Response_Message (Stream : not null access Root_Stream_Type'Class; Item : out HTTP_11_Response_Message);

   for HTTP_11_Response_Message'Read use Read_HTTP_11_Response_Message;
   for HTTP_11_Response_Message'Write use Write_HTTP_11_Response_Message;
end Extensible_HTTP.HTTP11;
