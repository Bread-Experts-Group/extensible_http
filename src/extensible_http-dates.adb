with GNAT.Calendar;

with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

--  (Source : String;
--    Count  : Natural;
--    Pad    : Character := Space) return String

package body Extensible_HTTP.Dates is

   Month_Names : constant array (1 .. 12)
     of String (1 .. 3) :=
     ["Jan",
     "Feb",
     "Mar",
     "Apr",
     "May",
     "Jun",
     "Jul",
     "Aug",
     "Sep",
     "Oct",
     "Nov",
     "Dec"];

   function Pad_Number_2 (Number : Integer) return String is

      String_2 : String (1 .. 2) := "00";

   begin
      Ada.Strings.Fixed.Move (Number'Image (2 .. Number'Image'Length), String_2, Ada.Strings.Left, Ada.Strings.Right, '0');
      return String_2;
   end Pad_Number_2;

   function Time_To_HTTP_Date_String (Time : Ada.Calendar.Time) return String is

      Year    : Ada.Calendar.Year_Number;
      Month   : Ada.Calendar.Month_Number;
      Day     : Ada.Calendar.Day_Number;
      Hour    : GNAT.Calendar.Hour_Number;
      Minute  : GNAT.Calendar.Minute_Number;
      Second  : GNAT.Calendar.Second_Number;
      discard : GNAT.Calendar.Second_Duration;

      Day_Name : String := GNAT.Calendar.Day_Of_Week (Time)'Image;

      Full_String : Unbounded_String;

   begin
      GNAT.Calendar.Split (Time, Year, Month, Day, Hour, Minute, Second, discard);
      Full_String.Append
        (Day_Name (1) & Ada.Characters.Handling.To_Lower (Day_Name (2 .. 3)) & ", " & Pad_Number_2 (Day) & ' ' & Month_Names (Month) & Year'Image & ' ' & Pad_Number_2 (Hour) & ':'
         & Pad_Number_2 (Minute) & ':' & Pad_Number_2 (Second) & " GMT");
      return Full_String.To_String;
   end Time_To_HTTP_Date_String;

end Extensible_HTTP.Dates;
