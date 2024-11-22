with Ada.Calendar;

package Extensible_HTTP.Dates is
   function Time_To_HTTP_Date_String (Time : Ada.Calendar.Time) return String;
end Extensible_HTTP.Dates;
