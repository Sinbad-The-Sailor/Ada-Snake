-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbj�rn Jonsson Ada library                      |--
--|                                                                         |--
--|                         T J A . C A L E N D A R                         |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  2.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2000                           |--
--|                   Torbj�rn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2000-04-14  Version 2.01 is ok.                                       |--
--|   2000-03-16  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbj�rn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package includes methods which makes it easier to handle dates   |--
--|   and times (and time intervals).                                       |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Calendar is

  -----------------------------------------------------------------------------
  --| "Day_Of_Week_Type"
  -----------------------------------------------------------------------------

  type Day_Of_Week_Type is
    (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

  -----------------------------------------------------------------------------
  --| "Date_Type" and "Time_Type" are the types for dates and times. The values
  --| "Null_Date" and "Null_Time" are used to identify that the datas are
  --| "empty".
  --|
  --| "Date_And_Time_Type" is the type for the combined type for date and time.
  --| "Null_Date_And_Time" represents the "empty" value.
  -----------------------------------------------------------------------------

  type Date_Type is private;
  type Time_Type is private;
  type Date_And_Time_Type is private;

  Null_Date          : constant Date_Type;
  Null_Time          : constant Time_Type;
  Null_Date_And_Time : constant Date_And_Time_Type;

  -----------------------------------------------------------------------------
  --| "Clock" returns the current time and date from the computer.
  -----------------------------------------------------------------------------

  procedure Clock(Current_Date_And_Time : out Date_And_Time_Type);

  function Clock return Date_And_Time_Type;

  -----------------------------------------------------------------------------
  --| "Get" reads a date/time from keyboard (if only one parameter) or from
  --| 'File'.
  -----------------------------------------------------------------------------

  procedure Get(Item : out Date_Type);
  procedure Get(File : in     File_Type;
                Item :    out Date_Type);

  procedure Get(Item : out Time_Type);
  procedure Get(File : in     File_Type;
                Item :    out Time_Type);

  -----------------------------------------------------------------------------
  --| "Put" writes a date/time to screen (if only one parameter) or to 'File'.
  --|
  --| "Put_Line" does a "Put" and then a "New_Line".
  -----------------------------------------------------------------------------

  procedure Put(Item : in Date_Type);
  procedure Put(File : in File_Type;
                Item : in Date_Type);

  procedure Put_Line(Item : in Date_Type);
  procedure Put_Line(File : in File_Type;
                     Item : in Date_Type);

  procedure Put(Item     : in Time_Type;
                Seconds  : in Boolean := True;
                Decimals : in Natural := 0);
  procedure Put(File     : in File_Type;
                Item     : in Time_Type;
                Seconds  : in Boolean := True;
                Decimals : in Natural := 0);

  procedure Put_Line(Item     : in Time_Type;
                     Seconds  : in Boolean := True;
                     Decimals : in Natural := 0);
  procedure Put_Line(File     : in File_Type;
                     Item     : in Time_Type;
                     Seconds  : in Boolean := True;
                     Decimals : in Natural := 0);

  -----------------------------------------------------------------------------
  --| "To_Date_And_Time_Type" creates a 'Date_And_Time_Type' from a 'Date_Type'
  --| and a 'Time_Type'.
  --|
  --| "To_Date_Type" and "To_Time_Type" converts a (unbounded) string to
  --| 'Date_Type' or 'Time_Type'.
  --| If the string doesn't have a legal date/time 'Date_Data_Error' or
  --| 'Time_Data_Error' will be raised.
  --|
  --| "To_[Unbounded_]String" converts a date or time and returns it as a
  --| '[Unbounded_]String'.
  -----------------------------------------------------------------------------

  function To_Date_And_Time_Type(Date : in Date_Type := Null_Date;
                                 Time : in Time_Type := Null_Time)
           return Date_And_Time_Type;

  function To_Date_Type(Item : in String) return Date_Type;
  function To_Date_Type(Item : in Unbounded_String) return Date_Type;

  function To_Time_Type(Item : in String) return Time_Type;
  function To_Time_Type(Item : in Unbounded_String) return Time_Type;

  function To_String(Item : in Date_Type) return String;
  function To_String(Item     : in Time_Type;
                     Seconds  : in Boolean := True;
                     Decimals : in Natural := 0)
           return String;

  function To_Unbounded_String(Item : in Date_Type) return Unbounded_String;
  function To_Unbounded_String(Item     : in Time_Type;
                               Seconds  : in Boolean := True;
                               Decimals : in Natural := 0)
           return Unbounded_String;

  -----------------------------------------------------------------------------
  --| "Get_Date" or "Get_Time" returns the date or time from a
  --| 'Date_And_Time_Type'.
  -----------------------------------------------------------------------------

  function Get_Date(Date_And_Time : in Date_And_Time_Type) return Date_Type;
  function Get_Time(Date_And_Time : in Date_And_Time_Type) return Time_Type;

  -----------------------------------------------------------------------------
  --| "Set" inserts date and/or time into a 'Date_And_Time_Type'.
  -----------------------------------------------------------------------------

  procedure Set(Date_And_Time : in out Date_And_Time_Type;
                Date          : in     Date_Type := Null_Date;
                Time          : in     Time_Type := Null_Time);

  -----------------------------------------------------------------------------
  --| "Get_Xxx" returns "Xxx" (where Xxx is the name of the data) from a
  --| 'Date_And_Time_Type'.
  -----------------------------------------------------------------------------

  function Get_Year(Date_And_Time : in Date_And_Time_Type) return Integer;
  function Get_Month(Date_And_Time : in Date_And_Time_Type) return Integer;
  function Get_Day(Date_And_Time : in Date_And_Time_Type) return Integer;
  function Get_Hour(Date_And_Time : in Date_And_Time_Type) return Integer;
  function Get_Minute(Date_And_Time : in Date_And_Time_Type) return Integer;
  function Get_Second(Date_And_Time : in Date_And_Time_Type) return Integer;
  function Get_Decimals(Date_And_Time : in Date_And_Time_Type) return Float;

  function Get_Day_In_Year(Date_And_Time : in Date_And_Time_Type)
           return Integer;
  function Get_Week_In_Year(Date_And_Time : in Date_And_Time_Type)
           return Integer;
  function Get_Day_Of_Week(Date_And_Time : in Date_And_Time_Type)
           return Day_Of_Week_Type;

  -----------------------------------------------------------------------------
  --| "Get_Xxx" returns "Xxx" (where Xxx is the name of the data) from a
  --| 'Date_Type'.
  -----------------------------------------------------------------------------

  function Get_Year(Date : in Date_Type) return Integer;
  function Get_Month(Date : in Date_Type) return Integer;
  function Get_Day(Date : in Date_Type) return Integer;

  function Get_Day_In_Year(Date : in Date_Type) return Integer;
  function Get_Week_In_Year(Date : in Date_Type) return Integer;
  function Get_Day_Of_Week(Date : in Date_Type) return Day_Of_Week_Type;

  -----------------------------------------------------------------------------
  --| "Get_Xxx" returns "Xxx" (where Xxx is the name of the data) from a
  --| 'Time_Type'.
  -----------------------------------------------------------------------------

  function Get_Hour(Time : in Time_Type) return Integer;
  function Get_Minute(Time : in Time_Type) return Integer;
  function Get_Second(Time : in Time_Type) return Integer;
  function Get_Decimals(Time : in Time_Type) return Float;

  -----------------------------------------------------------------------------
  --| "Legal_Date" and "Legal_Time" returns 'True' if the 'Item' consists of a
  --| legal value.
  -----------------------------------------------------------------------------

  function Legal_Date(Item : in Date_Type) return Boolean;
  function Legal_Time(Item : in Time_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| Min and Max for dates/times.
  -----------------------------------------------------------------------------

  function Min(A, B : in Date_Type) return Date_type;
  function Max(A, B : in Date_Type) return Date_type;

  function Min(A, B : in Time_Type) return Time_type;
  function Max(A, B : in Time_Type) return Time_type;

  -----------------------------------------------------------------------------
  --| Operators for comparing dates and/or times  ("/=" implicit from "=" in
  --| Ada).
  --|
  --| "=" returns 'True' only if 'Left' and 'Right' are identical.
  --| "<" is defined by 'Left' is before (earlier than) 'Right' and so on.
  -----------------------------------------------------------------------------

  function "="(Left, Right : in Date_Type) return Boolean;
  function "<"(Left, Right : in Date_Type) return Boolean;
  function ">"(Left, Right : in Date_Type) return Boolean;
  function "<="(Left, Right : in Date_Type) return Boolean;
  function ">="(Left, Right : in Date_Type) return Boolean;

  function "="(Left, Right : in Time_Type) return Boolean;
  function "<"(Left, Right : in Time_Type) return Boolean;
  function ">"(Left, Right : in Time_Type) return Boolean;
  function "<="(Left, Right : in Time_Type) return Boolean;
  function ">="(Left, Right : in Time_Type) return Boolean;

  function "="(Left, Right : in Date_And_Time_Type) return Boolean;
  function "<"(Left, Right : in Date_And_Time_Type) return Boolean;
  function ">"(Left, Right : in Date_And_Time_Type) return Boolean;
  function "<="(Left, Right : in Date_And_Time_Type) return Boolean;
  function ">="(Left, Right : in Date_And_Time_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| Operator "+" and "-" for 'Date_Type' combined with 'Integer'.
  --| The operators returns the date given if we add the number of days, given
  --| by the integer, to the given date.
  -----------------------------------------------------------------------------

  function "+"(Left  : in Date_Type;
               Right : in Integer) return Date_Type;

  function "-"(Left  : in Date_Type;
               Right : in Integer) return Date_Type;

  -----------------------------------------------------------------------------
  --| "Time_Interval_Type" is the type for time intervals. The value
  --| "Null_Time_Interval" is used to identify that the data are "empty".
  --|
  --| This type accepts more than 23 hours, 60 minutes and 60 seconds  (which
  --| 'Time_Type' doesn't) but an interval with seconds or minutes more than 59
  --| are converted to an "identical" interval where minutes and seconds are in
  --| the range 0 to 59. E.g. "12 h 61 min 63 sec" gives the new interval
  --| "13 h 2 min 3 sec". This type is used by the "+" and "-" operators to
  --| manipulate times.
  -----------------------------------------------------------------------------

  type Time_Interval_Type is private;

  Null_Time_Interval : constant Time_Interval_Type;

  -----------------------------------------------------------------------------
  --| "Get" reads a time interval from keyboard (if only one parameter) or from
  --| 'File'.
  -----------------------------------------------------------------------------

  procedure Get(Item : out Time_Interval_Type);
  procedure Get(File : in     File_Type;
                Item :    out Time_Interval_Type);

  -----------------------------------------------------------------------------
  --| "Put" writes a time interval to screen (if only one parameter) or to
  --| 'File'.
  --|
  --| "Put_Line" does a "Put" and then a "New_Line".
  -----------------------------------------------------------------------------

  procedure Put(Item     : in Time_Interval_Type;
                Decimals : in Natural := 0);
  procedure Put(File     : in File_Type;
                Item     : in Time_Interval_Type;
                Decimals : in Natural := 0);

  procedure Put_Line(Item     : in Time_Interval_Type;
                     Decimals : in Natural := 0);
  procedure Put_Line(File     : in File_Type;
                     Item     : in Time_Interval_Type;
                     Decimals : in Natural := 0);

  -----------------------------------------------------------------------------
  --| "To_Time_Interval_Type" converts a string or an unbounded string to
  --| 'Time_Interval_Type'.
  --| If the string doesn't have a legal time interval the exception named
  --| 'Time_Interval_Data_Error' will be raised.
  --|
  --| "To_[Unbounded_]String" converts a time interval and returns it as a
  --| '[Unbounded_]String'.
  -----------------------------------------------------------------------------

  function To_Time_Interval_Type(Item : in String) return Time_Interval_Type;
  function To_Time_Interval_Type(Item : in Unbounded_String)
           return Time_Interval_Type;

  function To_String(Item : in Time_Interval_Type) return String;

  function To_Unbounded_String(Item : in Time_Interval_Type)
           return Unbounded_String;

  -----------------------------------------------------------------------------
  --| "Get_Xxx" returns "Xxx" (where Xxx is the name of the data) from a
  --| 'Time_Interval_Type'.
  -----------------------------------------------------------------------------

  function Get_Hours(Time_Interval : in Time_Interval_Type) return Integer;
  function Get_Minutes(Time_Interval : in Time_Interval_Type) return Integer;
  function Get_Seconds(Time_Interval : in Time_Interval_Type) return Integer;
  function Get_Decimals(Time_Interval : in Time_Interval_Type) return Float;

  -----------------------------------------------------------------------------
  --| "Legal_Time_Interval" returns 'True' if the 'Item' consists of a legal
  --| value.
  -----------------------------------------------------------------------------

  function Legal_Time_Interval(Item : in Time_Interval_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| Min and Max for time intervals.
  -----------------------------------------------------------------------------

  function Min(A, B : in Time_Interval_Type) return Time_Interval_type;
  function Max(A, B : in Time_Interval_Type) return Time_Interval_type;

  -----------------------------------------------------------------------------
  --| Operator "+" and "-" for 'Time_Type' combined with 'Time_Interval_Type'.
  --| The operators returns the time given if we add the time interval to the
  --| given time.
  -----------------------------------------------------------------------------

  function "+"(Left  : in Time_Type;
               Right : in Time_Interval_Type) return Time_Type;

  function "-"(Left  : in Time_Type;
               Right : in Time_Interval_Type) return Time_Type;

  -----------------------------------------------------------------------------
  --| Operator "+" and "-" for 'Time_Interval_Type'.
  --| The operators returns a new time interval.
  -----------------------------------------------------------------------------

  function "+"(Left, Right : in Time_Interval_Type) return Time_Interval_Type;

  function "-"(Left, Right : in Time_Interval_Type) return Time_Interval_Type;

  -----------------------------------------------------------------------------
  --| Operator "*" and "/" for 'Time_Interval_Type' (right argument must be an
  --| 'Integer' or 'Float'.
  --| The operators returns a new time interval.
  -----------------------------------------------------------------------------

  function "*"(Left  : in Time_Interval_Type;
               Right : in Integer) return Time_Interval_Type;
  function "*"(Left  : in Time_Interval_Type;
               Right : in Float) return Time_Interval_Type;

  function "/"(Left  : in Time_Interval_Type;
               Right : in Integer) return Time_Interval_Type;
  function "/"(Left  : in Time_Interval_Type;
               Right : in Float) return Time_Interval_Type;

  -----------------------------------------------------------------------------
  --| Operator "-" for 'Time_Type'.
  --| The operator returns the time interval between the two times. The time
  --| interval is always calculated counter clockwise from 'Left' to 'Right'.
  -----------------------------------------------------------------------------

  function "-"(Left, Right : in Time_Type) return Time_Interval_Type;

  -----------------------------------------------------------------------------
  --| Operators for comparing time intervals  ("/=" implicit from "=" in
  --| Ada).
  --|
  --| "=" returns 'True' only if 'Left' and 'Right' are identical.
  -----------------------------------------------------------------------------

  function "="(Left, Right : in Time_Interval_Type) return Boolean;
  function "<"(Left, Right : in Time_Interval_Type) return Boolean;
  function ">"(Left, Right : in Time_Interval_Type) return Boolean;
  function "<="(Left, Right : in Time_Interval_Type) return Boolean;
  function ">="(Left, Right : in Time_Interval_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| Exceptions for TJa.Calendar-library.
  -----------------------------------------------------------------------------

  Date_Data_Error           : exception;  -- Not a date.
  Time_Data_Error           : exception;  -- Not a time.
  Time_Interval_Data_Error  : exception;  -- Not a time interval.
  Time_Interval_Value_Error : exception;  -- Can't have negative time interval.
  Time_Zero_Division_Error  : exception;  -- Division by zero not allowed.

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of 'Date_And_Time_Type', 'Date_Type' and
  --| 'Time_Type'.
  -----------------------------------------------------------------------------

  type Date_Type is
       record
         Year  : Integer := 0;
         Month : Integer := 0;
         Day   : Integer := 0;
       end record;

  type Time_Type is
       record
         Hour     : Integer := -1;
         Minute   : Integer := -1;
         Second   : Integer := -1;
         Decimals : Day_Duration := 0.0;
       end record;

  type Date_And_Time_Type is
       record
         Date : Date_Type := Null_Date;
         Time : Time_Type := Null_Time;
       end record;

  type Time_Interval_Type is
       record
         Hours    : Integer := -1;
         Minutes  : Integer := -1;
         Seconds  : Integer := -1;
         Decimals : Day_Duration := 0.0;
       end record;

  -----------------------------------------------------------------------------
  --| The constants that represents "empty" date, time and 'date and time'.
  -----------------------------------------------------------------------------

  Null_Date          : constant Date_Type := (Year  => 0,
                                              Month => 0,
                                              Day   => 0);
  Null_Time          : constant Time_Type := (Hour     => -1,
                                              Minute   => -1,
                                              Second   => -1,
                                              Decimals => 0.0);
  Null_Date_And_Time : constant Date_And_Time_Type := (Date => Null_Date,
                                                       Time => Null_Time);

  -----------------------------------------------------------------------------
  --| The constant that represents "empty" time interval.
  -----------------------------------------------------------------------------

  Null_Time_Interval : constant Time_Interval_Type := (Hours    => -1,
                                                       Minutes  => -1,
                                                       Seconds  => -1,
                                                       Decimals => 0.0);

  -----------------------------------------------------------------------------

end TJa.Calendar;
