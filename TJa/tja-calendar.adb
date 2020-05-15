-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                         T J A . C A L E N D A R                         |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  2.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2000                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2000-04-14  Version 2.01 is ok.                                       |--
--|   2000-03-16  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Calendar;                      use Ada.Calendar;

-- External libraries.

-- Internal libraries.
with TJa.Misc;                          use TJa.Misc;

-------------------------------------------------------------------------------
package body TJa.Calendar is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Methods for converting data between 'Time_Type', 'Time_Interval_Type' and
  --| 'Duration'.
  -----------------------------------------------------------------------------
  function To_Time_Type(Item : in Duration) return Time_Type;
  function To_Time_Type(Item : in Time_Interval_Type) return Time_Type;

  function To_Time_Interval_Type(Item : in Duration) return Time_Interval_Type;
  function To_Time_Interval_Type(Item : in Time_Type) return Time_Interval_Type;

  function To_Duration(Item : in Time_Type) return Duration;
  function To_Duration(Item : in Time_Interval_Type) return Duration;

  -----------------------------------------------------------------------------
  --| Methods for converting data between 'Date_Type' and 'Julian numbers'.
  -----------------------------------------------------------------------------
  function Date_To_Julian(Year, Month, Day : in Integer) return Integer;
  function Date_To_Julian(Item : in Date_Type) return Integer;

  procedure Julian_To_Date(Item  : in     Integer;
                           Year  :    out Integer;
                           Month :    out Integer;
                           Day   :    out Integer);
  function Julian_To_Date(Item : in Integer) return Date_Type;


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Clock
  -----------------------------------------------------------------------------

  procedure Clock(Current_Date_And_Time : out Date_And_Time_Type) is

    T : Ada.Calendar.Time;
    F : Day_Duration;

  begin
    loop
      T := Ada.Calendar.Clock;
      F := Ada.Calendar.Seconds(T);
      begin
        Current_Date_And_Time :=
          (Date => (Year  => Integer(Ada.Calendar.Year(T)),
                    Month => Integer(Ada.Calendar.Month(T)),
                    Day   => Integer(Ada.Calendar.Day(T))),
           Time => To_Time_Type(F));
        exit;  -- Everything ok.
      exception
        when others =>
          null;  -- Exception depending on strange "duration" ... Try again.
      end;
    end loop;
  end Clock;

  --===========================================================================
  function Clock
           return Date_And_Time_Type is

    New_Date_And_Time : Date_And_Time_Type;

  begin
    Clock(New_Date_And_Time);
    return New_Date_And_Time;
  end Clock;

  -----------------------------------------------------------------------------
  --| Get
  -----------------------------------------------------------------------------

  procedure Get(Item : out Date_Type) is

  begin
    Get(Standard_Input, Item);
  end Get;
  -----------------------------------------------------------------------------
  procedure Get(File : in     File_Type;
                Item :    out Date_Type) is

    Str  : Unbounded_String;
    Ch   : Character;
    Eoln : Boolean;
    N    : Natural;

  begin
    -- Ignore leading blanks
    while not End_Of_File(File) loop
      Look_Ahead(File, Ch, Eoln);
      exit when (not Eoln) and then (Ch /= ' ');
      Get(File, Ch);
    end loop;

    for I in 1..3 loop
      Str := Null_Unbounded_String;
      if I = 1 then
        N := 4;
      else
        N := 2;
      end if;
      for J in 1..N loop
        Look_Ahead(File, Ch, Eoln);
        exit when Eoln or else (not (Ch in '0'..'9'));
        -- Read the digit from file.
        Get(File, Ch);
        Append(Str, Ch);
      end loop;

      if Length(Str) < N then
        raise Date_Data_Error;
      end if;

      case I is
        when 1 =>
          Item.Year := Integer'Value(To_String(Str));
        when 2 =>
          Item.Month := Integer'Value(To_String(Str));
        when 3 =>
          Item.Day := Integer'Value(To_String(Str));
          -- All date is read.
          exit;
      end case;

      Look_Ahead(File, Ch, Eoln);
      if Eoln or else (Ch /= '-') then
        raise Date_Data_Error;
      end if;

      -- Clean '-' from file.
      Get(File, Ch);
    end loop;
  end Get;

  --===========================================================================
  procedure Get(Item : out Time_Type) is

  begin
    Get(Standard_Input, Item);
  end Get;
  -----------------------------------------------------------------------------
  procedure Get(File : in     File_Type;
                Item :    out Time_Type) is

    Str      : Unbounded_String;
    Ch       : Character;
    Ok       : Boolean;
    Eoln     : Boolean;
    Decimals : Float;

  begin
    -- Ignore leading blanks
    while not End_Of_File(File) loop
      Look_Ahead(File, Ch, Eoln);
      exit when (not Eoln) and then (Ch /= ' ');
      Get(File, Ch);
    end loop;

    for I in 1..3 loop
      Str := Null_Unbounded_String;
      for J in 1..2 loop
        Look_Ahead(File, Ch, Eoln);
        exit when Eoln or else (not (Ch in '0'..'9'));
        -- Read the digit from file.
        Get(File, Ch);
        Append(Str, Ch);
      end loop;

      if Length(Str) < 2 then
        raise Time_Data_Error;
      end if;

      case I is
        when 1 =>
          Item.Hour := Integer'Value(To_String(Str));
        when 2 =>
          Item.Minute := Integer'Value(To_String(Str));
        when 3 =>
          Item.Second := Integer'Value(To_String(Str));
          -- All time interval is read except the decimal part (if exists).
          exit;
      end case;

      Look_Ahead(File, Ch, Eoln);
      if Eoln or else (Ch /= ':') then
        raise Time_Data_Error;
      end if;

      -- Clean ':' from file.
      Get(File, Ch);
    end loop;

    -- Decimal part of seconds (if exists) ...
    Decimals := 0.0;
    Look_Ahead(File, Ch, Eoln);
    if not Eoln and (Ch = '.') then
      Get(Ch);
      Ok := False;
      loop
        Look_Ahead(File, Ch, Eoln);
        exit when Eoln or else (not (Ch in '0'..'9'));
        -- Read the digit from file.
        Ok := True;
        Get(File, Ch);
        Decimals := (Decimals * 10.0) + Float(To_Integer(Ch));
      end loop;
      if not Ok then
        raise Time_Interval_Data_Error;
      end if;
      while Decimals > 1.0 loop
        Decimals := Decimals / 10.0;
      end loop;
    end if;
    Item.Decimals := Day_Duration(Decimals);
  end Get;

  -----------------------------------------------------------------------------
  --| Put, Put_Line
  -----------------------------------------------------------------------------

  procedure Put(Item : in Date_Type) is

  begin
    Put(Standard_Output, Item);
  end Put;
  -----------------------------------------------------------------------------
  procedure Put(File : in File_Type;
                Item : in Date_Type) is

  begin
    Put(File, To_String(Item));
  end Put;

  --===========================================================================
  procedure Put_Line(Item : in Date_Type) is

  begin
    Put_Line(Standard_Output, Item);
  end Put_Line;
  -----------------------------------------------------------------------------
  procedure Put_Line(File : in File_Type;
                     Item : in Date_Type) is

  begin
    Put(File, To_String(Item));
    New_Line(File);
  end Put_Line;

  --===========================================================================
  procedure Put(Item     : in Time_Type;
                Seconds  : in Boolean := True;
                Decimals : in Natural := 0) is

  begin
    Put(Standard_Output, Item, Seconds, Decimals);
  end Put;
  -----------------------------------------------------------------------------
  procedure Put(File     : in File_Type;
                Item     : in Time_Type;
                Seconds  : in Boolean := True;
                Decimals : in Natural := 0) is

  begin
    Put(File, To_String(Item, Seconds, Decimals));
  end Put;

  --===========================================================================
  procedure Put_Line(Item     : in Time_Type;
                     Seconds  : in Boolean := True;
                     Decimals : in Natural := 0) is

  begin
    Put_Line(Standard_Output, Item, Seconds, Decimals);
  end Put_Line;
  -----------------------------------------------------------------------------
  procedure Put_Line(File     : in File_Type;
                     Item     : in Time_Type;
                     Seconds  : in Boolean := True;
                     Decimals : in Natural := 0) is

  begin
    Put(File, Item, Seconds, Decimals);
    New_Line(File);
  end Put_Line;

  -----------------------------------------------------------------------------
  --| To_Date_And_Time_Type, To_Date_Type, To_Time_Type,
  --| To_String, To_Unbounded_String
  -----------------------------------------------------------------------------

  function To_Date_And_Time_Type(Date : in Date_Type := Null_Date;
                                 Time : in Time_Type := Null_Time)
           return Date_And_Time_Type is

  begin
    return (Date => Date,
            Time => Time);
  end To_Date_And_Time_Type;
  -----------------------------------------------------------------------------
  function To_Date_Type(Item : in String)
           return Date_Type is

    Date  : Date_Type;
    Blank : Boolean := True;
    Str   : String(1..10);

  begin
    for I in Item'Range loop
      if Item(I) /= ' ' then
        Blank := False;
        exit;
      end if;
    end loop;
    if Blank then
      return Null_Date;
    end if;

    if Item'Length /= 10 then
      raise Date_Data_Error;
    end if;

    Str := Item;
    begin
      Date.Year := Integer'Value(Str(1..4));
      Date.Month := Integer'Value(Str(6..7));
      Date.Day := Integer'Value(Str(9..10));
    exception
      when others =>
        raise Date_Data_Error;
    end;

    if not Legal_Date(Date) then
      raise Date_Data_Error;
    end if;

    return Date;
  end To_Date_Type;
  -----------------------------------------------------------------------------
  function To_Date_Type(Item : in Unbounded_String)
           return Date_Type is

    Date : Date_Type;

  begin
    return To_Date_Type(To_String(Item));
  end To_Date_Type;

  --===========================================================================
  function To_Time_Type(Item : in String)
           return Time_Type is

    Time     : Time_Type;
    Blank    : Boolean := True;
    Decimals : Float;
    Str      : String(1..Item'Length);

  begin
    for I in Item'Range loop
      if Item(I) /= ' ' then
        Blank := False;
        exit;
      end if;
    end loop;
    if Blank then
      return Null_Time;
    end if;

    if (Item'Length < 8) or (Item'Length = 9) then
      raise Time_Data_Error;
    end if;

    Str := Item;

    begin
      Time.Hour := Integer'Value(Str(1..2));
      Time.Minute := Integer'Value(Str(4..5));
      Time.Second := Integer'Value(Str(7..8));
      Decimals := 0.0;
      for I in reverse 10..Str'Length loop
        Decimals := (Decimals + Float'Value(Str(I..I))) / 10.0;
      end loop;
      Time.Decimals := Day_Duration(Decimals);
    exception
      when others =>
        raise Time_Data_Error;
    end;

    if not Legal_Time(Time) then
      raise Time_Data_Error;
    end if;

    return Time;
  end To_Time_Type;
  -----------------------------------------------------------------------------
  function To_Time_Type(Item : in Unbounded_String)
           return Time_Type is

    Time : Time_Type;

  begin
    return To_Time_Type(To_String(Item));
  end To_Time_Type;

  --===========================================================================
  function To_String(Item : in Date_Type)
           return String is

    Str : Unbounded_String;
    D_Str : String(1..10) := "0000-00-00";

  begin
    if Item = Null_Date then
      return D_Str;
    end if;

    if not Legal_Date(Item) then
      raise Date_Data_Error;
    end if;

    Str := To_Unbounded_String(Integer'Image(Item.Year));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    D_Str(5 - Length(Str)..4) := To_String(Str);

    Str := To_Unbounded_String(Integer'Image(Item.Month));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    D_Str(8 - Length(Str)..7) := To_String(Str);

    Str := To_Unbounded_String(Integer'Image(Item.Day));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    D_Str(11 - Length(Str)..10) := To_String(Str);

    return D_Str;
  end To_String;
  -----------------------------------------------------------------------------
  function To_String(Item     : in Time_Type;
                     Seconds  : in Boolean := True;
                     Decimals : in Natural := 0)
           return String is

  begin
    return To_String(To_Unbounded_String(Item, Seconds, Decimals));
  end To_String;

  --===========================================================================
  function To_Unbounded_String(Item : in Date_Type)
           return Unbounded_String is

  begin
    return To_Unbounded_String(To_String(Item));
  end To_Unbounded_String;
  -----------------------------------------------------------------------------
  function To_Unbounded_String(Item     : in Time_Type;
                               Seconds  : in Boolean := True;
                               Decimals : in Natural := 0)
           return Unbounded_String is

    Str : Unbounded_String;
    T_Str : Unbounded_String := Null_Unbounded_String;

  begin
    if Item = Null_Time then
      return To_Unbounded_String("00:00:00");
    end if;

    if not Legal_Time(Item) then
      raise Time_Data_Error;
    end if;

    Str := To_Unbounded_String(Integer'Image(Item.Hour));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    if Length(Str) = 1 then
      Str := To_Unbounded_String("0") & Str;
    end if;
    T_Str := Str & To_Unbounded_String(":");

    Str := To_Unbounded_String(Integer'Image(Item.Minute));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    if Length(Str) = 1 then
      Str := To_Unbounded_String("0") & Str;
    end if;
    T_Str := T_Str & Str;

    -- Check if no seconds are wanted.
    if not Seconds then
      return T_Str;
    end if;

    Str := To_Unbounded_String(Integer'Image(Item.Second));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    if Length(Str) = 1 then
      Str := To_Unbounded_String("0") & Str;
    end if;
    T_Str := T_Str & To_Unbounded_String(":") & Str;

    -- Check if no decimals are wanted.
    if Decimals = 0 then
      return T_Str;
    end if;

    if Item.Decimals > 0.0 then
      Str := To_Unbounded_String(Day_Duration'Image(Item.Decimals));
      -- Don't save '.' in 'Str' ('.' is at index 3 in 'Str').
      Str := To_Unbounded_String(Slice(Str, 4, Length(Str)));
    else
      Str := Null_Unbounded_String;
    end if;

    T_Str := T_Str & To_Unbounded_String(".");
    for D in 1..Decimals loop
      if D > Length(Str) then
        T_Str := T_Str & To_Unbounded_String("0");
      else
        T_Str := T_Str & To_Unbounded_String(Slice(Str, D, D));
      end if;
    end loop;

    return T_Str;
  end To_Unbounded_String;

  -----------------------------------------------------------------------------
  --| Get_Date, Get_Time
  -----------------------------------------------------------------------------

  function Get_Date(Date_And_Time : in Date_And_Time_Type)
           return Date_Type is

  begin
    return Date_And_Time.Date;
  end Get_Date;
  -----------------------------------------------------------------------------
  function Get_Time(Date_And_Time : in Date_And_Time_Type)
           return Time_Type is

  begin
    return Date_And_Time.Time;
  end Get_Time;

  -----------------------------------------------------------------------------
  --| Set
  -----------------------------------------------------------------------------

  procedure Set(Date_And_Time : in out Date_And_Time_Type;
                Date          : in     Date_Type := Null_Date;
                Time          : in     Time_Type := Null_Time) is

--      Changed : Boolean := False;

  begin
    if Date /= Null_Date then
      Date_And_Time.Date := Date;
--        Changed := True;
    end if;
    if Time /= Null_Time then
      Date_And_Time.Time := Time;
--        Changed := True;
    end if;
  end Set;

  -----------------------------------------------------------------------------
  --| Get_Xxx (where "Xxx" is Year, Month, Day, Hour, Minute or Second)
  -----------------------------------------------------------------------------

  function Get_Year(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Year(Date_And_Time.Date);
  end Get_Year;
  -----------------------------------------------------------------------------
  function Get_Month(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Month(Date_And_Time.Date);
  end Get_Month;
  -----------------------------------------------------------------------------
  function Get_Day(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Day(Date_And_Time.Date);
  end Get_Day;
  -----------------------------------------------------------------------------
  function Get_Hour(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Hour(Date_And_Time.Time);
  end Get_Hour;
  -----------------------------------------------------------------------------
  function Get_Minute(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Minute(Date_And_Time.Time);
  end Get_Minute;
  -----------------------------------------------------------------------------
  function Get_Second(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Second(Date_And_Time.Time);
  end Get_Second;
  -----------------------------------------------------------------------------
  function Get_Decimals(Date_And_Time : in Date_And_Time_Type) return Float is

  begin
    return Get_Decimals(Date_And_Time.Time);
  end Get_Decimals;

  -----------------------------------------------------------------------------
  function Get_Day_In_Year(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Day_In_Year(Date_And_Time.Date);
  end Get_Day_In_Year;
  -----------------------------------------------------------------------------
  function Get_Week_In_Year(Date_And_Time : in Date_And_Time_Type)
           return Integer is

  begin
    return Get_Week_In_Year(Date_And_Time.Date);
  end Get_Week_In_Year;
  -----------------------------------------------------------------------------
  function Get_Day_Of_Week(Date_And_Time : in Date_And_Time_Type)
           return Day_Of_Week_Type is

  begin
    return Get_Day_Of_Week(Date_And_Time.Date);
  end Get_Day_Of_Week;

  -----------------------------------------------------------------------------
  --| Get_Xxx (where "Xxx" is Year, Month or Day)
  -----------------------------------------------------------------------------

  function Get_Year(Date : in Date_Type)
           return Integer is


  begin
    return Date.Year;
  end Get_Year;
  -----------------------------------------------------------------------------
  function Get_Month(Date : in Date_Type)
           return Integer is


  begin
    return Date.Month;
  end Get_Month;
  -----------------------------------------------------------------------------
  function Get_Day(Date : in Date_Type)
           return Integer is


  begin
    return Date.Day;
  end Get_Day;

  -----------------------------------------------------------------------------
  function Get_Day_In_Year(Date : in Date_Type) return Integer is

  begin
    return Date_To_Julian(Date) - Date_To_Julian(Get_Year(Date), 1, 1) + 1;
  end Get_Day_In_Year;
  -----------------------------------------------------------------------------
  function Get_Week_In_Year(Date : in Date_Type) return Integer is

    Offset : Natural := Date_To_Julian(Get_Year(Date), 1, 1) mod 7;

  begin
    return 1 + ((Get_Day_In_Year(Date) - 1) + Offset) / 7;
  end Get_Week_In_Year;
  -----------------------------------------------------------------------------
  function Get_Day_Of_Week(Date : in Date_Type) return Day_Of_Week_Type is


  begin
    return Day_Of_Week_Type'Val(Date_To_Julian(Date) mod 7);
  end Get_Day_Of_Week;

  -----------------------------------------------------------------------------
  --| Get_Xxx (where "Xxx" is Hour, Minute or Second)
  -----------------------------------------------------------------------------

  function Get_Hour(Time : in Time_Type)
           return Integer is


  begin
    return Time.Hour;
  end Get_Hour;
  -----------------------------------------------------------------------------
  function Get_Minute(Time : in Time_Type)
           return Integer is


  begin
    return Time.Minute;
  end Get_Minute;
  -----------------------------------------------------------------------------
  function Get_Second(Time : in Time_Type)
           return Integer is


  begin
    return Time.Second;
  end Get_Second;
  -----------------------------------------------------------------------------
  function Get_Decimals(Time : in Time_Type) return Float is

  begin
    return Float(Time.Decimals);
  end Get_Decimals;

  -----------------------------------------------------------------------------
  --| Legal_Date, Legal_Time
  -----------------------------------------------------------------------------

  function Legal_Date(Item : in Date_Type)
           return Boolean is

  begin
    if (Item.Year = 0) or not ((Item.Month in 1..12) or
                               (Item.Day in 1..31)) then
      return False;
    end if;

    case Item.Month is
      when 2 =>
        return (Item.Day in 1..28) or else
               ((Item.Day = 29) and then
                ((Item.Year mod 400 = 0) or else
                 ((Item.Year mod 4 = 0) and then
                  not (Item.Year mod 100 = 0))));
      when 4 | 6 | 9 | 11 =>
        return (Item.Day in 1..30);
      when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
        return (Item.Day in 1..31);
      when others =>
        null;
    end case;

    return False;
  end Legal_Date;
  -----------------------------------------------------------------------------
  function Legal_Time(Item : in Time_Type)
           return Boolean is

  begin
    return ((Item.Hour in 0..23) and (Item.Minute in 0..59) and
            (Item.Second in 0..59) and (Item.Decimals < 1.0));
  end Legal_Time;

  -----------------------------------------------------------------------------
  --| Min and Max for dates/times/time intervals.
  -----------------------------------------------------------------------------

  function Min(A, B : in Date_Type) return Date_Type is
    
  begin
    if A = Null_Date then
      return B;
    elsif B = Null_Date then
      return A;
    end if;
    
    if A < B then
      return A;
    end if;
    return B;
  end Min;
  
  -----------------------------------------------------------------------------
  
  function Max(A, B : in Date_Type) return Date_Type is
    
  begin
    if A = Null_Date then
      return B;
    elsif B = Null_Date then
      return A;
    end if;
    
    if A > B then
      return A;
    end if;
    return B;
  end Max;

  -----------------------------------------------------------------------------
  
  function Min(A, B : in Time_Type) return Time_Type is
    
  begin
    if A = Null_Time then
      return B;
    elsif B = Null_Time then
      return A;
    end if;
    
    if A < B then
      return A;
    end if;
    return B;
  end Min;
  
  -----------------------------------------------------------------------------
  
  function Max(A, B : in Time_Type) return Time_Type is
    
  begin
    if A = Null_Time then
      return B;
    elsif B = Null_Time then
      return A;
    end if;
    
    if A > B then
      return A;
    end if;
    return B;
  end Max;

  -----------------------------------------------------------------------------
  
  function Min(A, B : in Time_Interval_Type) return Time_Interval_Type is
    
  begin
    if A = Null_Time_Interval then
      return B;
    elsif B = Null_Time_Interval then
      return A;
    end if;
    
    if A < B then
      return A;
    end if;
    return B;
  end Min;
  
  -----------------------------------------------------------------------------
  
  function Max(A, B : in Time_Interval_Type) return Time_Interval_Type is
    
  begin
    if A = Null_Time_Interval then
      return B;
    elsif B = Null_Time_Interval then
      return A;
    end if;
    
    if A > B then
      return A;
    end if;
    return B;
  end Max;

  -----------------------------------------------------------------------------
  --| Relation operators: "=", "<", ">", "<=", ">="
  --| For 'Date_Type', 'Time_Type' and 'Date_And_Time_Type'.
  -----------------------------------------------------------------------------

  function "="(Left, Right : in Date_Type)
           return Boolean is


  begin
    return ((Left.Year = Right.Year) and then
            (Left.Month = Right.Month) and then
            (Left.Day = Right.Day));
  end "=";
  -----------------------------------------------------------------------------
  function "<"(Left, Right : in Date_Type)
           return Boolean is


  begin
    if Left = Right then
      return False;
    end if;

    if Left.Year < Right.Year then
      return True;
    elsif Left.Year > Right.Year then
      return False;
    end if;

    if Left.Month < Right.Month then
      return True;
    elsif Left.Month > Right.Month then
      return False;
    end if;

    return Left.Day < Right.Day;
  end "<";
  -----------------------------------------------------------------------------
  function ">"(Left, Right : in Date_Type)
           return Boolean is


  begin
    return Right < Left;
  end ">";
  -----------------------------------------------------------------------------
  function "<="(Left, Right : in Date_Type)
           return Boolean is


  begin
    return not (Right < Left);
  end "<=";
  -----------------------------------------------------------------------------
  function ">="(Left, Right : in Date_Type)
           return Boolean is


  begin
    return not (Left < Right);
  end ">=";

  --===========================================================================
  function "="(Left, Right : in Time_Type)
           return Boolean is


  begin
    return To_Duration(Left) = To_Duration(Right);
  end "=";
  -----------------------------------------------------------------------------
  function "<"(Left, Right : in Time_Type)
           return Boolean is


  begin
    return To_Duration(Left) < To_Duration(Right);
  end "<";
  -----------------------------------------------------------------------------
  function ">"(Left, Right : in Time_Type)
           return Boolean is


  begin
    return Right < Left;
  end ">";
  -----------------------------------------------------------------------------
  function "<="(Left, Right : in Time_Type)
           return Boolean is


  begin
    return not (Right < Left);
  end "<=";
  -----------------------------------------------------------------------------
  function ">="(Left, Right : in Time_Type)
           return Boolean is


  begin
    return not (Left < Right);
  end ">=";

  --===========================================================================
  function "="(Left, Right : in Date_And_Time_Type)
           return Boolean is

  begin
    return (Left.Date = Right.Date) and then (Left.Time = Right.Time);
  end "=";
  -----------------------------------------------------------------------------
  function "<"(Left, Right : in Date_And_Time_Type)
           return Boolean is

  begin
    return ((Left.Date < Right.Date) or else
            ((Left.Date = Right.Date) and (Left.Time < Right.Time)));
  end "<";
  -----------------------------------------------------------------------------
  function ">"(Left, Right : in Date_And_Time_Type)
           return Boolean is

  begin
    return Right < Left;
  end ">";
  -----------------------------------------------------------------------------
  function "<="(Left, Right : in Date_And_Time_Type)
           return Boolean is

  begin
    return not (Right < Left);
  end "<=";
  -----------------------------------------------------------------------------
  function ">="(Left, Right : in Date_And_Time_Type)
           return Boolean is

  begin
    return not (Left < Right);
  end ">=";

  -----------------------------------------------------------------------------
  --| Operator "+" and "-"
  --| For 'Date_Type' combined with 'Integer'.
  -----------------------------------------------------------------------------

  function "+"(Left  : in Date_Type;
               Right : in Integer) return Date_Type is

  begin
    return Julian_To_Date(Date_To_Julian(Left) + Right);
  end "+";

  --===========================================================================
  function "-"(Left  : in Date_Type;
               Right : in Integer) return Date_Type is

  begin
    return Left + (-Right);
  end "-";

  -----------------------------------------------------------------------------
  --| Get
  -----------------------------------------------------------------------------

  procedure Get(Item : out Time_Interval_Type) is

  begin
    Get(Standard_Input, Item);
  end Get;
  -----------------------------------------------------------------------------
  procedure Get(File : in     File_Type;
                Item :    out Time_Interval_Type) is

    Str      : Unbounded_String;
    Ch       : Character;
    Ok       : Boolean;
    Eoln     : Boolean;
    Decimals : Float;

  begin
    for I in 1..3 loop
      Str := Null_Unbounded_String;
      loop
        Look_Ahead(File, Ch, Eoln);
        exit when Eoln or else (not (Ch in '0'..'9'));
        -- Read the digit from file.
        Get(File, Ch);
        Append(Str, Ch);
      end loop;

      if Length(Str) = 0 then
        raise Time_Interval_Data_Error;
      end if;

      case I is
        when 1 =>
          Item.Hours := Integer'Value(To_String(Str));
        when 2 =>
          Item.Minutes := Integer'Value(To_String(Str));
        when 3 =>
          Item.Seconds := Integer'Value(To_String(Str));
          -- All time interval is read except the decimal part (if exists).
          exit;
      end case;

      if Eoln or else (Ch /= ':') then
        raise Time_Interval_Data_Error;
      end if;

      -- Clean ':' from file.
      Get(File, Ch);
    end loop;

    -- Decimal part of seconds (if exists) ...
    Decimals := 0.0;
    Look_Ahead(File, Ch, Eoln);
    if not Eoln and (Ch = '.') then
      Get(Ch);
      Ok := False;
      loop
        Look_Ahead(File, Ch, Eoln);
        exit when Eoln or else (not (Ch in '0'..'9'));
        -- Read the digit from file.
        Ok := True;
        Get(File, Ch);
        Decimals := (Decimals * 10.0) + Float(To_Integer(Ch));
      end loop;
      if not Ok then
        raise Time_Interval_Data_Error;
      end if;
      while Decimals > 1.0 loop
        Decimals := Decimals / 10.0;
      end loop;
    end if;
    Item.Decimals := Day_Duration(Decimals);
  end Get;

  -----------------------------------------------------------------------------
  --| Put, Put_Line
  -----------------------------------------------------------------------------

  procedure Put(Item     : in Time_Interval_Type;
                Decimals : in Natural := 0) is

  begin
    Put(Standard_Output, Item, Decimals);
  end Put;
  -----------------------------------------------------------------------------
  procedure Put(File     : in File_Type;
                Item     : in Time_Interval_Type;
                Decimals : in Natural := 0) is

    Str        : Unbounded_String;
    Min_Length : Natural := Integer'Image(Item.Hours)'Length + 5;
    Temp       : Time_Interval_Type := (Hours    => Item.Hours,
                                        Minutes  => Item.Minutes,
                                        Seconds  => Item.Seconds,
                                        Decimals => 0.0);

  begin
    if Decimals = 0 then
      Put(File, To_String(Temp));
    else
      Str := To_Unbounded_String(Item);
      if Length(Str) = Min_Length then
        Append(Str, ".");
      end if;
      while Length(Str) < (Min_Length + Decimals) loop
        Append(Str, "0");
      end loop;
      Put(File, Slice(Str, 1, (Min_Length + 1 + Decimals)));
    end if;
  end Put;

  --===========================================================================
  procedure Put_Line(Item     : in Time_Interval_Type;
                     Decimals : in Natural := 0) is

  begin
    Put_Line(Standard_Output, Item, Decimals);
  end Put_Line;
  -----------------------------------------------------------------------------
  procedure Put_Line(File     : in File_Type;
                     Item     : in Time_Interval_Type;
                     Decimals : in Natural := 0) is

  begin
    Put(File, Item, Decimals);
    New_Line(File);
  end Put_Line;

  -----------------------------------------------------------------------------
  --| To_Time_Interval_Type, To_String, To_Unbounded_String
  -----------------------------------------------------------------------------

  function To_Time_Interval_Type(Item : in String)
           return Time_Interval_Type is

    Time_Interval : Time_Interval_Type;
    Blank         : Boolean := True;
    No_Of_Colons  : Natural := 0;
    No_Of_Dots    : Natural := 0;
    Colon_1_Index : Natural;
    Colon_2_Index : Natural;
    Dot_1_Index   : Natural;
    Decimals      : Float;

  begin
    for I in Item'Range loop
      if Item(I) /= ' ' then
        Blank := False;
        exit;
      end if;
    end loop;
    if Blank then
      return Null_Time_Interval;
    end if;

    for I in Item'Range loop
      if Item(I) = ':' then
        No_Of_Colons := No_Of_Colons + 1;
      elsif Item(I) = '.' then
        No_Of_Dots := No_Of_Dots + 1;
      elsif Item(I) in '0'..'9' then
        -- It must be digits so it's ok.
        null;
      else
        raise Time_Interval_Data_Error;
      end if;
    end loop;

    if (No_Of_Colons /= 2) or else (No_Of_Dots > 1) then
      raise Time_Interval_Data_Error;
    end if;

    Colon_1_Index := Index(Item, ":");
    Colon_2_Index := Index(Item((Colon_1_Index + 1) .. Item'Last), ":");

    if ((Colon_1_Index = 1) or else (Colon_2_Index = Item'Last) or else
        (Colon_2_Index - Colon_1_Index = 1)) then
      raise Time_Interval_Data_Error;
    end if;

    if No_Of_Dots > 0 then
       Dot_1_Index := Index(Item, ".");
       if ((Dot_1_Index < Colon_2_Index + 2) or else
           (Dot_1_Index = Item'Last)) then
          raise Time_Interval_Data_Error;
       end if;
    end if;

    Time_Interval.Hours := Integer'Value(Item(1 .. (Colon_1_Index - 1)));
    Time_Interval.Minutes := Integer'Value(Item((Colon_1_Index + 1) ..
                                                (Colon_2_Index - 1)));
    if No_Of_Dots = 0 then
       Time_Interval.Seconds := Integer'Value(Item((Colon_2_Index + 1) ..
                                                   Item'Last));
       Time_Interval.Decimals := 0.0;
    else
       Time_Interval.Seconds := Integer'Value(Item((Colon_2_Index + 1) ..
                                                   (Dot_1_Index - 1)));
       Decimals := 0.0;
       for I in reverse Dot_1_Index + 1..Item'Last loop
          Decimals := (Decimals + Float'Value(Item(I..I))) / 10.0;
       end loop;
       Time_Interval.Decimals := Day_Duration(Decimals);
    end if;

    if Time_Interval.Seconds > 60 then
      Time_Interval.Minutes := (Time_Interval.Minutes +
                                (Time_Interval.Seconds / 60));
      Time_Interval.Seconds := Time_Interval.Seconds mod 60;
    end if;
    if Time_Interval.Minutes > 60 then
      Time_Interval.Hours := (Time_Interval.Hours +
                              (Time_Interval.Minutes / 60));
      Time_Interval.Minutes := Time_Interval.Minutes mod 60;
    end if;

    return Time_Interval;
  end To_Time_Interval_Type;
  -----------------------------------------------------------------------------
  function To_Time_Interval_Type(Item : in Unbounded_String)
           return Time_Interval_Type is

    Time_Interval : Time_Interval_Type;

  begin
    return To_Time_Interval_Type(To_String(Item));
  end To_Time_Interval_Type;

  --===========================================================================
  function To_String(Item : in Time_Interval_Type)
           return String is

  begin
    return To_String(To_Unbounded_String(Item));
  end To_String;

  --===========================================================================
  function To_Unbounded_String(Item : in Time_Interval_Type)
           return Unbounded_String is

    Str : Unbounded_String;
    T_Str : Unbounded_String;

  begin
    if Item = Null_Time_Interval then
      return To_Unbounded_String("00:00:00");
    end if;

    if not Legal_Time_Interval(Item) then
      raise Time_Interval_Data_Error;
    end if;

    Str := To_Unbounded_String(Integer'Image(Item.Hours));
    Str := To_Unbounded_String(Slice(Str, 2, Length(str)));
    T_Str := T_Str & Str & To_Unbounded_String(":");

    Str := To_Unbounded_String(Integer'Image(Item.Minutes));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    if Length(Str) = 1 then
      Str := To_Unbounded_String("0") & Str;
    end if;
    T_Str := T_Str & Str & To_Unbounded_String(":");

    Str := To_Unbounded_String(Integer'Image(Item.Seconds));
    Str := To_Unbounded_String(Slice(Str, 2, Length(Str)));
    if Length(Str) = 1 then
      Str := To_Unbounded_String("0") & Str;
    end if;
    T_Str := T_Str & Str;

    if Item.Decimals > 0.0 then
      Str := To_Unbounded_String(Day_Duration'Image(Item.Decimals));
      Str := To_Unbounded_String(Slice(Str, 3, Length(Str)));
      T_Str := T_Str & Str;  -- Includes character '.' before decimal part.
    end if;

    return T_Str;
  end To_Unbounded_String;

  -----------------------------------------------------------------------------
  --| Get_Xxx (where "Xxx" is Hours, Minutes or Seconds)
  -----------------------------------------------------------------------------

  function Get_Hours(Time_Interval : in Time_Interval_Type)
           return Integer is


  begin
    return Time_Interval.Hours;
  end Get_Hours;
  -----------------------------------------------------------------------------
  function Get_Minutes(Time_Interval : in Time_Interval_Type)
           return Integer is


  begin
    return Time_Interval.Minutes;
  end Get_Minutes;
  -----------------------------------------------------------------------------
  function Get_Seconds(Time_Interval : in Time_Interval_Type)
           return Integer is


  begin
    return Time_Interval.Seconds;
  end Get_Seconds;
  -----------------------------------------------------------------------------
  function Get_Decimals(Time_Interval : in Time_Interval_Type) return Float is

  begin
    return Float(Time_Interval.Decimals);
  end Get_Decimals;

  -----------------------------------------------------------------------------
  --| Legal_Time_Interval
  -----------------------------------------------------------------------------

  function Legal_Time_Interval(Item : in Time_Interval_Type)
           return Boolean is

  begin
    return ((Item.Hours >= 0) or (Item.Minutes in 0..59) or
            (Item.Seconds in 0..59));
  end Legal_Time_Interval;

  -----------------------------------------------------------------------------
  --| Operator "+" and "-"
  --| For 'Time_Type' combined with 'Time_Interval_Type'.
  -----------------------------------------------------------------------------

  function "+"(Left  : in Time_Type;
               Right : in Time_Interval_Type)
           return Time_Type is

    Temp : Time_Type;
    X    : Natural;

  begin
    Temp.Decimals := Left.Decimals + Right.Decimals;
    X := Integer(Float'Floor(Float(Temp.Decimals)));
    Temp.Decimals := Temp.Decimals - Day_Duration(X);

    Temp.Second := Left.Second + Right.Seconds + X;
    X := Temp.Second / 60;
    Temp.Second := Temp.Second mod 60;

    Temp.Minute := Left.Minute + Right.Minutes + X;
    X := Temp.Minute / 60;
    Temp.Minute := Temp.Minute mod 60;

    Temp.Hour := (Left.Hour + Right.Hours + X) mod 24;

    return Temp;
  end "+";

  --===========================================================================
  function "-"(Left  : in Time_Type;
               Right : in Time_Interval_Type)
           return Time_Type is

    Temp : Time_Type := Left;

  begin
    if Left.Decimals < Right.Decimals then
      Temp.Decimals := Left.Decimals + 1.0 - Right.Decimals;
      Temp.Second := Left.Second - 1 - Right.Seconds;
    else
      Temp.Decimals := Left.Decimals - Right.Decimals;
      Temp.Second := Left.Second - Right.Seconds;
    end if;
    Temp.Minute := Left.Minute - Right.Minutes;
    if Temp.Second < 0 then
      Temp.Second := Temp.Second + 60;
      Temp.Minute := Temp.Minute - 1;
    end if;
    Temp.Hour := Left.Hour - Right.Hours;
    if Temp.Minute < 0 then
      Temp.Minute := Temp.Minute + 60;
      Temp.Hour := Temp.Hour - 1;
    end if;
    -- Can be many hours (even many days ...).
    while Temp.Hour < 0 loop
      Temp.Hour := Temp.Hour + 24;
    end loop;

    return Temp;
  end "-";

  -----------------------------------------------------------------------------
  --| Operator "+" and "-"
  --| For 'Time_Interval_Type'.
  -----------------------------------------------------------------------------

  function "+"(Left, Right : in Time_Interval_Type)
           return Time_Interval_Type is

    Temp : Time_Interval_Type;
    X    : Natural;

  begin
    Temp.Decimals := Left.Decimals + Right.Decimals;
    X := Integer(Float'Floor(Float(Temp.Decimals)));
    Temp.Decimals := Temp.Decimals - Day_Duration(X);

    Temp.Seconds := Left.Seconds + Right.Seconds + X;
    X := Temp.Seconds / 60;
    Temp.Seconds := Temp.Seconds mod 60;

    Temp.Minutes := Left.Minutes + Right.Minutes + X;
    X := Temp.Minutes / 60;
    Temp.Minutes := Temp.Minutes mod 60;

    Temp.Hours := Left.Hours + Right.Hours + X;

    return Temp;
  end "+";

  --===========================================================================
  function "-"(Left, Right : in Time_Interval_Type)
           return Time_Interval_Type is

    Temp : Time_Interval_Type;

  begin
    if Left < Right then
      raise Time_Interval_Value_Error;
    end if;

    if Left.Decimals < Right.Decimals then
      Temp.Decimals := Left.Decimals + 1.0 - Right.Decimals;
      Temp.Seconds := Left.Seconds - 1 - Right.Seconds;
    else
      Temp.Decimals := Left.Decimals - Right.Decimals;
      Temp.Seconds := Left.Seconds - Right.Seconds;
    end if;
    Temp.Minutes := Left.Minutes - Right.Minutes;
    if Temp.Seconds < 0 then
      Temp.Seconds := Temp.Seconds + 60;
      Temp.Minutes := Temp.Minutes - 1;
    end if;
    Temp.Hours := Left.Hours - Right.Hours;
    if Temp.Minutes < 0 then
      Temp.Minutes := Temp.Minutes + 60;
      Temp.Hours := Temp.Hours - 1;
    end if;

    return Temp;
  end "-";

  -----------------------------------------------------------------------------
  --| Operator "*" and "/"
  --| For 'Time_Interval_Type'.
  -----------------------------------------------------------------------------

  function "*"(Left  : in Time_Interval_Type;
               Right : in Integer)
           return Time_Interval_Type is

  begin
    return Left * Float(Right);
  end "*";
  -----------------------------------------------------------------------------
  function "*"(Left  : in Time_Interval_Type;
               Right : in Float)
           return Time_Interval_Type is

  begin
    return To_Time_Interval_Type(Duration(Float(To_Duration(Left)) * Right));
  end "*";

  --===========================================================================
  function "/"(Left  : in Time_Interval_Type;
               Right : in Integer)
           return Time_Interval_Type is

  begin
    return Left / Float(Right);
  end "/";
  -----------------------------------------------------------------------------
  function "/"(Left  : in Time_Interval_Type;
               Right : in Float)
           return Time_Interval_Type is

  begin
    if Right = 0.0 then
      raise Time_Zero_Division_Error;
    end if;

    return To_Time_Interval_Type(Duration(Float(To_Duration(Left)) / Right));
  end "/";

  -----------------------------------------------------------------------------
  --| Operator "-"
  --| For 'Time_Type'.
  -----------------------------------------------------------------------------

  function "-"(Left, Right : in Time_Type)
           return Time_Interval_Type is

    Temp_Left  : Time_Interval_Type;
    Temp_Right : Time_Interval_Type;

  begin
    if Left < Right then
      Temp_Left.Hours := Left.Hour + 24;
    else
      Temp_Left.Hours := Left.Hour;
    end if;
    Temp_Left.Minutes := Left.Minute;
    Temp_Left.Seconds := Left.Second;
    Temp_Left.Decimals := Left.Decimals;

    Temp_Right.Hours := Right.Hour;
    Temp_Right.Minutes := Right.Minute;
    Temp_Right.Seconds := Right.Second;
    Temp_Right.Decimals := Right.Decimals;

    return Temp_Left - Temp_Right;
  end "-";

  -----------------------------------------------------------------------------
  --| Relation operators: "=", "<", ">", "<=", ">="
  --| For 'Time_Interval_Type'.
  -----------------------------------------------------------------------------

  function "="(Left, Right : in Time_Interval_Type)
           return Boolean is


  begin
    return ((Left.Hours = Right.Hours) and then
            (Left.Minutes = Right.Minutes) and then
            (Left.Seconds = Right.Seconds) and then
            (Left.Decimals = Right.Decimals));
  end "=";
  -----------------------------------------------------------------------------
  function "<"(Left, Right : in Time_Interval_Type)
           return Boolean is


  begin
    if Left = Right then
      return False;
    end if;

    if Left.Hours < Right.Hours then
      return True;
    elsif Left.Hours > Right.Hours then
      return False;
    end if;

    if Left.Minutes < Right.Minutes then
      return True;
    elsif Left.Minutes > Right.Minutes then
      return False;
    end if;

    if Left.Seconds < Right.Seconds then
      return True;
    elsif Left.Seconds > Right.Seconds then
      return False;
    end if;

    return Left.Decimals < Right.Decimals;
  end "<";
  -----------------------------------------------------------------------------
  function ">"(Left, Right : in Time_Interval_Type)
           return Boolean is


  begin
    return Right < Left;
  end ">";
  -----------------------------------------------------------------------------
  function "<="(Left, Right : in Time_Interval_Type)
           return Boolean is


  begin
    return not (Right < Left);
  end "<=";
  -----------------------------------------------------------------------------
  function ">="(Left, Right : in Time_Interval_Type)
           return Boolean is


  begin
    return not (Left < Right);
  end ">=";


  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| To_Time_Type, To_Time_Interval_Type, To_Duration
  -----------------------------------------------------------------------------
  function To_Time_Type(Item : in Duration)
           return Time_Type is

    S : Integer := Integer(Float'Floor(Float(Item)));

  begin
--     begin
      -- NYI: Error message: CONSTRAINT_ERROR at marked (*) row.
      return (Hour     => (S / 3600) mod 24,
              Minute   => ((S mod 3600) / 60),
              Second   => (S mod 60),
              Decimals => Day_Duration(Item - Duration(S)));  -- (*)
--     exception
--        when others =>
--          -- NYI: This is a fix ...
--          Put_Line("Internal error in To_Time_Type: sets decimals to zero.");
--          return (Hour     => (S / 3600) mod 24,
--                  Minute   => ((S mod 3600) / 60),
--                  Second   => (S mod 60),
--                  Decimals => 0.0);
--     end;
  end To_Time_Type;
  -----------------------------------------------------------------------------
  function To_Time_Type(Item : in Time_Interval_Type)
           return Time_Type is

  begin
    return To_Time_Type(To_Duration(Item));
  end To_Time_Type;

  --===========================================================================
  function To_Time_Interval_Type(Item : in Duration)
           return Time_Interval_Type is

    S : Integer := Integer(Float'Floor(Float(Item)));

  begin
    return (Hours    => (S / 3600),
            Minutes  => ((S mod 3600) / 60),
            Seconds  => (S mod 60),
            Decimals => Day_Duration(Item - Duration(S)));
  end To_Time_Interval_Type;
  -----------------------------------------------------------------------------
  function To_Time_Interval_Type(Item : in Time_Type)
           return Time_Interval_Type is

  begin
    return To_Time_Interval_Type(To_Duration(Item));
  end To_Time_Interval_Type;

  --===========================================================================
  function To_Duration(Item : in Time_Type)
           return Duration is

  begin
    return (Duration(Item.Hour * 3600 + Item.Minute * 60 + Item.Second) +
            Item.Decimals);
  end To_Duration;
  -----------------------------------------------------------------------------
  function To_Duration(Item : in Time_Interval_Type)
           return Duration is

  begin
    return (Duration(Item.Hours * 3600 + Item.Minutes * 60 + Item.Seconds) +
            Item.Decimals);
  end To_Duration;

  -----------------------------------------------------------------------------
  --| Date_To_Julian, Julian_To_Date
  -----------------------------------------------------------------------------
  function Date_To_Julian(Year, Month, Day : in Integer) return Integer is

    Y : Integer := Year;
    M : Integer := Month;
    D : Integer := Day;

    C  : Integer;
    Ya : Integer;

  begin
--           if M < 1 or M > 12 then
--                    raise Invalid_Month;
--           end if;
--           if D < 1 or D > 31 then
--                    raise Invalid_Day;
--           end if;

    if M > 2 then
      M := M - 3;
    else
      M := M + 9;
      Y  := Y - 1;
    end if;

    C  := Y / 100;
    Ya := Y - (100 * C);

    return ((146_097 * C) / 4 + (1_461 * Ya) / 4 +
            (153 * M + 2) / 5 + D + 1_721_119);
  end Date_To_Julian;
  -----------------------------------------------------------------------------
  function Date_To_Julian(Item : in Date_Type) return Integer is

  begin
    return Date_To_Julian(Get_Year(Item), Get_Month(Item), Get_Day(Item));
  end Date_To_Julian;

  --===========================================================================
  procedure Julian_To_Date(Item  : in     Integer;
                           Year  :    out Integer;
                           Month :    out Integer;
                           Day   :    out Integer) is

    J : Integer;
    L : Integer;

  begin
--       if J < 0 then
--            raise Invalid_Date;
--       end if;

    J := Item - 1_721_119;

    Year := (4 * J - 1) / 146_097;
    J := 4 * J - 1 - 146_097 * Year;
    L := J / 4;
    J := (4 * L + 3) / 1461;
    L := 4 * L + 3 - 1461 * J;
    L := (L + 4) / 4;
    Month := (5 * L - 3) / 153;
    L := 5 * L - 3 - 153 * Month;
    Day := (L + 5) / 5;
    Year := 100 * Year + J;

    if (Month < 10) then
      Month := Month + 3;
    else
      Month := Month - 9;
      Year := Year + 1;
    end if;
  end Julian_To_Date;
  -----------------------------------------------------------------------------
  function Julian_To_Date(Item : in Integer) return Date_Type is

    Date : Date_Type;

  begin
    Julian_To_Date(Item, Date.Year, Date.Month, Date.Day);
    return Date;
  end Julian_To_Date;


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Calendar;
