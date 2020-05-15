-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                         T J A . K E Y B O A R D                         |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  2.01                              |--
--|                                                                         |--
--|                           (C) Copyright, 2000                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2000-03-10  Version 2.01 is ok.                                       |--
--|               "Is_Meta" functions inserted.                             |--
--|   2000-02-25  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;            use Ada.Characters.Latin_1;

-- External libraries.
with Gnat.Os_Lib;                       use Gnat.Os_Lib;

-- Internal libraries.

-------------------------------------------------------------------------------
package body TJa.Keyboard is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| "Special_Key_Repr" contains all the key codes for the special keys.
  -----------------------------------------------------------------------------

  Special_Key_Repr  : array (0..40) of Unbounded_String;

  -----------------------------------------------------------------------------
  --| "Current_Echo_Mode" are global in this library and gives information
  --| about how the mode for echo to screen is set.
  --|
  --| "Args_Echo" and "Args_No_Echo" is used for set echo on or off. These are
  --| also global.
  -----------------------------------------------------------------------------

  Current_Echo_Mode : Echo_Mode_Type := On;
  Args_Echo         : Argument_List(1..1);
  Args_No_Echo      : Argument_List(1..1);

  -----------------------------------------------------------------------------
  --| "Current_Buffer_Mode" are global in this library and gives information
  --| about how the mode for buffering input from keyboard is set.
  --|
  --| "Args_Buffer" and "Args_No_Buffer" is used for set buffer on or off.
  --| These are also global.
  -----------------------------------------------------------------------------

  Current_Buffer_Mode : Buffer_Mode_Type := On;
  Args_Buffer         : Argument_List(1..1);
  Args_No_Buffer      : Argument_List(1..1);

  -----------------------------------------------------------------------------
  --| "To_String_Access" is used to convert strings to access types for use in
  --| argument lists.
  -----------------------------------------------------------------------------
  function To_String_Access(S : in String) return Gnat.Os_Lib.String_Access;

  -----------------------------------------------------------------------------
  --| "To_Key_Code" and "To_Key_String" is used to convert between key name
  --| strings and the internal key codes.
  -----------------------------------------------------------------------------

  function To_Key_Code(Key_Str : String) return Natural;
  function To_Key_String(Key_Code : Natural) return String;

  -----------------------------------------------------------------------------
  --| "Get_Immediate" is used to get a key stroke. If 'Wait = True' then the
  --| mtehod waits for a key stroke if buffer is empty. 'Available' returns the
  --| value 'True' if a key is read.
  -----------------------------------------------------------------------------

  procedure Get_Immediate(Item      :    out Key_Type;
                          Available :    out Boolean;
                          Wait      : in     Boolean);


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Set_Echo_Mode, Get_Echo_Mode
  -----------------------------------------------------------------------------

  procedure Set_Echo_Mode(Mode : in Echo_Mode_Type := On) is

    Ok : Boolean;  -- NYI: Not used yet.

  begin
    if (Mode /= Current_Echo_Mode) then
      case Mode is
        when On  => Spawn("/bin/stty", Args_Echo, Ok);
        when Off => Spawn("/bin/stty", Args_No_Echo, Ok);
      end case;
      Current_Echo_Mode := Mode;
    end if;

  exception
    when E : others =>
      Put_Line("Internal Error in 'Set_Echo_Mode'");
      raise;
  end Set_Echo_Mode;
  -----------------------------------------------------------------------------
  function Get_Echo_Mode
           return Echo_Mode_Type is

  begin
    return Current_Echo_Mode;

  exception
    when E : others =>
      Put_Line("Internal Error in 'Get_Echo_Mode'");
      raise;
  end Get_Echo_Mode;

  -----------------------------------------------------------------------------
  --| Set_Buffer_Mode, Get_Buffer_Mode
  -----------------------------------------------------------------------------

  procedure Set_Buffer_Mode(Mode : in Buffer_Mode_Type := On) is

    Ok : Boolean;  -- NYI: Not used yet.

  begin
    if (Mode /= Current_Buffer_Mode) then
      case Mode is
        when On  => Spawn("/bin/stty", Args_Buffer, Ok);
        when Off => Spawn("/bin/stty", Args_No_Buffer, Ok);
      end case;
      Current_Buffer_Mode := Mode;
    end if;

  exception
    when E : others =>
      Put_Line("Internal Error in 'Set_Buffer_Mode'");
      raise;
  end Set_Buffer_Mode;
  -----------------------------------------------------------------------------
  function Get_Buffer_Mode
           return Buffer_Mode_Type is

  begin
    return Current_Buffer_Mode;

  exception
    when E : others =>
      Put_Line("Internal Error in 'Get_Buffer_Mode'");
      raise;
  end Get_Buffer_Mode;

  -----------------------------------------------------------------------------
  --| Get_Immediate
  -----------------------------------------------------------------------------

  procedure Get_Immediate(Item : out Key_Type) is

    Available : Boolean;

  begin
    Get_Immediate(Item, Available, Wait => True);

  exception
    when E : others =>
      Put_Line("Internal Error in 'Get_Immediate'");
      raise;
  end Get_Immediate;
  -----------------------------------------------------------------------------
  procedure Get_Immediate(Item      : out Key_Type;
                          Available : out Boolean) is

  begin
    Get_Immediate(Item, Available, Wait => False);

  exception
    when E : others =>
      Put_Line("Internal Error in 'Get_Immediate'");
      raise;
  end Get_Immediate;

  -----------------------------------------------------------------------------
  --| Put, Put_Line
  -----------------------------------------------------------------------------

  procedure Put(Item : in Key_Type) is

  begin
    Put(To_String(Item));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Put'");
      raise;
  end Put;
  -----------------------------------------------------------------------------
  procedure Put_Line(Item : in Key_Type) is

  begin
    Put_Line(To_String(Item));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Put_Line'");
      raise;
  end Put_Line;

  -----------------------------------------------------------------------------
  --| To_Character, To_String
  -----------------------------------------------------------------------------

  function To_Character(Source : in Key_Type)
           return Character is

  begin
    return Ada.Strings.Unbounded.Element(Source.Key, 1);

  exception
    when E : others =>
      Put_Line("Internal Error in 'To_Character'");
      raise;
  end To_Character;

  --===========================================================================
  function To_String(Source : in Key_Type)
           return String is

  begin
    return To_String(To_Unbounded_String(Source));
  end To_String;

  --===========================================================================
  function To_Unbounded_String(Source : in Key_Type)
           return Unbounded_String is

    Str : Unbounded_String := To_Unbounded_String("");

  begin
    -- Special cases.
    if Source = Null_Key then
      return To_Unbounded_String("");
    elsif Source = Undefined_Key then
      return To_Unbounded_String("Strange *key*!");
    end if;

    -- Normal cases.
    if (Is_Character(Source)) then
      -- Single character.
      Append(Str, To_Character(Source));
    elsif (Is_Control(Source)) then
      -- Control character.
      Append(Str, "Ctrl-");
      Append(Str, Character'Val(Character'Pos('A') - 1 +
                                Character'Pos(To_Character(Source))));
    elsif (Is_Special(Source)) then
      -- Simple special key?
      if (Is_Return(Source)) then
        Append(Str, "Return");
      elsif (Is_Delete(Source)) then
        Append(Str, "Delete");
      elsif (Is_Backspace(Source)) then
        Append(Str, "Backspace");
      elsif (Is_Tab(Source)) then
        Append(Str, "Tab");
      elsif (Is_Esc(Source)) then
        Append(Str, "Esc");
      -- Meta key?
      elsif (Is_Meta_Backspace(Source)) then
        Append(Str, "Meta_Backspace");
      elsif (Is_Meta_Tab(Source)) then
        Append(Str, "Meta_Tab");
      elsif (Is_Meta_Return(Source)) then
        Append(Str, "Meta_Return");
      elsif (Is_Meta_Esc(Source)) then
        Append(Str, "Meta_Esc");
      else
        -- Function key, Arrow key, Home, End, PgUp, PgDn, Insert, Abort,
        -- Again, Properties, Undo, Front, Copy, Open, Paste, Search or Cut.
        for I in Special_Key_Repr'Range loop
          if (Source.Key = Special_Key_Repr(I)) then
            Append(Str, To_Key_String(I));
          end if;
        end loop;
      end if;
    else
      -- Some strange key! Possible that it's an undefined key.
      Append(Str, "Strange *key*!");
    end if;

    return Str;

  exception
    when E : others =>
      Put_Line("Internal Error in 'To_String'");
      raise;
  end To_Unbounded_String;

  -----------------------------------------------------------------------------
  --| Is_Character, Is_Control, Is_Special, Is_Function, Is_Arrow, Is_Meta
  -----------------------------------------------------------------------------

  function Is_Character(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            ((Character'Pos(To_Character(Item)) in 32..126) or
             (Character'Pos(To_Character(Item)) in 160..255)));  -- All?

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Character'");
      raise;
  end Is_Character;

  --===========================================================================
  function Is_Control(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_Character(Item)) in 0..26));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Control'");
      raise;
  end Is_Control;

  --===========================================================================
  function Is_Special(Item : in Key_Type)
           return Boolean is

    Simple_Answer   : Boolean;
    Advanced_Answer : Boolean;

  begin
    Simple_Answer := (Is_Return(Item) or else Is_Delete(Item) or else
                      Is_Backspace(Item) or else Is_Tab(Item) or else
                      Is_Esc(Item));

    Advanced_Answer := False;
    if (Length(Item.Key) > 1) then
      for I in Special_Key_Repr'Range loop
        Advanced_Answer := Advanced_Answer or (Item.Key = Special_Key_Repr(I));
      end loop;
    end if;

    return Simple_Answer or Advanced_Answer or Is_Meta(Item);

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Special'");
      raise;
  end Is_Special;

  --===========================================================================
  function Is_Function(Item : in Key_Type)
           return Boolean is

  begin
    for I in To_Key_Code("F1")..To_Key_Code("F12") loop
      if (Item.Key = Special_Key_Repr(I)) then
        return True;
      end if;
    end loop;

    return False;

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Function'");
      raise;
  end Is_Function;

  --===========================================================================
  function Is_Arrow(Item : in Key_Type)
           return Boolean is

  begin
    return Is_Up_Arrow(Item) or else Is_Down_Arrow(Item) or else
           Is_Left_Arrow(Item) or else Is_Right_Arrow(Item);

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Arrow'");
      raise;
  end Is_Arrow;

  --===========================================================================
  function Is_Meta(Item : in Key_Type)
           return Boolean is

  begin
    return Is_Meta_Tab(Item) or else Is_Meta_Esc(Item) or else
           Is_Meta_Backspace(Item) or else Is_Meta_Return(Item);

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Meta'");
      raise;
  end Is_Meta;

  -----------------------------------------------------------------------------
  --| Is_Ctrl_Xxx (all control keys by name)
  -----------------------------------------------------------------------------

  function Is_Ctrl_Space(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 0));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_Space'");
      raise;
  end Is_Ctrl_Space;
  -----------------------------------------------------------------------------
  function Is_Ctrl_A(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 1));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_A'");
      raise;
  end Is_Ctrl_A;
  -----------------------------------------------------------------------------
  function Is_Ctrl_B(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 2));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_B'");
      raise;
  end Is_Ctrl_B;
  -----------------------------------------------------------------------------
  function Is_Ctrl_C(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 3));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_C'");
      raise;
  end Is_Ctrl_C;
  -----------------------------------------------------------------------------
  function Is_Ctrl_D(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 4));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_D'");
      raise;
  end Is_Ctrl_D;
  -----------------------------------------------------------------------------
  function Is_Ctrl_E(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 5));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_E'");
      raise;
  end Is_Ctrl_E;
  -----------------------------------------------------------------------------
  function Is_Ctrl_F(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 6));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_F'");
      raise;
  end Is_Ctrl_F;
  -----------------------------------------------------------------------------
  function Is_Ctrl_G(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 7));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_G'");
      raise;
  end Is_Ctrl_G;
  -----------------------------------------------------------------------------
  function Is_Ctrl_H(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 8));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_H'");
      raise;
  end Is_Ctrl_H;
  -----------------------------------------------------------------------------
  function Is_Ctrl_I(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 9));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_I'");
      raise;
  end Is_Ctrl_I;
  -----------------------------------------------------------------------------
  function Is_Ctrl_J(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 10));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_J'");
      raise;
  end Is_Ctrl_J;
  -----------------------------------------------------------------------------
  function Is_Ctrl_K(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 11));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_K'");
      raise;
  end Is_Ctrl_K;
  -----------------------------------------------------------------------------
  function Is_Ctrl_L(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 12));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_L'");
      raise;
  end Is_Ctrl_L;
  -----------------------------------------------------------------------------
  function Is_Ctrl_M(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 13));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_M'");
      raise;
  end Is_Ctrl_M;
  -----------------------------------------------------------------------------
  function Is_Ctrl_N(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 14));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_N'");
      raise;
  end Is_Ctrl_N;
  -----------------------------------------------------------------------------
  function Is_Ctrl_O(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 15));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_O'");
      raise;
  end Is_Ctrl_O;
  -----------------------------------------------------------------------------
  function Is_Ctrl_P(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 16));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_P'");
      raise;
  end Is_Ctrl_P;
  -----------------------------------------------------------------------------
  function Is_Ctrl_Q(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 17));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_Q'");
      raise;
  end Is_Ctrl_Q;
  -----------------------------------------------------------------------------
  function Is_Ctrl_R(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 18));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_R'");
      raise;
  end Is_Ctrl_R;
  -----------------------------------------------------------------------------
  function Is_Ctrl_S(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 19));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_S'");
      raise;
  end Is_Ctrl_S;
  -----------------------------------------------------------------------------
  function Is_Ctrl_T(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 20));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_T'");
      raise;
  end Is_Ctrl_T;
  -----------------------------------------------------------------------------
  function Is_Ctrl_U(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 21));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_U'");
      raise;
  end Is_Ctrl_U;
  -----------------------------------------------------------------------------
  function Is_Ctrl_V(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 22));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_V'");
      raise;
  end Is_Ctrl_V;
  -----------------------------------------------------------------------------
  function Is_Ctrl_W(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 23));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_W'");
      raise;
  end Is_Ctrl_W;
  -----------------------------------------------------------------------------
  function Is_Ctrl_X(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 24));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_X'");
      raise;
  end Is_Ctrl_X;
  -----------------------------------------------------------------------------
  function Is_Ctrl_Y(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 25));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_Y'");
      raise;
  end Is_Ctrl_Y;
  -----------------------------------------------------------------------------
  function Is_Ctrl_Z(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            (Character'Pos(To_String(Item.Key)(1)) = 26));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Ctrl_Z'");
      raise;
  end Is_Ctrl_Z;

  -----------------------------------------------------------------------------
  --| Is_Return, Is_Delete, Is_Backspace, Is_Tab, Is_Esc, Is_Home, Is_End,
  --| Is_PgUp, Is_PgDn, Is_Insert, Is_Abort, Is_Properties, Is_Front, Is_Open,
  --| Is_Search, Is_Again, Is_Undo, Is_Copy, Is_Paste, Is_Cut
  -----------------------------------------------------------------------------

  function Is_Return(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then
            ((Ascii.CR = To_Character(Item)) or else
             (Ascii.LF = To_Character(Item))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Return'");
      raise;
  end Is_Return;

  --===========================================================================
  function Is_Delete(Item : in Key_Type)
           return Boolean is

  begin
    return (((Length(Item.Key) = 1) and then
             (Ascii.DEL = To_Character(Item))) or else
            ((Length(Item.Key) > 1) and then
             (Item.Key = Special_Key_Repr(To_Key_Code("Delete")))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Delete'");
      raise;
  end Is_Delete;
  -----------------------------------------------------------------------------
  function Is_Backspace(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then (Ascii.BS = To_Character(Item)));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Backspace'");
      raise;
  end Is_Backspace;

  --===========================================================================
  function Is_Tab(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then (Ascii.HT = To_Character(Item)));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Tab'");
      raise;
  end Is_Tab;

  --===========================================================================
  function Is_Esc(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) = 1) and then (Ascii.ESC = To_Character(Item)));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Esc'");
      raise;
  end Is_Esc;

  --===========================================================================
  function Is_Home(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Home"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Home'");
      raise;
  end Is_Home;
  -----------------------------------------------------------------------------
  function Is_End(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("End"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_End'");
      raise;
  end Is_End;
  --===========================================================================
  function Is_PgUp(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("PgUp"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_PgUp'");
      raise;
  end Is_PgUp;
  -----------------------------------------------------------------------------
  function Is_PgDn(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("PgDn"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_PgDn'");
      raise;
  end Is_PgDn;

  --===========================================================================
  function Is_Insert(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Insert"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Insert'");
      raise;
  end Is_Insert;

  --===========================================================================
  function Is_Abort(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Abort"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Abort'");
      raise;
  end Is_Abort;

  --===========================================================================
  function Is_Properties(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Properties"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Properties'");
      raise;
  end Is_Properties;

  --===========================================================================
  function Is_Front(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Front"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Front'");
      raise;
  end Is_Front;

  --===========================================================================
  function Is_Open(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Open"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Open'");
      raise;
  end Is_Open;

  --===========================================================================
  function Is_Search(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Search"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Search'");
      raise;
  end Is_Search;

  --===========================================================================
  function Is_Again(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Again"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Again'");
      raise;
  end Is_Again;

  --===========================================================================
  function Is_Undo(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Undo"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Undo'");
      raise;
  end Is_Undo;

  --===========================================================================
  function Is_Copy(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Copy"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Copy'");
      raise;
  end Is_Copy;
  -----------------------------------------------------------------------------
  function Is_Paste(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Paste"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Paste'");
      raise;
  end Is_Paste;
  -----------------------------------------------------------------------------
  function Is_Cut(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Cut"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Cut'");
      raise;
  end Is_Cut;

  -----------------------------------------------------------------------------
  --| Is_F1, Is_F2, ..., Is_F12
  -----------------------------------------------------------------------------

  function Is_F1(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F1"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F1'");
      raise;
  end Is_F1;
  -----------------------------------------------------------------------------
  function Is_F2(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F2"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F2'");
      raise;
  end Is_F2;
  -----------------------------------------------------------------------------
  function Is_F3(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F3"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F3'");
      raise;
  end Is_F3;
  -----------------------------------------------------------------------------
  function Is_F4(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F4"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F4'");
      raise;
  end Is_F4;
  -----------------------------------------------------------------------------
  function Is_F5(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F5"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F5'");
      raise;
  end Is_F5;
  -----------------------------------------------------------------------------
  function Is_F6(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F6"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F6'");
      raise;
  end Is_F6;
  -----------------------------------------------------------------------------
  function Is_F7(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F7"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F7'");
      raise;
  end Is_F7;
  -----------------------------------------------------------------------------
  function Is_F8(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F8"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F8'");
      raise;
  end Is_F8;
  -----------------------------------------------------------------------------
  function Is_F9(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F9"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F9'");
      raise;
  end Is_F9;
  -----------------------------------------------------------------------------
  function Is_F10(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F10"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F10'");
      raise;
  end Is_F10;
  -----------------------------------------------------------------------------
  function Is_F11(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F11"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F11'");
      raise;
  end Is_F11;
  -----------------------------------------------------------------------------
  function Is_F12(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("F12"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_F12'");
      raise;
  end Is_F12;

  -----------------------------------------------------------------------------
  --| Is_Up_Arrow, Is_Down_Arrow, Is_Left_Arrow, Is_Right_Arrow
  -----------------------------------------------------------------------------

  function Is_Up_Arrow(Item : in Key_Type)
           return Boolean is

  begin
     return ((Length(Item.Key) > 1) and then
             (Item.Key = Special_Key_Repr(To_Key_Code("Up_Arrow"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Up_Arrow'");
      raise;
  end Is_Up_Arrow;
  -----------------------------------------------------------------------------
  function Is_Down_Arrow(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Down_Arrow"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Down_Arrow'");
      raise;
  end Is_Down_Arrow;
  -----------------------------------------------------------------------------
  function Is_Left_Arrow(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Left_Arrow"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Left_Arrow'");
      raise;
  end Is_Left_Arrow;
  -----------------------------------------------------------------------------
  function Is_Right_Arrow(Item : in Key_Type)
           return Boolean is

  begin
    return ((Length(Item.Key) > 1) and then
            (Item.Key = Special_Key_Repr(To_Key_Code("Right_Arrow"))));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Right_Arrow'");
      raise;
  end Is_Right_Arrow;

  -----------------------------------------------------------------------------
  --| Is_Meta_Backspace, Is_Meta_Tab, Is_Meta_Return, Is_Meta_Esc
  -----------------------------------------------------------------------------

  function Is_Meta_Backspace(Item : in Key_Type)
           return Boolean is

  begin
     return ((Length(Item.Key) = 1) and then
             (Ada.Characters.Latin_1.Hts = To_Character(Item)));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Meta_Backspace'");
      raise;
  end Is_Meta_Backspace;

  -----------------------------------------------------------------------------
  function Is_Meta_Tab(Item : in Key_Type)
           return Boolean is

  begin
     return ((Length(Item.Key) = 1) and then
             (Ada.Characters.Latin_1.Htj = To_Character(Item)));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Meta_Tab'");
      raise;
  end Is_Meta_Tab;

  -----------------------------------------------------------------------------
  function Is_Meta_Return(Item : in Key_Type)
           return Boolean is

  begin
     return ((Length(Item.Key) = 1) and then
             (Ada.Characters.Latin_1.Ri = To_Character(Item)));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Meta_Return'");
      raise;
  end Is_Meta_Return;

  -----------------------------------------------------------------------------
  function Is_Meta_Esc(Item : in Key_Type)
           return Boolean is

  begin
     return ((Length(Item.Key) = 1) and then
             (Ada.Characters.Latin_1.Csi = To_Character(Item)));

  exception
    when E : others =>
      Put_Line("Internal Error in 'Is_Meta_Esc'");
      raise;
  end Is_Meta_Esc;


  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| To_String_Access
  -----------------------------------------------------------------------------

  function To_String_Access(S : in String)
           return Gnat.Os_Lib.String_Access is

    Sp : Gnat.Os_Lib.String_Access;

  begin
    Sp := new String(S'Range);
    Sp.All := S;

    return Sp;

  exception
    when E : others =>
      Put_Line("Internal Error in 'function To_String_Access'.");
      raise;
  end To_String_Access;

  -----------------------------------------------------------------------------
  --| To_Key_Code
  -----------------------------------------------------------------------------

  function To_Key_Code(Key_Str : String)
           return Natural is

  begin
    if (Key_Str = "Not legal!") then
      return 0;

    elsif (Key_Str = "F1") then
      return 1;
    elsif (Key_Str = "F2") then
      return 2;
    elsif (Key_Str = "F3") then
      return 3;
    elsif (Key_Str = "F4") then
      return 4;
    elsif (Key_Str = "F5") then
      return 5;
    elsif (Key_Str = "F6") then
      return 6;
    elsif (Key_Str = "F7") then
      return 7;
    elsif (Key_Str = "F8") then
      return 8;
    elsif (Key_Str = "F9") then
      return 9;
    elsif (Key_Str = "F10") then
      return 10;
    elsif (Key_Str = "F11") then
      return 11;
    elsif (Key_Str = "F12") then
      return 12;

    elsif (Key_Str = "Delete") then
      return 15;

    elsif (Key_Str = "Up_Arrow") then
      return 21;
    elsif (Key_Str = "Down_Arrow") then
      return 22;
    elsif (Key_Str = "Left_Arrow") then
      return 23;
    elsif (Key_Str = "Right_Arrow") then
      return 24;

    elsif (Key_Str = "Home") then
      return 25;
    elsif (Key_Str = "End") then
      return 26;
    elsif (Key_Str = "PgUp") then
      return 27;
    elsif (Key_Str = "PgDn") then
      return 28;

    elsif (Key_Str = "Insert") then
      return 29;

    elsif (Key_Str = "Abort") then
      return 30;
    elsif (Key_Str = "Properties") then
      return 31;
    elsif (Key_Str = "Front") then
      return 32;
    elsif (Key_Str = "Open") then
      return 33;
    elsif (Key_Str = "Search") then
      return 34;
    elsif (Key_Str = "Again") then
      return 35;
    elsif (Key_Str = "Undo") then
      return 36;
    elsif (Key_Str = "Copy") then
      return 37;
    elsif (Key_Str = "Paste") then
      return 38;
    elsif (Key_Str = "Cut") then
      return 39;
    end if;

    raise Key_Code_Error;

  exception
    when E : others =>
      Put_Line("Internal Error in 'To_Key_Code'");
      raise;
  end To_Key_Code;
  -----------------------------------------------------------------------------
  function To_Key_String(Key_Code : Natural)
           return String is

  begin
    if (Key_Code = 0) then
      return "Not legal!";

    elsif (Key_Code = 1) then
      return "F1";
    elsif (Key_Code = 2) then
      return "F2";
    elsif (Key_Code = 3) then
      return "F3";
    elsif (Key_Code = 4) then
      return "F4";
    elsif (Key_Code = 5) then
      return "F5";
    elsif (Key_Code = 6) then
      return "F6";
    elsif (Key_Code = 7) then
      return "F7";
    elsif (Key_Code = 8) then
      return "F8";
    elsif (Key_Code = 9) then
      return "F9";
    elsif (Key_Code = 10) then
      return "F10";
    elsif (Key_Code = 11) then
      return "F11";
    elsif (Key_Code = 12) then
      return "F12";

    elsif (Key_Code = 15) then
      return "Delete";

    elsif (Key_Code = 21) then
      return "Up_Arrow";
    elsif (Key_Code = 22) then
      return "Down_Arrow";
    elsif (Key_Code = 23) then
      return "Left_Arrow";
    elsif (Key_Code = 24) then
      return "Right_Arrow";

    elsif (Key_Code = 25) then
      return "Home";
    elsif (Key_Code = 26) then
      return "End";
    elsif (Key_Code = 27) then
      return "PgUp";
    elsif (Key_Code = 28) then
      return "PgDn";

    elsif (Key_Code = 29) then
      return "Insert";

    elsif (Key_Code = 30) then
      return "Abort";
    elsif (Key_Code = 31) then
      return "Properties";
    elsif (Key_Code = 32) then
      return "Front";
    elsif (Key_Code = 33) then
      return "Open";
    elsif (Key_Code = 34) then
      return "Search";
    elsif (Key_Code = 35) then
      return "Again";
    elsif (Key_Code = 36) then
      return "Undo";
    elsif (Key_Code = 37) then
      return "Copy";
    elsif (Key_Code = 38) then
      return "Paste";
    elsif (Key_Code = 39) then
      return "Cut";
    end if;

    raise Key_Code_Error;

  exception
    when E : others =>
      Put_Line("Internal Error in 'To_Key_String'");
      raise;
  end To_Key_String;

  -----------------------------------------------------------------------------
  --| Get_Immediate
  -----------------------------------------------------------------------------

  procedure Get_Immediate(Item      :    out Key_Type;
                          Available :    out Boolean;
                          Wait      : in     Boolean) is

    Echo_Modified   : Boolean := False;
    Buffer_Modified : Boolean := False;
    Ch              : Character;

  begin
    Item := Null_Key;

    if (Get_Echo_Mode = On) then
      Set_Echo_Mode(Off);
      Echo_Modified := True;
    end if;

    if (Get_Buffer_Mode = On) then
      Set_Buffer_Mode(Off);
      Buffer_Modified := True;
    end if;

    begin
      if (wait) then
        TJa.Standard_IO.Get_Immediate(Ch);
        Available := True;
      else
        TJa.Standard_IO.Get_Immediate(Ch, Available);
      end if;
    exception
      when End_Error =>
        -- Special when C-A in Unix.
        Ch := Character'Val(1);
    end;

    if (Available) then
      Append(Item.Key, Ch);
      if ((Ch = Ascii.Esc) or                                   -- Unix.
          (Ch = Ascii.Nul) or (Character'Pos(Ch) = 224)) then   -- Dos.
        loop
          TJa.Standard_IO.Get_Immediate(Ch, Available);
          exit when (not Available);
          Append(Item.Key, Ch);
          -- NYI: exit when found legal key value.
        end loop;
      end if;
      Available := True;
    end if;

    if (Echo_Modified) then
      Set_Echo_Mode(On);
      Echo_Modified := False;
    end if;

    if (Buffer_Modified) then
      Set_Buffer_Mode(On);
      Buffer_Modified := False;
    end if;

  exception
    when E : others =>
      Put_Line("Internal Error in 'Get_Immediate'");
      raise;
  end Get_Immediate;


  --***************************************************************************
  --* Very private procedures, functions, variables, types and constants just
  --* for initiation of package.
  --***************************************************************************
  procedure Get_Word(F   : in Ada.Text_IO.File_Type;
                     Str : out Unbounded_String) is

    Ch : Character;

  begin
    Str := To_Unbounded_String("");
    while (not End_Of_Line(F)) loop
      Get(F, Ch);
      exit when (Ch = ' ');
      Append(Str, Ch);
    end loop;

  exception
    when E : others =>
      Put_Line("Internal Error in 'Get_Word'");
      raise;
  end Get_Word;

  --===========================================================================
  --  procedure Put_Representation(Item : in Key_Type) is
  --
  --  begin
  --    Put(To_String(Item));
  --    Put("  ");
  --
  --    for I in 1..Length(Item.Key) loop
  --      Put(Character'Pos(Element(Item.Key, I)), Width => 4);
  --    end loop;
  --
  --  exception
  --    when E : others =>
  --      Put_Line("Internal Error in 'Put_Representation'");
  --      raise;
  --  end Put_Representation;
  -----------------------------------------------------------------------------
  --  procedure Put_Representation(File    : in Ada.Text_IO.File_Type;
  --                               Key_Str : in String;
  --                               Item    : in Key_Type) is
  --
  --  begin
  --    Put(File, To_String(Item));
  --    Put(File, "  ");
  --
  --    for I in 1..Length(Item.Key) loop
  --      Put(File, Character'Pos(Element(Item.Key, I)), Width => 4);
  --    end loop;
  --
  --  exception
  --    when E : others =>
  --      Put_Line("Internal Error in 'Put_Representation'");
  --      raise;
  --  end Put_Representation;


  --***************************************************************************
  Repr_File : Ada.Text_IO.File_Type;
  Key_Str   : Unbounded_String;
  Code      : Natural;

begin
  Args_Echo(1) := To_String_Access("echo");
  Args_No_Echo(1) := To_String_Access("-echo");

  Args_Buffer(1) := To_String_Access("-raw");
  Args_No_Buffer(1) := To_String_Access("raw");

  for I in Special_Key_Repr'Range loop
    Special_Key_Repr(I) := To_Unbounded_String("Not legal!");
  end loop;

  begin
    Open(Repr_File, In_File, "key_codes");
    while (not End_Of_File(Repr_File)) loop
      begin
        Get_Word(Repr_File, Key_Str);
        Special_Key_Repr(To_Key_Code(To_String(Key_Str))) :=
          To_Unbounded_String("");
        while (not End_Of_Line(Repr_File)) loop
          begin
            Get(Repr_File, Code);
            Append(Special_Key_Repr(To_Key_Code(To_String(Key_Str))),
                   Character'Val(Code));
          exception
            when Data_Error =>
              Put("Warning: Bad code in file 'key_codes' for key: '");
              Put(To_String(Key_Str));
              Put_Line("' ignored.");
              Special_Key_Repr(To_Key_Code(To_String(Key_Str))) :=
                To_Unbounded_String("Not legal!");
              exit;
            when E : others =>
              Special_Key_Repr(To_Key_Code(To_String(Key_Str))) :=
                To_Unbounded_String("Not legal!");
              raise;
          end;
        end loop;
      exception
        when Key_Code_Error =>
          Put("Warning: Syntax error in file 'key_codes'. Illegal key: '");
          Put(To_String(Key_Str));
          Put_Line("' ignored.");
      end;
      Skip_Line(Repr_File);
    end loop;
    Close(Repr_File);
  exception
    when Name_Error =>
      Put("Warning: File 'key_codes' doesn't exist.");
      Put_Line(" Special keys not implemented.");
    when E : others =>
      raise;
  end;


  --***************************************************************************

end TJa.Keyboard;
