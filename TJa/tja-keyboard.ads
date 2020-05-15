-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                         T J A . K E Y B O A R D                         |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  2.01                              |--
--|                                                                         |--
--|                           (C) Copyright, 2000                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2000-03-10  Version 2.01 is ok.                                       |--
--|   2000-02-25  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package can be used for keyboard handling. The special type      |--
--|   'Key_Type' represents a single key including special keys like HOME,  |--
--|   END, INSERT, RETURN, ESC, F1, CTRL-F and so on.                       |--
--|                                                                         |--
--|   "Get_Immediate" is implemented for 'Key_Type'.                        |--
--|                                                                         |--
--|   Conversions from 'Key_Type' to 'Character' and 'String' are included  |--
--|   and special functions to check what type of key we have got from      |--
--|   "Get_Immediate" are also included in this library.                    |--
--|                                                                         |--
--|   The methods "Set_Echo_Mode" and "Set_Buffer_Mode" are used to modify  |--
--|   keyboard echo on screen (on/off) and modify buffering from keyboard   |--
--|   before reading (on/off). These methods can make the program to be     |--
--|   more effective.                                                       |--
--|                                                                         |--
--|   If the file ".TJa_key_codes" exists in current library we use these   |--
--|   key definitions instead of the internal predefined ones. In this file |--
--|   the name and key codes are listed like this (one line for each        |--
--|   special key).                                                         |--
--|                                                                         |--
--|       F1             27  91  49  49 126                                 |--
--|       F2             27  91  49  50 126                                 |--
--|       F3             27  91  49  51 126                                 |--
--|       Up_Arrow       27  91  65                                         |--
--|       Down_Arrow     27  91  66                                         |--
--|       Abort          27  91  50  51 126                                 |--
--|       Properties     27  91  50  53 126                                 |--
--|                                                                         |--
--|   The key codes are the "Ascii code numbers" for the character sequence |--
--|   generated when a special key is pressed.                              |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.

-- Internal libraries.
with TJa.Standard_IO;			use TJa.Standard_IO;

-------------------------------------------------------------------------------
package TJa.Keyboard is

  -----------------------------------------------------------------------------
  --| "Key_Type" is the type for a general key. The value "Null_Key" are used
  --| to identify that the key is "empty". The value "Undefined_Key" are used
  --| to identify that a key is "undefined" (not included in this library).
  -----------------------------------------------------------------------------

  type Key_Type is private;

  Null_Key      : constant Key_Type;
  Undefined_Key : constant Key_Type;

  -----------------------------------------------------------------------------
  --| "Echo_Mode_Type" is used to set output echo mode on/off.
  --|
  --| "Set_Echo_Mode" manipulates the output echo mode. If 'On' then every key
  --| pressed have an echo on screen (user can se what he/she writes). If 'Off'
  --| then no echo is done.
  --|
  --| "Get_Echo_Mode" returns the current status (from beginning this library
  --| assumes that the mode is 'On').
  -----------------------------------------------------------------------------

  type Echo_Mode_Type is (On, Off);

  procedure Set_Echo_Mode(Mode : in Echo_Mode_Type := On);
  function Get_Echo_Mode return Echo_Mode_Type;

  -----------------------------------------------------------------------------
  --| "Buffer_Mode_Type" is used to set input buffer mode on/off.
  --|
  --| "Set_Buffer_Mode" manipulates the input buffer mode. If 'On' then input
  --| is buffered until 'RETURN' is pressed (operating system has control over
  --| input). If 'Off' then no buffering is done (the program has to deal with
  --| all keys pressed).
  --|
  --| "Get_Buffer_Mode" returns the current status (from beginning this library
  --| assumes that the mode is 'On').
  -----------------------------------------------------------------------------

  type Buffer_Mode_Type is (On, Off);

  procedure Set_Buffer_Mode(Mode : in Buffer_Mode_Type := On);
  function Get_Buffer_Mode return Buffer_Mode_Type;

  -----------------------------------------------------------------------------
  --| "Get_Immediate" returns next key pressed.
  --|
  --| The first version waits until key is pressed (if no key pressed before).
  --|
  --| The second version don't wait. If no key is pressed 'False' is returned
  --| in the 'Available' parameter and the 'Item' is undefined.
  -----------------------------------------------------------------------------

  procedure Get_Immediate(Item  : out Key_Type);
  procedure Get_Immediate(Item      : out Key_Type;
                          Available : out Boolean);

  -----------------------------------------------------------------------------
  --| "Put" writes the key (or it's "name") on screen.
  --| "Put_Line" does a "Put" and then a "New_Line".
  -----------------------------------------------------------------------------

  procedure Put(Item : in Key_Type);
  procedure Put_Line(Item : in Key_Type);

  -----------------------------------------------------------------------------
  --| "To_Character" converts a 'Key_Type' to a 'Character'. This is only
  --| allowed if "Is_Character" returns 'True'.
  --|
  --| "To_String" converts a 'Key_Type' to a 'String'.
  --|
  --| "To_Unbounded_String" converts a 'Key_Type' to a 'Unbounded_String'.
  -----------------------------------------------------------------------------

  function To_Character(Source : in Key_Type) return Character;
  function To_String(Source : in Key_Type) return String;
  function To_Unbounded_String(Source : in Key_Type) return Unbounded_String;

  -----------------------------------------------------------------------------
  --| "Is_Character" returns 'True' if key is a character.
  --|
  --| "Is_Control" returns 'True' if key is a 'control character' (CTRL pressed
  --| with a charcter).
  --|
  --| "Is_Special" return 'True' if key is a 'special key' (including function
  --| and arrow keys) like F1, HOME, UP_ARROW, INSERT, META_TAB, etc.
  --|
  --| "Is_Function" return 'True' if key is a 'function key' (F1, F2, ...).
  --|
  --| "Is_Arrow" return 'True' if key is an 'arrow key' (LEFT, RIGHT, ...).
  --|
  --| "Is_Meta" return 'True' if key is an 'meta key' (META_TAB, ...).
  -----------------------------------------------------------------------------

  function Is_Character(Item : in Key_Type) return Boolean;
  function Is_Control(Item : in Key_Type) return Boolean;
  function Is_Special(Item : in Key_Type) return Boolean;
  function Is_Function(Item : in Key_Type) return Boolean;
  function Is_Arrow(Item : in Key_Type) return Boolean;
  function Is_Meta(Item : in Key_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| "Is_Xxx" returns 'True' if key is 'Xxx key' (where Xxx is the key name).
  --| These function are only relevant when "Is_Control" returns 'True'.
  --| Observe that some of the control keys are missing:
  --|   Ctrl_H (Backspace), Ctrl_I (Tab), Ctrl_J (Line feed), Ctrl_M (Return).
  -----------------------------------------------------------------------------

  function Is_Ctrl_Space(Item : in Key_Type) return Boolean;
  function Is_Ctrl_A(Item : in Key_Type) return Boolean;
  function Is_Ctrl_B(Item : in Key_Type) return Boolean;
  function Is_Ctrl_C(Item : in Key_Type) return Boolean;
  function Is_Ctrl_D(Item : in Key_Type) return Boolean;
  function Is_Ctrl_E(Item : in Key_Type) return Boolean;
  function Is_Ctrl_F(Item : in Key_Type) return Boolean;
  function Is_Ctrl_G(Item : in Key_Type) return Boolean;
  function Is_Ctrl_J(Item : in Key_Type) return Boolean;
  function Is_Ctrl_K(Item : in Key_Type) return Boolean;
  function Is_Ctrl_L(Item : in Key_Type) return Boolean;
  function Is_Ctrl_N(Item : in Key_Type) return Boolean;
  function Is_Ctrl_O(Item : in Key_Type) return Boolean;
  function Is_Ctrl_P(Item : in Key_Type) return Boolean;
  function Is_Ctrl_Q(Item : in Key_Type) return Boolean;
  function Is_Ctrl_R(Item : in Key_Type) return Boolean;
  function Is_Ctrl_S(Item : in Key_Type) return Boolean;
  function Is_Ctrl_T(Item : in Key_Type) return Boolean;
  function Is_Ctrl_U(Item : in Key_Type) return Boolean;
  function Is_Ctrl_V(Item : in Key_Type) return Boolean;
  function Is_Ctrl_W(Item : in Key_Type) return Boolean;
  function Is_Ctrl_X(Item : in Key_Type) return Boolean;
  function Is_Ctrl_Y(Item : in Key_Type) return Boolean;
  function Is_Ctrl_Z(Item : in Key_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| "Is_Xxx" returns 'True' if key is 'Xxx key' (where Xxx is the key name).
  --| These function are only relevant when "Is_Special" returns 'True'.
  -----------------------------------------------------------------------------

  function Is_Return(Item : in Key_Type) return Boolean;
  function Is_Delete(Item : in Key_Type) return Boolean;
  function Is_Backspace(Item : in Key_Type) return Boolean;
  function Is_Tab(Item : in Key_Type) return Boolean;
  function Is_Esc(Item : in Key_Type) return Boolean;

  function Is_Home(Item : in Key_Type) return Boolean;
  function Is_End(Item : in Key_Type) return Boolean;
  function Is_PgUp(Item : in Key_Type) return Boolean;
  function Is_PgDn(Item : in Key_Type) return Boolean;
  function Is_Insert(Item : in Key_Type) return Boolean;

  function Is_Abort(Item : in Key_Type) return Boolean;
  function Is_Properties(Item : in Key_Type) return Boolean;
  function Is_Front(Item : in Key_Type) return Boolean;
  function Is_Open(Item : in Key_Type) return Boolean;
  function Is_Search(Item : in Key_Type) return Boolean;
  function Is_Again(Item : in Key_Type) return Boolean;
  function Is_Undo(Item : in Key_Type) return Boolean;
  function Is_Copy(Item : in Key_Type) return Boolean;
  function Is_Paste(Item : in Key_Type) return Boolean;
  function Is_Cut(Item : in Key_Type) return Boolean;

  -- "Is_Function" is the same thing as all "Is_F1", "Is_F2", etc.
  function Is_F1(Item : in Key_Type) return Boolean;
  function Is_F2(Item : in Key_Type) return Boolean;
  function Is_F3(Item : in Key_Type) return Boolean;
  function Is_F4(Item : in Key_Type) return Boolean;
  function Is_F5(Item : in Key_Type) return Boolean;
  function Is_F6(Item : in Key_Type) return Boolean;
  function Is_F7(Item : in Key_Type) return Boolean;
  function Is_F8(Item : in Key_Type) return Boolean;
  function Is_F9(Item : in Key_Type) return Boolean;
  function Is_F10(Item : in Key_Type) return Boolean;
  function Is_F11(Item : in Key_Type) return Boolean;
  function Is_F12(Item : in Key_Type) return Boolean;

  -- "Is_Arrow" is the same thing as all "Is_Up_Arrow", "Is_Down_Arrow", etc.
  function Is_Up_Arrow(Item : in Key_Type) return Boolean;
  function Is_Down_Arrow(Item : in Key_Type) return Boolean;
  function Is_Left_Arrow(Item : in Key_Type) return Boolean;
  function Is_Right_Arrow(Item : in Key_Type) return Boolean;

  -- "Is_Meta" is the same thing as all "Is_Meta_Tab", "Is_Meta_Esc", etc.
  function Is_Meta_Backspace(Item : in Key_Type) return Boolean;
  function Is_Meta_Tab(Item : in Key_Type) return Boolean;
  function Is_Meta_Return(Item : in Key_Type) return Boolean;
  function Is_Meta_Esc(Item : in Key_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| Global exceptions for TJa-keyboard.
  -----------------------------------------------------------------------------

  Key_Code_Error : exception;

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of "Key_Type".
  -----------------------------------------------------------------------------

  type Key_Type is
    record
      Key : Unbounded_String;
    end record;

  -----------------------------------------------------------------------------
  --| The constants that represents "empty" and "undefined" key.
  -----------------------------------------------------------------------------

  Null_Key      : constant Key_Type := (Key => To_Unbounded_String(""));
  Undefined_Key : constant Key_Type :=
    (Key => To_Unbounded_String("Strange *key*!"));

  -----------------------------------------------------------------------------

end TJa.Keyboard;
