-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                    T J A . K E Y B O A R D . K E Y S                    |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  2.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2000                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2000-04-13  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package is a complement to "TJa.Keyboard" and consists of a      |--
--|   number of constants representing the different keys on a keyboard.    |--
--|   The type 'Key_Code_Type' can be used in intervals in a loop or as     |--
--|   values in a "case" statement.                                         |--
--|                                                                         |--
--|   The methods 'To_Key_Code_Type' and 'To_Key_Type' are used to convert  |--
--|   between the two types. Observe that it's only legal to convert to a   |--
--|   'Key_Code_Type' if the 'Key_Type' doesn't represent a 'Character'.    |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Keyboard.Keys is

  -----------------------------------------------------------------------------
  --| "Key_Code_Type" is the type for key code.
  -----------------------------------------------------------------------------

  type Key_Code_Type is new Integer range -2..63;

  -----------------------------------------------------------------------------
  --| "To_Key_Code_Type" and "To_Key_Type" converts between the two types.
  --| These methods are only legal to use if the key isn't a normal character.
  -----------------------------------------------------------------------------

  function To_Key_Code_Type(Source : in Key_Type) return Key_Code_Type;

  -- Not yet implemented:
  --   function To_Key_Type(Source : in Key_Code_Type) return Key_Type;

  -----------------------------------------------------------------------------
  --| The values "Key_Xxx" represents a key code values for each key "Xxx".
  -----------------------------------------------------------------------------

  -- Special values for empty and undefined keys.

  Key_Null      : constant Key_Code_Type := -2;
  Key_Undefined : constant Key_Code_Type := -1;

  -- Control keys.

  Key_Ctrl_Space : constant Key_Code_Type := 0;
  Key_Ctrl_A     : constant Key_Code_Type := 1;
  Key_Ctrl_B     : constant Key_Code_Type := 2;
  Key_Ctrl_C     : constant Key_Code_Type := 3;
  Key_Ctrl_D     : constant Key_Code_Type := 4;
  Key_Ctrl_E     : constant Key_Code_Type := 5;
  Key_Ctrl_F     : constant Key_Code_Type := 6;
  Key_Ctrl_G     : constant Key_Code_Type := 7;
  Key_Ctrl_H     : constant Key_Code_Type := 8;  -- Same as. Key_Backspace
  Key_Ctrl_I     : constant Key_Code_Type := 9;  -- Same as: Key_Tab
  Key_Ctrl_J     : constant Key_Code_Type := 10;
  Key_Ctrl_K     : constant Key_Code_Type := 11;
  Key_Ctrl_L     : constant Key_Code_Type := 12;
  Key_Ctrl_M     : constant Key_Code_Type := 13;  -- Same as: Key_Return
  Key_Ctrl_N     : constant Key_Code_Type := 14;
  Key_Ctrl_O     : constant Key_Code_Type := 15;
  Key_Ctrl_P     : constant Key_Code_Type := 16;
  Key_Ctrl_Q     : constant Key_Code_Type := 17;
  Key_Ctrl_R     : constant Key_Code_Type := 18;
  Key_Ctrl_S     : constant Key_Code_Type := 19;
  Key_Ctrl_T     : constant Key_Code_Type := 20;
  Key_Ctrl_U     : constant Key_Code_Type := 21;
  Key_Ctrl_V     : constant Key_Code_Type := 22;
  Key_Ctrl_W     : constant Key_Code_Type := 23;
  Key_Ctrl_X     : constant Key_Code_Type := 24;
  Key_Ctrl_Y     : constant Key_Code_Type := 25;
  Key_Ctrl_Z     : constant Key_Code_Type := 26;

  -- Special keys.

  Key_Backspace  : constant Key_Code_Type := 8;
  Key_Tab        : constant Key_Code_Type := 9;
  Key_Return     : constant Key_Code_Type := 13;
  Key_Esc        : constant Key_Code_Type := 27;
  Key_Delete     : constant Key_Code_Type := 28;

  Key_Home       : constant Key_Code_Type := 29;
  Key_End        : constant Key_Code_Type := 30;
  Key_PgUp       : constant Key_Code_Type := 31;
  Key_PgDn       : constant Key_Code_Type := 32;
  Key_Insert     : constant Key_Code_Type := 33;

  Key_Abort      : constant Key_Code_Type := 34;
  Key_Properties : constant Key_Code_Type := 35;
  Key_Front      : constant Key_Code_Type := 36;
  Key_Open       : constant Key_Code_Type := 37;
  Key_Search     : constant Key_Code_Type := 38;
  Key_Again      : constant Key_Code_Type := 39;
  Key_Undo       : constant Key_Code_Type := 40;
  Key_Copy       : constant Key_Code_Type := 41;
  Key_Paste      : constant Key_Code_Type := 42;
  Key_Cut        : constant Key_Code_Type := 43;

  -- Function keys.

  Key_F1  : constant Key_Code_Type := 44;
  Key_F2  : constant Key_Code_Type := 45;
  Key_F3  : constant Key_Code_Type := 46;
  Key_F4  : constant Key_Code_Type := 47;
  Key_F5  : constant Key_Code_Type := 48;
  Key_F6  : constant Key_Code_Type := 49;
  Key_F7  : constant Key_Code_Type := 50;
  Key_F8  : constant Key_Code_Type := 51;
  Key_F9  : constant Key_Code_Type := 52;
  Key_F10 : constant Key_Code_Type := 53;
  Key_F11 : constant Key_Code_Type := 54;
  Key_F12 : constant Key_Code_Type := 55;

  -- Arrow keys.

  Key_Up_Arrow    : constant Key_Code_Type := 56;
  Key_Down_Arrow  : constant Key_Code_Type := 57;
  Key_Left_Arrow  : constant Key_Code_Type := 58;
  Key_Right_Arrow : constant Key_Code_Type := 59;

  -- Meta keys.

  Key_Meta_Backspace : constant Key_Code_Type := 60;
  Key_Meta_Tab       : constant Key_Code_Type := 61;
  Key_Meta_Return    : constant Key_Code_Type := 62;
  Key_Meta_Esc       : constant Key_Code_Type := 63;

  -----------------------------------------------------------------------------
  --| Exceptions for TJa.Keyboard.Keys-library.
  -----------------------------------------------------------------------------

  Keyboard_Key_Data_Error : exception;  -- Illegal with normal character key.

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------

  -- Nothing private just now.

  -----------------------------------------------------------------------------

end TJa.Keyboard.Keys;
