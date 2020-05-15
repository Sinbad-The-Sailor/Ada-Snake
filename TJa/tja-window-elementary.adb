-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                T J A . W I N D O W . E L E M E N T A R Y                |--
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
--|   2000-02-25  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
--|   Uses 'ansi codes' for clearing window and moving cursor.              |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package body TJa.Window.Elementary is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -- Nothing just now.


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Clear_Window
  -----------------------------------------------------------------------------

  procedure Clear_Window is

  begin
    Put(Ascii.Esc);
    Put("[H");
    Put(Ascii.Esc);
    Put("[2J");
    Flush;
  end Clear_Window;

  -----------------------------------------------------------------------------
  --| Goto_XY
  -----------------------------------------------------------------------------

  procedure Goto_XY(X, Y : in Positive) is

  begin
    Put(Ascii.Esc);
    Put('[');
    Put(Y, Width => 0);
    Put(';');
    Put(X, Width => 0);
    Put('H');
    Flush;
  end Goto_XY;

  -----------------------------------------------------------------------------
  --| Cursor_Invisible
  -----------------------------------------------------------------------------

  procedure Cursor_Invisible is

  begin
    Put(Ascii.Esc);
    Put("[?25l");
    Flush;
  end Cursor_Invisible;

  -----------------------------------------------------------------------------
  --| Cursor_Visible
  -----------------------------------------------------------------------------

  procedure Cursor_Visible is

  begin
    Put(Ascii.Esc);
    Put("[?25h");
    Flush;
  end Cursor_Visible;


  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -- Nothing just now.


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Window.Elementary;
