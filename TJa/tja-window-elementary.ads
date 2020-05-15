-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                T J A . W I N D O W . E L E M E N T A R Y                |--
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
--|   2000-02-25  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package can be used to modify text area in a normal text window. |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Window.Elementary is

  -----------------------------------------------------------------------------
  --| "Clear_Window" clear text area and moves the cursor to upper left corner.
  -----------------------------------------------------------------------------

  procedure Clear_Window;

  -----------------------------------------------------------------------------
  --| "Goto_XY" moves cursor to position '(X, Y)' in text area. The upper left
  --| corner is position '(1, 1)' and 'X' increases to the right and 'Y'
  --| increases downwards.
  --|
  --| If '(X, Y)' is out of window size it's undefined what happens.
  -----------------------------------------------------------------------------

  procedure Goto_XY(X, Y : in Positive);

  -----------------------------------------------------------------------------
  --| "Cursor_Invisible" / "Cursor_Visible" make the cursor (in)visible.
  -----------------------------------------------------------------------------

  procedure Cursor_Invisible;
  procedure Cursor_Visible;

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Nothing private just now.
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------

end TJa.Window.Elementary;
