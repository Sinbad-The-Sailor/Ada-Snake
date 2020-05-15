-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                   T J A . W I N D O W . G R A P H I C                   |--
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
--|   This package can be used for graphic handling in a text window. It is |--
--|   "ascii graphic" it supportes and it uses 'ansi codes' for doing it.   |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Window.Graphic is

  -----------------------------------------------------------------------------
  --| "Graphical_Mode_Type" is used to set graphical mode on/off.
  --|
  --| "Set_Graphical_Mode" manipulates the output graphical mode. If 'On' then
  --| it's ok to draw graphical characters.
  --|
  --| "Get_Graphical_Mode" returns the current status (from beginning this
  --| library assumes that the mode is 'Off').
  -----------------------------------------------------------------------------

  type Graphical_Mode_Type is (On, Off);

  Default_Graphical_Mode : constant Graphical_Mode_Type := Off;

  procedure Set_Graphical_Mode(Mode : Graphical_Mode_Type := On);
  function Get_Graphical_Mode return Graphical_Mode_Type;

  -----------------------------------------------------------------------------
  --| "Graphical_Charcter_Type" is the type for a graphical character. The
  --| values represents the different type of legal characters to draw.
  -----------------------------------------------------------------------------

  type Graphical_Character_Type is
    (Lower_Right_Corner, Upper_Right_Corner, Upper_Left_Corner,
     Lower_Left_Corner, Cross, Horisontal_Very_High_Line,
     Horisontal_High_Line, Horisontal_Line, Horisontal_Low_Line,
     Horisontal_Very_Low_Line, Vertical_Right, Vertical_Left,
     Horisontal_Up, Horisontal_Down, Vertical_Line, Blended_Raster);

  -----------------------------------------------------------------------------
  --| "Put" writes 'Times' number of the graphical character on screen.
  --| "Put_Line" does a "Put" and then a "New_Line".
  -----------------------------------------------------------------------------

  procedure Put(Item  : in Graphical_Character_Type;
                Times : in Positive := 1);
  procedure Put_Line(Item : in Graphical_Character_Type;
                     Times : in Positive := 1);

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Nothing private just now.
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------

end TJa.Window.Graphic;
