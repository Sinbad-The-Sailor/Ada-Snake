-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                   T J A . W I N D O W . G R A P H I C                   |--
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
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package body TJa.Window.Graphic is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| "Current_Graphical_Mode" is the current status of the graphical mode.
  --| From the beginning the library assumes that graphical mode is off.
  -----------------------------------------------------------------------------

  Current_Graphical_Mode : Graphical_Mode_Type := Default_Graphical_Mode;


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Set_Graphical_Mode, Get_Graphical_Mode
  -----------------------------------------------------------------------------

  procedure Set_Graphical_Mode(Mode : Graphical_Mode_Type := On) is

  begin
    if Mode /= Current_Graphical_Mode then
      if Mode = On then
        Put(Ascii.ESC);
        Put("(0");
        Put(Ascii.ESC);
        Put(")0");
      else  -- Mode = Off.
        -- Old "sequence".
--          Put(Ascii.ESC);
--          Put("(A");
--          Put(Ascii.ESC);
--          Put(")A");
        -- New "sequence". (Wonder wy?)
        Put(Ascii.ESC);
        Put("(B");
        Put(Ascii.ESC);
        Put(")B");
      end if;
      Flush;

      Current_Graphical_Mode := Mode;
    end if;
  end Set_Graphical_Mode;
  -----------------------------------------------------------------------------
  function Get_Graphical_Mode return Graphical_Mode_Type is

  begin
    return Current_Graphical_Mode;
  end Get_Graphical_Mode;

  -----------------------------------------------------------------------------
  --| Put
  -----------------------------------------------------------------------------

  procedure Put(Item  : in Graphical_Character_Type;
                Times : in Positive := 1) is

    Org_Graphical_Mode : Graphical_Mode_Type := Current_Graphical_Mode;

  begin
    if Current_Graphical_Mode = Off then
      Set_Graphical_Mode(On);
    end if;

    for I in 1..Times loop
      if Item = Blended_Raster then
	Put(Character'Val(Character'Pos('Z') + 7));
      else	  
	Put(Character'Val(Graphical_Character_Type'Pos(Item) +
			  Character'Pos('j')));
      end if;
    end loop;
    Flush;

    if Org_Graphical_Mode = Off then
      Set_Graphical_Mode(Off);
    end if;
  end Put;

  --===========================================================================
  procedure Put_Line(Item  : in Graphical_Character_Type;
                     Times : in Positive := 1) is

  begin
    Put(Item);
    New_Line;
  end Put_Line;

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
end TJa.Window.Graphic;
