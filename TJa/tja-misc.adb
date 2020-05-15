-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                             T J A . M I S C                             |--
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
--|   2000-03-19  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package body TJa.Misc is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -- No local information just now.


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Beep
  -----------------------------------------------------------------------------

  procedure Beep is

  begin
    Put(Ascii.Bel);
    Flush;
  end Beep;

  -----------------------------------------------------------------------------
  --| To_Integer, To_Character
  -----------------------------------------------------------------------------

  function To_Integer(Item : in Character)
           return Integer is

  begin
    if not (Item in '0'..'9') then
      raise Misc_Data_Error;
    end if;
    return Character'Pos(Item) - Character'Pos('0');
  end To_Integer;

  --===========================================================================
  function To_Character(Item : in Integer)
           return Character is

  begin
    if not (Item in 0..9) then
      raise Misc_Data_Error;
    end if;
    return Character'Val(Item + Character'Pos('0'));
  end To_Character;

  -----------------------------------------------------------------------------
  --| To_Integer, To_String
  -----------------------------------------------------------------------------

  function To_Integer(Item : in String)
           return Integer is

  begin
    return Integer'Value(Item);
  exception
     when others =>
       raise Misc_Data_Error;
  end To_Integer;

  --===========================================================================
  function To_String(Item  : in Integer;
                     Width : in Natural   := 0;
                     Pad   : in Character := ' ')
           return String is

  begin
    return To_String(To_Unbounded_String(Item, Width, Pad));
  end To_String;

  -----------------------------------------------------------------------------
  --| To_Integer, To_Unbounded_String
  -----------------------------------------------------------------------------

  function To_Integer(Item : in Unbounded_String)
           return Integer is

  begin
    return To_Integer(To_String(Item));
  end To_Integer;

  --===========================================================================
  function To_Unbounded_String(Item  : in Integer;
                               Width : in Natural   := 0;
                               Pad   : in Character := ' ')
           return Unbounded_String is

    U_S   : Unbounded_String;
    Pad_S : String(1 .. 1) := (others => Pad);

  begin
    U_S := Trim(To_Unbounded_String(Integer'Image(Item)), Both);
    if Length(U_S) >= Width then
      return U_S;
    end if;

    return ((Width - Length(U_S)) * Pad_S) & U_S;
  end To_Unbounded_String;

  -----------------------------------------------------------------------------
  --| To_Boolean, To_String
  -----------------------------------------------------------------------------

  function To_Boolean(Item : in String) return Boolean is

  begin
    return Boolean'Value(Item);
  exception
     when others =>
       raise Misc_Data_Error;
  end To_Boolean;

  --===========================================================================
  function To_String(Item  : in Boolean;
                     Width : in Natural   := 0;
                     Pad   : in Character := ' ') return String is

  begin
    return To_String(To_Unbounded_String(Item, Width, Pad));
  end To_String;

  -----------------------------------------------------------------------------
  --| To_Boolean, To_Unbounded_String
  -----------------------------------------------------------------------------

  function To_Boolean(Item : in Unbounded_String) return Boolean is

  begin
    return To_Boolean(To_String(Item));
  end To_Boolean;

  --===========================================================================
  function To_Unbounded_String(Item  : in Boolean;
                               Width : in Natural   := 0;
                               Pad   : in Character := ' ')
           return Unbounded_String is

    U_S   : Unbounded_String;
    Pad_S : String(1 .. 1) := (others => Pad);

  begin
    U_S := Trim(To_Unbounded_String(Boolean'Image(Item)), Both);
    if Length(U_S) >= Width then
      return U_S;
    end if;

    return ((Width - Length(U_S)) * Pad_S) & U_S;
  end To_Unbounded_String;


  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -- No local information just now.


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Misc;
