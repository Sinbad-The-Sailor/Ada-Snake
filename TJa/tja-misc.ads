-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                             T J A . M I S C                             |--
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
--|   2000-03-19  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package includes methods we "need" but dont have any special     |--
--|   package for.                                                          |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Misc is

  -----------------------------------------------------------------------------
  --| "Relation_Type" is used to describe relations between some objects.
  -----------------------------------------------------------------------------

  type Relation_Type is
    (Less, Equal, Greater, Uncomparable);

  -----------------------------------------------------------------------------
  --| "Beep" let the computer make a beep (an alert signal).
  -----------------------------------------------------------------------------

  procedure Beep;

  -----------------------------------------------------------------------------
  --| "To_Integer" converts a digit character to an integer.
  --| "To_Character" converts an one digit integer to a character.
  -----------------------------------------------------------------------------

  function To_Integer(Item : in Character) return Integer;
  function To_Character(Item : in Integer) return Character;

  -----------------------------------------------------------------------------
  --| To_Integer, To_String
  -----------------------------------------------------------------------------

  function To_Integer(Item : in String) return Integer;
  function To_String(Item  : in Integer;
                     Width : in Natural   := 0;
                     Pad   : in Character := ' ') return String;

  -----------------------------------------------------------------------------
  --| To_Integer, To_Unbounded_String
  -----------------------------------------------------------------------------

  function To_Integer(Item : in Unbounded_String) return Integer;
  function To_Unbounded_String(Item  : in Integer;
                               Width : in Natural   := 0;
                               Pad   : in Character := ' ')
           return Unbounded_String;

  -----------------------------------------------------------------------------
  --| To_Boolean, To_String
  -----------------------------------------------------------------------------

  function To_Boolean(Item : in String) return Boolean;
  function To_String(Item  : in Boolean;
                     Width : in Natural   := 0;
                     Pad   : in Character := ' ') return String;

  -----------------------------------------------------------------------------
  --| To_Boolean, To_Unbounded_String
  -----------------------------------------------------------------------------

  function To_Boolean(Item : in Unbounded_String) return Boolean;
  function To_Unbounded_String(Item  : in Boolean;
                               Width : in Natural   := 0;
                               Pad   : in Character := ' ')
           return Unbounded_String;

  -----------------------------------------------------------------------------
  --| Exceptions for TJa.Misc-library.
  -----------------------------------------------------------------------------
  Misc_Data_Error : exception;  -- Can't do the conversion.

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of 'Relation_Type'.
  -----------------------------------------------------------------------------

  for Relation_Type'Size use 2;
  for Relation_Type use
    (Less         => 0,
     Equal        => 1,
     Greater      => 2,
     Uncomparable => 3);

  -----------------------------------------------------------------------------

end TJa.Misc;
