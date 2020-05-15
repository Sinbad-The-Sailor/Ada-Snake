-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                                  T J A                                  |--
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
--|   2000-02-21  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This is the top level for all packages in the "Torbjörn Jonsson Ada   |--
--|   library".                                                             |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa is

  -----------------------------------------------------------------------------
  --| Global exceptions for all TJa-library.
  -----------------------------------------------------------------------------

  Not_Yet_Implemented : exception;  -- Method is not implemented yet.
  Internal_Error      : exception;  -- Something very strange has happend.

  Data_Error          : exception;  -- Wrong type of data.
  Constraint_Error    : exception;  -- Data out of bounds.

  -----------------------------------------------------------------------------

end TJa;
