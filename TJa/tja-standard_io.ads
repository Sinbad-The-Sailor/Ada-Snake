-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                      T J A . S T A N D A R D _ I O                      |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  1.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2003                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2003-04-16  Version 1.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
--|   2015-03-22  Updated with Get_Immediate by Torbjörn Jonsson.           |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package includes methods which should have been in the pacakges  |--
--|   Ada.Text_IO, Ada.Integer_Text_IO and Ada.Float_Text_IO. Why they are  |--
--|   not in these packages is strange, but ...                             |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Text_IO.C_Streams;		use Ada.Text_IO.C_Streams;
with Interfaces.C_Streams; 		use Interfaces.C_Streams;
--  with Ada.Strings;                       use Ada.Strings;
--  with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Standard_IO is

  -----------------------------------------------------------------------------
  --| "Get_Immediate" does a "Get" without waiting on ENTER from user (or file)
  --| and it doesn't care about standards like UTF-8 an so on. Just take the
  --| bytes as they come. This is an implementation which are different from
  --| the Ada standard library.
  -----------------------------------------------------------------------------

  procedure Get_Immediate(File : in     File_Type;
			  Item :    out Character);
  
  procedure Get_Immediate(Item : out Character);
  
  -----------------------------------------------------------------------------
  
  procedure Get_Immediate(File      : in     File_Type;
			  Item      :    out Character;
			  Available :    out Boolean);
  
  procedure Get_Immediate(Item      : out Character;
			  Available : out Boolean);
  
  -----------------------------------------------------------------------------
  --| "Get_Line" does a "Get" and a "Skip_Line" for the types that doesn't have
  --| it in the standard. Special is "Get_Line" for the type 'String' which has
  --| an implementation which are different in the Ada standard library.
  -----------------------------------------------------------------------------

  procedure Get_Line(File : in     File_Type;
                     Item :    out Character);

  procedure Get_Line(Item : out Character);

  -----------------------------------------------------------------------------
  procedure Get_Line(File : in     File_Type;
                     Item :    out String);

  procedure Get_Line(Item : out String);

  -----------------------------------------------------------------------------
  procedure Get_Line(File  : in     File_Type;
                     Item  :    out Integer;
                     Width : in     Field := 0);

  procedure Get_Line(Item  :    out Integer;
                     Width : in     Field := 0);

  -----------------------------------------------------------------------------
  procedure Get_Line(File : in     File_Type;
                     Item :    out Float);

  procedure Get_Line(Item : out Float);

  -----------------------------------------------------------------------------
  --| "Put" is a special variant with format possibilities for the types
  --| 'Character' and 'String'.
  -----------------------------------------------------------------------------

  procedure Put(File  : in File_Type;
                Item  : in Character;
                Width : in Field;
                Fill  : in Character);
  procedure Put(Item  : in Character;
                Width : in Field;
                Fill  : in Character);

  -----------------------------------------------------------------------------
  procedure Put(File  : in File_Type;
                Item  : in String;
                Width : in Field;
                Fill  : in Character);

  procedure Put(Item  : in String;
                Width : in Field;
                Fill  : in Character);

  -----------------------------------------------------------------------------
  procedure Put(File  : in File_Type;
                Item  : in Integer;
                Width : in Field;
                Base  : in Number_Base;
                Fill  : in Character);

  procedure Put(Item  : in Integer;
                Width : in Field;
                Base  : in Number_Base;
                Fill  : in Character);

  -----------------------------------------------------------------------------
  procedure Put(File : in File_Type;
                Item : in Float;
                Fore : in Field;
                Aft  : in Field;
                Exp  : in Field;
                Fill : in Character);

  procedure Put(Item : in Float;
                Fore : in Field;
                Aft  : in Field;
                Exp  : in Field;
                Fill : in Character);

  -----------------------------------------------------------------------------
  --| "Put_Line" does a "Put" and a "New_Line" for the types that doesn't have
  --| it in the standard.
  -----------------------------------------------------------------------------

  procedure Put_Line(File  : in File_Type;
                     Item  : in Character;
                     Width : in Field;
                     Fill  : in Character);

  procedure Put_Line(Item  : in Character;
                     Width : in Field;
                     Fill  : in Character);

  -----------------------------------------------------------------------------
  procedure Put_Line(File  : in File_Type;
                     Item  : in String;
                     Width : in Field;
                     Fill  : in Character);

  procedure Put_Line(Item  : in String;
                     Width : in Field;
                     Fill  : in Character);

  -----------------------------------------------------------------------------
  procedure Put_Line(File : in File_Type;
                     Item  : in Integer;
                     Width : in Field;
                     Base  : in Number_Base;
                     Fill  : in Character);

  procedure Put_Line(Item  : in Integer;
                     Width : in Field;
                     Base  : in Number_Base;
                     Fill  : in Character);

  -----------------------------------------------------------------------------
  procedure Put_Line(File : in File_Type;
                     Item : in Float;
                     Fore : in Field;
                     Aft  : in Field;
                     Exp  : in Field;
                     Fill : in Character);

  procedure Put_Line(Item : in Float;
                     Fore : in Field;
                     Aft  : in Field;
                     Exp  : in Field;
                     Fill : in Character);

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Nothing just now ...
  -----------------------------------------------------------------------------

end TJa.Standard_IO;
