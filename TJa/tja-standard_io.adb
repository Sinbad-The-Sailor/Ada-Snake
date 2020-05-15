-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                      T J A . S T A N D A R D _ I O                      |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  1.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2003                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2003-04-16  Version 1.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
--|   2015-03-22  Updated with Get_Immediate by Torbjörn Jonsson.           |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

package body TJa.Standard_IO is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -- No local information just now.


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Get_Immediate
  -----------------------------------------------------------------------------

  procedure Get_Immediate(File : in     File_Type;
			  Item :    out Character) is
    
    -----------------------------------------------------------------
    procedure getc_immediate(stream      : in     FILEs;
    			     ch          :    out int;
    			     end_of_file :    out int);
    pragma Import(C, getc_immediate, "getc_immediate");
    -----------------------------------------------------------------
    
    ch          : int;
    end_of_file : int;
    Stream      : FILEs;
    
  begin
    Stream := C_Stream(File);
    getc_immediate(Stream, ch, end_of_file);

    if ferror(Stream) /= 0 then
      raise Device_Error;
      
    elsif end_of_file /= 0 then
      raise End_Error;
      
    else
      if Ch < 0 then
	Ch := 256 + Ch;
      end if;
      Item := Character'Val(Ch);
    end if;
  end Get_Immediate;
  -----------------------------------------------------------------------------
  procedure Get_Immediate(Item : out Character) is
    
  begin
    Get_Immediate(Standard_Input, Item);
  end Get_Immediate;

  --===========================================================================
  
  procedure Get_Immediate(File      : in     File_Type;
			  Item      :    out Character;
			  Available :    out Boolean) is

    -----------------------------------------------------------------
    procedure getc_immediate_nowait
      (stream      : in    FILEs;
       ch          :    out int;
       end_of_file :    out int;
       avail       :    out int);
    pragma Import(C, getc_immediate_nowait, "getc_immediate_nowait");
    -----------------------------------------------------------------
    
    ch          : int;
    end_of_file : int;
    avail       : int;
    Stream      : FILEs;
    
  begin
    --    FIO.Check_Read_Status (AP (File));
    Available := True;

    Stream := C_Stream(File);
    getc_immediate_nowait(Stream, ch, end_of_file, avail);

    if ferror(Stream) /= 0 then
      raise Device_Error;

    elsif end_of_file /= 0 then
      raise End_Error;

    elsif avail = 0 then
      Available := False;
      Item := ASCII.NUL;

    else
      Available := True;
      if Ch < 0 then
	Ch := 256 + Ch;
      end if;
      Item := Character'Val(Ch);
    end if;
  end Get_Immediate;
  -----------------------------------------------------------------------------
  procedure Get_Immediate(Item      : out Character;
			     Available : out Boolean) is
  begin
    Get_Immediate(Standard_Input, Item, Available);
  end Get_Immediate;
  
  -----------------------------------------------------------------------------
  --| Get_Line
  -----------------------------------------------------------------------------

  procedure Get_Line(File : in     File_Type;
                     Item :    out Character) is

  begin
    Get(File, Item);
    Skip_Line(File);
  end Get_Line;
  -----------------------------------------------------------------------------
  procedure Get_Line(Item : out Character) is

  begin
    Get_Line(Standard_Input, Item);
  end Get_Line;

  --===========================================================================
  procedure Get_Line(File : in     File_Type;
                     Item :    out String) is

  begin
    Get(File, Item);
    Skip_Line(File);
  end Get_Line;
  -----------------------------------------------------------------------------
  procedure Get_Line(Item : out String) is

  begin
    Get_Line(Standard_Input, Item);
  end Get_Line;

  --===========================================================================
  procedure Get_Line(File  : in     File_Type;
                     Item  :    out Integer;
                     Width : in     Field := 0) is

  begin
    Get(File, Item, Width);
    Skip_Line;
  end Get_Line;
  -----------------------------------------------------------------------------
  procedure Get_Line(Item  :    out Integer;
                     Width : in     Field := 0) is

  begin
    Get_Line(Standard_Input, Item, Width);
  end Get_Line;

  --===========================================================================
  procedure Get_Line(File : in     File_Type;
                     Item :    out Float) is

  begin
    Get(File, Item);
    Skip_Line;
  end Get_Line;
  -----------------------------------------------------------------------------
  procedure Get_Line(Item : out Float) is

  begin
    Get_Line(Standard_Input, Item);
  end Get_Line;

  -----------------------------------------------------------------------------
  --| Put
  -----------------------------------------------------------------------------

  procedure Put(File  : in File_Type;
                Item  : in Character;
                Width : in Field;
                Fill  : in Character) is

  begin
    for I in 2..Width loop
      Put(File, Fill);
    end loop;
    Put(File, Item);
  end Put;
  -----------------------------------------------------------------------------
  procedure Put(Item  : in Character;
                Width : in Field;
                Fill  : in Character) is

  begin
    Put(Standard_Output, Item, Width, Fill);
  end Put;

  --===========================================================================
  procedure Put(File  : in File_Type;
                Item  : in String;
                Width : in Field;
                Fill  : in Character) is

  begin
    for I in 1 .. (Width - Item'Length) loop
      Put(File, Fill);
    end loop;
    Put(File, Item);
  end Put;
  -----------------------------------------------------------------------------
  procedure Put(Item  : in String;
                Width : in Field;
                Fill  : in Character) is

  begin
    Put(Standard_Output, Item, Width, Fill);
  end Put;

  --===========================================================================
  procedure Put(File  : in File_Type;
                Item  : in Integer;
                Width : in Field;
                Base  : in Number_Base;
                Fill  : in Character) is

  begin
    -- NYI: Filling isn't fixed ...
    Put(File, Item, Width => Width, Base => Base);
  end Put;
  -----------------------------------------------------------------------------
  procedure Put(Item  : in Integer;
                Width : in Field;
                Base  : in Number_Base;
                Fill  : in Character) is

  begin
    Put(Standard_Output, Item, Width, Base, Fill);
  end Put;

  --===========================================================================
  procedure Put(File : in File_Type;
                Item : in Float;
                Fore : in Field;
                Aft  : in Field;
                Exp  : in Field;
                Fill : in Character) is

  begin
    -- NYI: Filling isn't fixed ...
    Put(File, Item, Fore, Aft, Exp);
  end Put;
  -----------------------------------------------------------------------------
  procedure Put(Item : in Float;
                Fore : in Field;
                Aft  : in Field;
                Exp  : in Field;
                Fill : in Character) is

  begin
    Put(Standard_Output, Item, Fore, Aft, Exp, Fill);
  end Put;

  -----------------------------------------------------------------------------
  --| Put_Line
  -----------------------------------------------------------------------------

  procedure Put_Line(File  : in File_Type;
                     Item  : in Character;
                     Width : in Field;
                     Fill  : in Character) is

  begin
    Put(File, Item, Width, Fill);
    New_Line(File);
  end Put_Line;
  -----------------------------------------------------------------------------
  procedure Put_Line(Item  : in Character;
                     Width : in Field;
                     Fill  : in Character) is

  begin
    Put_Line(Standard_Output, Item, Width, Fill);
  end Put_Line;

  --===========================================================================
  procedure Put_Line(File  : in File_Type;
                     Item  : in String;
                     Width : in Field;
                     Fill  : in Character) is

  begin
    Put(File, Item, Width, Fill);
    New_Line(File);
  end Put_Line;
  -----------------------------------------------------------------------------
  procedure Put_Line(Item  : in String;
                     Width : in Field;
                     Fill  : in Character) is

  begin
    Put_Line(Standard_Output, Item, Width, Fill);
  end Put_Line;

  --===========================================================================
  procedure Put_Line(File : in File_Type;
                     Item  : in Integer;
                     Width : in Field;
                     Base  : in Number_Base;
                     Fill  : in Character) is

  begin
    Put(File, Item, Width, Base, Fill);
    New_Line(File);
  end Put_Line;
  -----------------------------------------------------------------------------
  procedure Put_Line(Item  : in Integer;
                     Width : in Field;
                     Base  : in Number_Base;
                     Fill  : in Character) is

  begin
    Put_Line(Standard_Output, Item, Width, Base, Fill);
  end Put_Line;

  --===========================================================================
  procedure Put_Line(File : in File_Type;
                     Item : in Float;
                     Fore : in Field;
                     Aft  : in Field;
                     Exp  : in Field;
                     Fill : in Character) is

  begin
    Put(File, Item , Fore, Aft, Exp, Fill);
    New_Line(File);
  end Put_Line;
  -----------------------------------------------------------------------------
  procedure Put_Line(Item : in Float;
                     Fore : in Field;
                     Aft  : in Field;
                     Exp  : in Field;
                     Fill : in Character) is

  begin
    Put_Line(Standard_Output, Item, Fore, Aft, Exp, Fill);
  end Put_Line;


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
end TJa.Standard_IO;
