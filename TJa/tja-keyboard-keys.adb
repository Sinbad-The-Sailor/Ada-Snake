-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                    T J A . K E Y B O A R D . K E Y S                    |--
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
--|   2000-04-13  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

package body TJa.Keyboard.Keys is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| To_Key_Code_Type, To_Key_Type
  -----------------------------------------------------------------------------

  function To_Key_Code_Type(Source : in Key_Type)
           return Key_Code_Type is

  begin
    -- Special cases.
    if Source = Null_Key then
      return Key_Null;
    elsif Source = Undefined_Key then
      return Key_Undefined;
    end if;

    -- Unlegal cases.
    if Is_Character(Source) then
      raise Keyboard_Key_Data_Error;
    end if;

    -- Normal cases.
    if Is_Control(Source) then
      -- Control key.
      return Key_Code_Type(Character'Pos(To_Character(Source)));
    elsif Is_Special(Source) then
      if Is_Meta(Source) then
	-- Meta key.
	if (Is_Meta_Backspace(Source)) then
	  return Key_Meta_Backspace;
	elsif (Is_Meta_Tab(Source)) then
	  return Key_Meta_Tab;
	elsif (Is_Meta_Return(Source)) then
	  return Key_Meta_Return;
	elsif (Is_Meta_Esc(Source)) then
	  return Key_Meta_Esc;
	else
	  -- Should not happen, but ...
	  raise Keyboard_Key_Data_Error;
	end if;
      elsif Is_Function(Source) then
	-- Function key.
	if Is_F1(Source) then
	  return Key_F1;
	elsif Is_F2(Source) then
	  return Key_F2;
	elsif Is_F3(Source) then
	  return Key_F3;
	elsif Is_F4(Source) then
	  return Key_F4;
	elsif Is_F5(Source) then
	  return Key_F5;
	elsif Is_F6(Source) then
	  return Key_F6;
	elsif Is_F7(Source) then
	  return Key_F7;
	elsif Is_F8(Source) then
	  return Key_F8;
	elsif Is_F9(Source) then
	  return Key_F9;
	elsif Is_F10(Source) then
	  return Key_F10;
	elsif Is_F11(Source) then
	  return Key_F11;
	elsif Is_F12(Source) then
	  return Key_F12;
	else
	  -- Should not happen, but ...
	  raise Keyboard_Key_Data_Error;
	end if;
      elsif Is_Arrow(Source) then
	-- Arrow key.
	if Is_Up_Arrow(Source) then
	  return Key_Up_Arrow;
	elsif Is_Down_Arrow(Source) then
	  return Key_Down_Arrow;
	elsif Is_Left_Arrow(Source) then
	  return Key_Left_Arrow;
	elsif Is_Right_Arrow(Source) then
	  return Key_Right_Arrow;
	else
	  -- Should not happen, but ...
	  raise Keyboard_Key_Data_Error;
	end if;
      else
	-- Simple special key?
	if Is_Return(Source) then
	  return Key_Return;
	elsif Is_Delete(Source) then
	  return Key_Delete;
	elsif Is_Backspace(Source) then
	  return Key_Backspace;
	elsif Is_Tab(Source) then
	  return Key_Tab;
	elsif Is_Esc(Source) then
	  return Key_Esc;
        -- Home, End, PgUp, PgDn, Insert, 
	elsif Is_Home(Source) then
	  return Key_Home;
	elsif Is_End(Source) then
	  return Key_End;
	elsif Is_PgUp(Source) then
	  return Key_PgUp;
	elsif Is_PgDn(Source) then
	  return Key_PgDn;
	elsif Is_Insert(Source) then
	  return Key_Insert;
        -- Abort, Again, Properties, Undo, Front,
	-- Copy, Open, Paste, Search and Cut.
	elsif Is_Abort(Source) then
	  return Key_Abort;
	elsif Is_Again(Source) then
	  return Key_Again;
	elsif Is_Properties(Source) then
	  return Key_Properties;
	elsif Is_Undo(Source) then
	  return Key_Undo;
	elsif Is_Front(Source) then
	  return Key_Front;
	elsif Is_Copy(Source) then
	  return Key_Copy;
	elsif Is_Open(Source) then
	  return Key_Open;
	elsif Is_Paste(Source) then
	  return Key_Paste;
	elsif Is_Search(Source) then
	  return Key_Search;
	elsif Is_Cut(Source) then
	  return Key_Cut;
        else
	  -- Should not happen, but ...
	  raise Keyboard_Key_Data_Error;
	end if;
      end if;
    else
      -- Some strange key! Possible that it's an undefined key.
      return Key_Undefined;
    end if;
  end To_Key_Code_Type;

  --===========================================================================
  function To_Key_Type(Source : in Key_Code_Type)
           return Key_Type is

  begin
    raise Not_Yet_Implemented;
    return Null_Key;
  end To_Key_Type;


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
end TJa.Keyboard.Keys;
