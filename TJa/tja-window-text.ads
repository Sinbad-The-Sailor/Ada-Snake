-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                      T J A . W I N D O W . T E X T                      |--
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
--|   This package can be to modify text area in a normal text window.      |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Window.Text is

  -----------------------------------------------------------------------------
  --| "Colour_Type" is the type to use when changing colours on foreground and
  --| background for text. "Coulor_Type" is just a subtype to "Colour_Type" so
  --| we dont have to discuss english versus american english.
  -----------------------------------------------------------------------------

  -- Old definition:
  -- type Colour_Type is
  --   (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);
  
  type Colour_Type is
    -- Low contrast colors
    (Black, Red, Green, Yellow, Blue, Magenta, Cyan, Light_Grey,
     -- High contrast colors
     Dark_Grey, Bright_Red, Bright_Green, Bright_Yellow, 
     Bright_Blue, Bright_Magenta, Bright_Cyan, White);
  
  subtype Color_Type is Colour_Type;

  Default_Foreground_Colour : constant Colour_Type := Black;
  Default_Background_Colour : constant Colour_Type := White;

  -----------------------------------------------------------------------------
  --| "Undelined_Mode_Type", "Inverted_Mode_Type" and "Bold_Mode_Type" are the
  --| types to use when changing these text modes.
  -----------------------------------------------------------------------------

  type Underlined_Mode_Type is (On, Off);
  type Inverted_Mode_Type is (On, Off);
  type Bold_Mode_Type is (On, Off);

  Default_Underlined_Mode : constant Underlined_Mode_Type := Off;
  Default_Inverted_Mode   : constant Inverted_Mode_Type   := Off;
  Default_Bold_Mode       : constant Bold_Mode_Type       := Off;

  -----------------------------------------------------------------------------
  --| "Set_Default_Colours" sets all colours to new default values.
  --| "Set_Default_Text_Modes" sets all text modes to new default values.
  --|
  --| Observe that the current settings doesn't change. "Reset_..." has to be
  --| done to change them.
  -----------------------------------------------------------------------------

  procedure Set_Default_Colours(Foreground, Background : in Colour_Type);
  procedure Set_Default_Text_Modes(Underlined : in Underlined_Mode_Type;
                                   Inverted   : in Inverted_Mode_Type;
                                   Bold       : in Bold_Mode_Type);

  -----------------------------------------------------------------------------
  --| "Reset_Colours" resets all colours to default values.
  --| "Reset_Text_Modes" resets all text modes to default values.
  --| "Reset_Colours_And_Text_Modes" resets all colours and text modes to
  --| default values.
  --|
  --| Observe that default values can be different than original settings.
  --|
  --| "Reset_To_Original_Window_Settings" restores the original colours and
  --| text_modes (as it was before starting the program).
  -----------------------------------------------------------------------------

  procedure Reset_Colours;
  procedure Reset_Text_Modes;
  procedure Reset_Colours_And_Text_Modes;

  procedure Reset_To_Original_Window_Settings;

  -----------------------------------------------------------------------------
  --| "Set_Colours" sets all colours to new values.
  --| "Set_Text_Modes" sets all text modes to new values.
  --| "Set_Colours_And_Text_Modes" sets all colours and modes to new values.
  -----------------------------------------------------------------------------

  procedure Set_Colours(Foreground, Background : in Colour_Type);
  procedure Set_Text_Modes(Underlined : in Underlined_Mode_Type;
                           Inverted   : in Inverted_Mode_Type;
                           Bold       : in Bold_Mode_Type);
  procedure Set_Colours_And_Text_Modes(Foreground : in Colour_Type;
                                       Background : in Colour_Type;
                                       Underlined : in Underlined_Mode_Type;
                                       Inverted   : in Inverted_Mode_Type;
                                       Bold       : in Bold_Mode_Type);

  -----------------------------------------------------------------------------
  --| "Set_Foreground_Colour" and "Set_Background_Colour" sets the colour to a
  --| new value.
  --|
  --| "Get_Foreground_Colour" and "Get_Background_Colour" receives the current
  --| settings.
  -----------------------------------------------------------------------------

  procedure Set_Foreground_Colour(Colour : in Colour_Type);
  procedure Set_Background_Colour(Colour : in Colour_Type);

  function Get_Foreground_Colour return Colour_Type;
  function Get_Background_Colour return Colour_Type;

  -----------------------------------------------------------------------------
  --| "Set_Underlined_Mode", "Set_Inverted_Mode" and "Set_Bold_Mode" sets the
  --| text mode to a new value.
  --|
  --| "Get_Underlined_Mode", "Get_Inverted_Mode" and "Get_Bold_Mode" receives
  --| the current settings.
  -----------------------------------------------------------------------------

  procedure Set_Underlined_Mode(Mode : in Underlined_Mode_Type := On);
  procedure Set_Inverted_Mode(Mode : in Inverted_Mode_Type := On);
  procedure Set_Bold_Mode(Mode : in Bold_Mode_Type := On);

  function Get_Underlined_Mode return Underlined_Mode_Type;
  function Get_Inverted_Mode return Inverted_Mode_Type;
  function Get_Bold_Mode return Bold_Mode_Type;

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Nothing just now.
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------

end TJa.Window.Text;
