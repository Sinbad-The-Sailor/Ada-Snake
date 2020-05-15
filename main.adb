-- Ada Standard Libraries
-- with Ada.Text_IO;               use Ada.Text_IO;
-- with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
-- TJa. Libraries
with TJa.Keyboard;              use TJa.Keyboard;

-- Internal Libraries
with logic;                     use logic;
with graphics;                  use graphics;

procedure main is
--------------------------------------------------------------------------------
  Running      : Boolean := False;
  Speed        : Duration;
  PosX, PosY   : Integer;
  PreX, PreY   : Integer;
  Snake_Length : Integer;

  Fruit_Position : Random_Array_Type;

  Game_Width         : constant Integer := 320;
  Game_Height        : constant Integer :=  80;
  Background_Start_X : constant Integer :=  40;
  Background_Start_Y : constant Integer :=  10;

  Background_Image                : Picture_Type(1..350, 1..350);
  Background_X, Background_Y      : Integer;

  Fruit_Image                     : Picture_Type(1..4, 1..4);
  Fruit_X, Fruit_Y                : Integer;

  Snake_Piece_Image               : Picture_Type(1..4, 1..4);
  Snake_X, Snake_Y                : Integer;

  Bush_Image                      : Picture_Type(1..4, 1..4);
  Bush_X, Bush_Y                  : Integer;
--------------------------------------------------------------------------------
procedure Init_Logic is
begin
  Running := True;
  PosX    := 35;
  PosY    := 35;
  Speed   := 0.25;
  Snake_Length := 5;
end Init_Logic;
--------------------------------------------------------------------------------
procedure Init_Graphics is
begin
  Get_Picture_Dimensions("./Images/background.txt", Background_X, Background_Y);
  Load_Picture("./Images/background.txt", Background_Image);

  Get_Picture_Dimensions("./Images/snake.txt", Snake_X, Snake_Y);
  Load_Picture("./Images/snake.txt", Snake_Piece_Image);

  Get_Picture_Dimensions("./Images/fruit.txt", Fruit_X, Fruit_Y);
  Load_Picture("./Images/fruit.txt", Fruit_Image);

  Get_Picture_Dimensions("./Images/bush.txt", Bush_X, Bush_Y);
  Load_Picture("./Images/bush.txt", Bush_Image);

  Put_Picture(Background_Image, Background_Start_X, Background_Start_Y, Background_X, Background_Y);
  Put_Bushes(Bush_Image, Background_Start_X, Background_Start_Y, Game_Width, Game_Height);

  Setup_Terminal;
end Init_Graphics;
--------------------------------------------------------------------------------
procedure Tick is
  Key         : Key_Type;
  Key_Pressed : Boolean;
begin
  Get_Immediate(Key, Key_Pressed);
  Check_Exit_Game(Key, Running);
  Update_Direction(Key);
  Update_Position(PosX, PosY);
end Tick;
--------------------------------------------------------------------------------
procedure Render is
  GX, GY : Integer;
begin
  Transform_To_Graphical(PosX, PosY, GX, GY);
  Put_Picture(Snake_Piece_Image, GX, GY, Snake_X, Snake_Y);
end Render;
--------------------------------------------------------------------------------
begin
  Init_Logic;
  Init_Graphics;
  while Running loop
    delay Speed;
    Tick;
    Render;
  end loop;
  Exit_Game;
end main;
