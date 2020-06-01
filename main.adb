-- Ada Standard Libraries
 with Ada.Text_IO;              use Ada.Text_IO;
 with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
-- TJa. Libraries
with TJa.Keyboard;              use TJa.Keyboard;
with TJa.Window.Elementary;     use TJa.Window.Elementary;

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
  Score        : Integer;
  Name         : String(1..3);

  Snake_Position : Snake_Position_Type(1..1000);
  Fruit_Position : Random_Array_Type;

  Highscore_List : Highscore_List_Type;

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
  PosX    := 5;
  PosY    := 5;
  Speed   := 0.10;
  Score   := 0;
  Snake_Length := 5;
  Init_Snake(Snake_Position);
  Read_Highscore("./Highscores/highscore.txt", Highscore_List);
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

  Fruit_Position := Random_Coordinate(Background_Start_X, Background_Start_Y, Game_Width, Game_Height, 2*Bush_X);

  Put_Picture(Background_Image, Background_Start_X, Background_Start_Y, Background_X, Background_Y);
  Put_Picture(Fruit_Image, Fruit_Position(1), Fruit_Position(2), Fruit_X, Fruit_Y);
  Put_Bushes(Bush_Image, Background_Start_X, Background_Start_Y, Game_Width, Game_Height);

  Setup_Terminal;
end Init_Graphics;
--------------------------------------------------------------------------------
procedure Tick is
  Key            : Key_Type;
  Key_Pressed    : Boolean;
  Temp_X, Temp_Y : Integer;
begin
  Get_Immediate(Key, Key_Pressed);
  Update_Direction(Key);
  Update_Position(PosX, PosY);

  PreX := PosX;
  PreY := PosY;

  for I in 1..Snake_Length loop
    Temp_X := Snake_Position(I)(1);
    Temp_Y := Snake_Position(I)(2);
    Snake_Position(I)(1) := PreX;
    Snake_Position(I)(2) := PreY;
    PreX := Temp_X;
    PreY := Temp_Y;
  end loop;

   if Snake_Length > 5 then
    Check_Snake_Intersection(Snake_Length, Snake_Position, Running);
   end if;

end Tick;
--------------------------------------------------------------------------------
procedure Render is
  Game_Width_Transformed  : Integer;
  Game_Height_Transformed : Integer;
  GX, GY       : Integer;
  PreGX, PreGY : Integer;
begin
  Transform_To_Graphical(PosX, PosY, GX, GY);
  Transform_To_Graphical(PreX, PreY, PreGX, PreGY);
  Game_Width_Transformed := Game_Width + Background_Start_X;
  Game_Height_Transformed := Game_Height + Background_Start_Y;

  Put_Picture(Snake_Piece_Image, GX, GY, Snake_X, Snake_Y);
  Fix_Picture(Background_Image, PreGX, PreGY , Snake_X, Snake_Y);
  Check_Out_Of_Bounds(GX, GY, Game_Width_Transformed, Game_Height_Transformed, Bush_X, Running);

  if Check_Fruit(GX, GY, Snake_X, Snake_Y, Fruit_X, Fruit_Y, Fruit_Position) = True then
    Speed        := Speed - 0.01;
    Snake_Length := Snake_Length + 1;
    Score        := Score + 10;
    Fix_Picture(Background_Image, Fruit_Position(1), Fruit_Position(2), Fruit_X, Fruit_Y);
    Fruit_Position := Random_Coordinate(Background_Start_X, Background_Start_Y, Game_Width, Game_Height, Bush_X);
    Put_Picture(Fruit_Image, Fruit_Position(1), Fruit_Position(2), Fruit_X, Fruit_Y);
  end if;
end Render;
--------------------------------------------------------------------------------
begin
  Start_Up_Screen(Name);
  Init_Logic;
  Init_Graphics;
  while Running = True loop
    delay Speed;
    Tick;
    Render;
  end loop;
  Put_End_Screen(Background_Start_X, Background_Start_Y, Background_X, Background_Y);

  if Score > Highscore_List(3).Score then
    Goto_XY(Background_Start_X+Integer(Background_X/2)-8, Background_Start_Y+Integer(Background_Y/2)-2);
    Put_Line("DU KOM MED PÅ LISTAN!");
    Add_Score(Name, Score, Highscore_List);
    Sort_Scores(Highscore_List);
    Write_Highscore("./Highscores/highscore.txt", Highscore_List);
  else
    Goto_XY(Background_Start_X+Integer(Background_X/2)-8, Background_Start_Y+Integer(Background_Y/2)-2);
    Put_Line("DU KOM INTE MED PÅ LISTAN!");
  end if;
    Put_Highscore(Background_Start_X+Integer(Background_X/2), Background_Start_Y+Integer(Background_Y/2), Highscore_List);
  Exit_Game;
end main;
