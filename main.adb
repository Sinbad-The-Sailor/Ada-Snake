-- Ada Standard Libraries
 with Ada.Text_IO;               use Ada.Text_IO;
-- with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
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
  GX, GY       : Integer;
  PreX, PreY   : Integer;
  Snake_Length : Integer;
  Score        : Integer;
  Name         : String(1..3);

  Snake_Position : Snake_Position_Type(1..1000);
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
  PosX    := 5;
  PosY    := 5;
  Speed   := 0.10;
  Score   := 0;
  Snake_Length := 5;
  Init_Snake(Snake_Position);
  Fruit_Position := Random_Coordinate(Background_Start_X, Background_Start_Y, Game_Width, Game_Height);
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

   if Check_Fruit(2*PosX+40, 2*PosY+10, Snake_X, Snake_Y, Fruit_X, Fruit_Y, Fruit_Position) = True then
     Speed        := Speed - 0.01;
     Snake_Length := Snake_Length + 1;
     Score        := Score + 10;
   end if;
end Tick;
--------------------------------------------------------------------------------
procedure Render is
  Game_Width_Transformed  : Integer;
  Game_Height_Transformed : Integer;
begin
  Transform_To_Graphical(PosX, PosY, GX, GY);
  Game_Width_Transformed := Game_Width + 40;
  Game_Height_Transformed := Game_Height + 10;

  Put_Picture(Snake_Piece_Image, GX, GY, Snake_X, Snake_Y);
  Fix_Picture(Background_Image, PreX*2+40, PreY*2+10, Snake_X, Snake_Y);
  Check_Out_Of_Bounds(GX, GY, Game_Width_Transformed, Game_Height_Transformed, Running);


  if Check_Fruit(2*PosX+40, 2*PosY+10, Snake_X, Snake_Y, Fruit_X, Fruit_Y, Fruit_Position) = True then
    Fix_Picture(Background_Image, Fruit_Position(1), Fruit_Position(2), Fruit_X, Fruit_Y);
    Fruit_Position := Random_Coordinate(Background_Start_X, Background_Start_Y, Game_Width, Game_Height);
    Put_Picture(Fruit_Image, Fruit_Position(1), Fruit_Position(2), Fruit_X, Fruit_Y);
  end if;
end Render;
--------------------------------------------------------------------------------
begin
  Init_Logic;
  Init_Graphics;
  while Running = True loop
    delay Speed;
    Tick;
    Render;
  end loop;

  -- Last_Score := Highscore(3).Score;
  -- if New_Score > Last_Score then
  --   Add_Score(New_Score);
  --   Sort_Score(Highscore);
  --   Put("Du kom med!");
  -- else
  --   Put("Du suger!");
  -- end if;


  -- Display_highscores

  --Skriv till filen.



  Exit_Game;
end main;
