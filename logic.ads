-- Ada Standard Libraries
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
-- TJa. Libraries
with TJa.Keyboard;              use TJa.Keyboard;


package logic is
--------------------------------------------------------------------------------
  type Random_Array_Type is
    array (1..2) of Integer;

  type Position_Type is
    array (1..2) of Integer;

  type Snake_Position_Type is
    array(Positive Range <>) of Position_Type;

  type Coordinate is
    array(1..2) of Integer;

  type Coordinate_Matrix is
    array (Positive Range <>, Positive Range <>) of Coordinate;
--------------------------------------------------------------------------------
  dx, dy : Integer := 0;
--------------------------------------------------------------------------------
  procedure Update_Position(X_Pos, Y_Pos : in out Integer);
  procedure Update_Direction(Key : in Key_Type);
  procedure Init_Snake(Snake : in out Snake_Position_Type);
  procedure Check_Snake_Intersection(Length : in Integer; Snake : in Snake_Position_Type; Running : in out Boolean);
  procedure Check_Exit_Game(Key : in Key_Type; Running : out Boolean);

  procedure Fill_Matrix(X, Y, Size_X, Size_Y : in Integer; Matrix : out Coordinate_Matrix);
--------------------------------------------------------------------------------
  function Check_Fruit(X_Pos, Y_pos, Snake_X, Snake_Y, Fruit_X, Fruit_Y : in Integer; Fruit_Pos : in Random_Array_Type)
    return Boolean;
  function Random_Coordinate(StartX, StartY, Width, Height : in Integer)
    return Random_Array_Type;
--------------------------------------------------------------------------------


private
end logic;
