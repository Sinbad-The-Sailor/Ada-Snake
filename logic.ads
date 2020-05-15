-- Ada Standard Libraries
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
-- TJa. Libraries
with TJa.Keyboard;              use TJa.Keyboard;


package logic is
--------------------------------------------------------------------------------
  dx, dy : Integer := 0;
--------------------------------------------------------------------------------
  type Random_Array_Type is
    array (1..2) of Integer;
--------------------------------------------------------------------------------
  procedure Update_Position(X_Pos, Y_Pos : in out Integer);
  procedure Update_Direction(Key : in Key_Type);
  procedure Check_Exit_Game(Key : in Key_Type; Running : out Boolean);
--------------------------------------------------------------------------------
  -- function Random_Coordinate(StartX, StartY, Width, Height : in Integer)
  --   return Random_Array_Type;
--------------------------------------------------------------------------------


private


end logic;
