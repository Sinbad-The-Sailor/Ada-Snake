package body logic is
--------------------------------------------------------------------------------
  procedure Update_Position(X_Pos, Y_Pos: in out Integer) is
  begin
    X_Pos := X_Pos + dx;
    Y_Pos := Y_Pos + dy;
  end Update_Position;
--------------------------------------------------------------------------------
  procedure Update_Direction(Key : in Key_Type) is
  begin
    if Is_Up_Arrow(key) and dy = 0 then
      dy := -1;
      dx :=  0;
    elsif Is_Down_Arrow(key) and dy = 0 then
      dy :=  1;
      dx :=  0;
    elsif Is_Left_Arrow(key) and dx = 0 then
      dx := -1;
      dy :=  0;
    elsif Is_Right_Arrow(key) and dx = 0 then
      dx :=  1;
      dy :=  0;
    else
      null;
    end if;
  end Update_Direction;
--------------------------------------------------------------------------------
  procedure Init_Snake(Snake : in out Snake_Position_Type) is
  begin
    for I in 1..Snake'Length loop
      Snake(I)(1) := 0;
      Snake(I)(2) := 0;
    end loop;
  end Init_Snake;
--------------------------------------------------------------------------------
  procedure Check_Exit_Game(Key : in Key_Type; Running : out Boolean) is
  begin
    if Is_Return(Key) then
      Running := False;
    else
      Running := True;
    end if;
  end Check_Exit_Game;
--------------------------------------------------------------------------------
  function Random_Coordinate(StartX, StartY, Width, Height : in Integer) return Random_Array_Type is
    Random_Cords         : Random_Array_Type;
    Random_X, Random_Y   : Integer;
    Uniform_X, Uniform_Y : Float;
    X_MIN, X_MAX         : Float;
    Y_MIN, Y_MAX         : Float;
    -- This should be a paramater with scaling.
    Size_Of_Water : Integer := 2;
    G : Ada.Numerics.Float_Random.Generator;
  begin
    Ada.Numerics.Float_Random.Reset(G);

    Uniform_X := Ada.Numerics.Float_Random.Random(G);
    Uniform_Y := Ada.Numerics.Float_Random.Random(G);

    X_MIN := Float(StartX + Size_Of_Water);
    X_MAX := Float(StartX + Width - 2*Size_Of_Water);
    Y_MIN := Float(StartY + Size_Of_Water);
    Y_MAX := Float(StartY + Height - 2*Size_Of_Water);

    Random_X := Integer((X_MAX - X_MIN)*Uniform_X + X_MIN);
    Random_Y := Integer((Y_MAX - Y_MIN)*Uniform_Y + Y_MIN);

    Random_Cords(1) := Random_X;
    Random_Cords(2) := Random_Y;
    return Random_Cords;
  end Random_Coordinate;
--------------------------------------------------------------------------------
end logic;
