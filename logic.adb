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

  -- function Random_Coordinate(StartX, StartY, Width, Height : in Integer) return Random_Array_Type is
  --   Random_Cords         : Random_Array_Type;
  --   Random_X, Random_Y   : Integer;
  --   Uniform_X, Uniform_Y : Float;
  --
  --   Size_Of_Water : Integer := 2;
  -- begin
  --   return Null;
  -- end Random_Coordinate;


end logic;
