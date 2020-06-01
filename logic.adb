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
  procedure Check_Snake_Intersection(Length : in Integer; Snake : in Snake_Position_Type; Running : in out Boolean) is
  begin
    for I in 2..Length loop
      if Snake(1)(1) = Snake (I)(1) and Snake(1)(2) = Snake(I)(2) then
        Running := False;
      end if;
    end loop;
  end Check_Snake_Intersection;
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
  procedure Read_Highscore(File_Name : in String; Highscore_List : out Highscore_List_Type) is
    File         : File_Type;
    Temp_String  : String(1..3);
    Temp_Integer : Integer;
    I            : Integer := 1;
  begin
    Open(File, In_File, File_Name);
    while not End_Of_File(File) loop
      Get(File, Temp_String);
      Get(File, Temp_Integer);
      Highscore_List(I).Name  := Temp_String;
      Highscore_List(I).Score := Temp_Integer;
      I := I + 1;
    end loop;
    Close(File);
  end Read_Highscore;
  --------------------------------------------------------------------------------
  procedure Add_Score(New_Name : in String; New_Score : in Integer; Highscore_List : out Highscore_List_Type) is
    Last : Integer := Highscore_List'Last;
  begin
    Highscore_List(Last).Name  := New_Name;
    Highscore_List(Last).Score := New_Score;
  end Add_Score;
--------------------------------------------------------------------------------
  procedure Write_Highscore(File_Name : in String; Highscore_List : in Highscore_List_Type) is
    File : File_Type;
    Last : Integer := Highscore_List'Last;
  begin
    Open(File, Out_File, File_Name);
    for I in 1..Last loop
      Put(File, Highscore_List(I).Name);
      Put(File, " ");
      Put(File, Highscore_List(I).Score, Width => 0);
      New_Line(File);
    end loop;
  end Write_Highscore;
--------------------------------------------------------------------------------
  procedure Swap_Scores(New_Score, Old_Score : in out Score_Type) is
    Temp_Score : Score_Type;
  begin
    Temp_Score := New_Score;
    New_Score  := Old_Score;
    Old_Score  := Temp_Score;
  end Swap_Scores;
--------------------------------------------------------------------------------
  procedure Sort_Scores(Highscore_List : in out Highscore_List_Type) is
  begin
    for I in Highscore_List'First + 1 .. Highscore_List'Last loop
        for J in reverse I..Highscore_List'Last loop
          if Highscore_List(J).Score >= Highscore_List(J-1).Score then
            Swap_Scores(Highscore_List(J), Highscore_List(J-1));
          end if;
        end loop;
    end loop;
  end Sort_Scores;
--------------------------------------------------------------------------------
  procedure Put_Highscore(X_Placement, Y_Placement : in Integer; Highscore_List : in Highscore_List_Type) is
    Org_X : Integer := X_Placement;
    Org_Y : Integer := Y_Placement;
  begin
    Goto_XY(Org_X, Org_Y);
    Put_Line("HIGHSCORES!");
    for I in Highscore_List'Range loop
      Goto_XY(Org_X, Org_Y + I);
      Put(I, Width => 1);
      Put(" ");
      Put(Highscore_List(I).Name);
      Put(" ");
      Put(Highscore_List(I).Score, Width => 0);
    end loop;
  end Put_Highscore;
-------------------------------------------------------------------------------
  procedure Fill_Matrix(X, Y, Size_X, Size_Y : in Integer; Matrix : out Coordinate_Matrix) is
    Temp_X : Integer := X;
    Temp_Y : Integer := Y;
    Org_X  : Integer := X;
    Org_Y  : Integer := Y;
  begin
    for J in 1..Size_Y loop
      for I in 1..Size_X loop
        Matrix(J,I)(1) := Org_X;
        Org_X := Org_X + 1;
      end loop;
      Org_X := Temp_X;
    end loop;

    for I in 1..Size_X loop
      for J in 1..Size_Y loop
        Matrix(J,I)(2) := Org_Y;
        Org_Y := Org_Y + 1;
      end loop;
      Org_Y := Temp_Y;
    end loop;
  end Fill_Matrix;
--------------------------------------------------------------------------------
  function Check_Fruit(X_Pos, Y_pos, Snake_X, Snake_Y, Fruit_X, Fruit_Y : in Integer; Fruit_Pos : in Random_Array_Type) return Boolean is
    Temp_Boolean : Boolean;
    Snake_Coordinate_Matrix : Coordinate_Matrix(1..Snake_X, 1..Snake_Y);
    Fruit_Coordinate_Matrix : Coordinate_Matrix(1..Fruit_X, 1..Fruit_Y);
  begin
    Fill_Matrix(X_Pos, Y_Pos, Snake_X, Snake_Y, Snake_Coordinate_Matrix);
    Fill_Matrix(Fruit_Pos(1), Fruit_Pos(2), Fruit_X, Fruit_Y, Fruit_Coordinate_Matrix);
    for I in 1..Snake_X loop
      for J in 1..Snake_Y loop
        for K in 1..Fruit_X loop
          for L in 1..Fruit_Y loop
            if (Snake_Coordinate_Matrix(J,I)(1) = Fruit_Coordinate_Matrix(L,K)(1) and
               Snake_Coordinate_Matrix(J,I)(2) = Fruit_Coordinate_Matrix(L,K)(2))
                then
                 Temp_Boolean := True;
                 return Temp_Boolean;
            end if;
          end loop;
        end loop;
      end loop;
    end loop;
    return False;
  end Check_Fruit;
--------------------------------------------------------------------------------
  function Random_Coordinate(StartX, StartY, Width, Height, Size_Of_Water : in Integer) return Random_Array_Type is
    Random_Cords         : Random_Array_Type;
    Random_X, Random_Y   : Integer;
    Uniform_X, Uniform_Y : Float;
    X_MIN, X_MAX         : Float;
    Y_MIN, Y_MAX         : Float;
    G : Ada.Numerics.Float_Random.Generator;
  begin
    Ada.Numerics.Float_Random.Reset(G);

    Uniform_X := Ada.Numerics.Float_Random.Random(G);
    Uniform_Y := Ada.Numerics.Float_Random.Random(G);

    X_MIN := Float(StartX + Size_Of_Water);
    X_MAX := Float(StartX + Width - 4*Size_Of_Water);
    Y_MIN := Float(StartY + Size_Of_Water);
    Y_MAX := Float(StartY + Height - 4*Size_Of_Water);

    Random_X := Integer((X_MAX - X_MIN)*Uniform_X + X_MIN);
    Random_Y := Integer((Y_MAX - Y_MIN)*Uniform_Y + Y_MIN);

    Random_Cords(1) := Random_X;
    Random_Cords(2) := Random_Y;
    return Random_Cords;
  end Random_Coordinate;
--------------------------------------------------------------------------------
end logic;
