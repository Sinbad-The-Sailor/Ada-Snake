package body graphics is
--------------------------------------------------------------------------------
  procedure Transform_To_Graphical(X_Pos, Y_pos : in Integer; G_X, G_Y : out Integer) is
  begin
    G_X := X_Pos*Scaling_Factor + 40;
    G_Y := Y_Pos*Scaling_Factor + 10;
  end Transform_To_Graphical;
--------------------------------------------------------------------------------
  procedure Get_Picture_Dimensions(File_Name : in String; X_Dim, Y_Dim : out Integer) is
    File        : File_Type;
    Temp_String : String(1..10);
  begin
    Open(File, In_File, File_Name);
    While not End_Of_Line(File) loop
      Get(File, Temp_String);
      X_Dim := Integer'Value(Temp_String(7..10));
      Skip_Line(File);
      Get(File, Temp_String);
      Y_Dim := Integer'Value(Temp_String(7..10));
    end loop;
    Close(File);
  end Get_Picture_Dimensions;
--------------------------------------------------------------------------------
  procedure Load_Picture(File_Name : in String; Picture : out Picture_Type) is
    File                   : File_Type;
    X_Boundary, Y_Boundary : Integer;
    Temp_Integer           : Integer;
  begin
    Get_Picture_Dimensions(File_Name, X_Boundary, Y_Boundary);
    Open(File, In_File, File_Name);
    Skip_Line(File);
    Skip_Line(File);
    for I in 1..Y_Boundary loop
      for J in 1..X_Boundary loop
        Get(File, Temp_Integer);
        Picture(I,J) := Temp_Integer;
      end loop;
    end loop;
    Close(File);
  end Load_Picture;
--------------------------------------------------------------------------------
  procedure Put_Picture(Picture : in Picture_Type; X_Pos, Y_Pos, X_Range, Y_Range : in Integer) is
    Temp_Color   : Colour_Type;
    Temp_Integer : Integer;
    Temp_X_Pos   : Integer := X_Pos;
    Org_X        : Integer := X_Pos;
    Org_Y        : Integer := Y_Pos;
  begin
    for I in 1..Y_Range loop
      for J in 1..X_Range loop
        Goto_XY(Org_X, Org_Y);
        Temp_Integer := Picture(I, J);
        Temp_Color   := Colour_Type'Val(Temp_Integer);
        Set_Background_Colour(Temp_Color);
        Put(" ");
        Org_X := Org_X + 1;
      end loop;
      New_Line;
      Org_X := Temp_X_Pos;
      Org_Y := Org_Y + 1;
    end loop;
    Reset_Colours;
  end Put_Picture;
--------------------------------------------------------------------------------
  procedure Fix_Picture(Picture : in Picture_Type; X_Pos, Y_Pos, X_Range, Y_Range : in Integer) is
    Temp_Color   : Colour_Type;
    Temp_Integer : Integer;
    Temp_X_Pos   : Integer := X_Pos;
    Org_X        : Integer := X_Pos;
    Org_Y        : Integer := Y_Pos;
  begin
    for I in 1..Y_Range loop
      for J in 1..X_Range loop
        Goto_XY(Org_X, Org_Y);
        Temp_Integer := Picture(Integer(Float(Org_Y/2)+10.0), Integer(Float(Org_X/2)+40.0));
--        Temp_Integer := Picture(Integer(Float(Org_Y)-40.0), Integer(Float(Org_X)-10.0));
--        Temp_Integer := Picture(Org_Y, Org_X);
        Temp_Color   := Colour_Type'Val(Temp_Integer);
        Set_Background_Colour(Temp_Color);
        Put(" ");
        Org_X := Org_X + 1;
      end loop;
      New_Line;
      Org_X := Temp_X_Pos;
      Org_Y := Org_Y + 1;
    end loop;
  end Fix_Picture;
--------------------------------------------------------------------------------
  procedure Put_Bushes(Picture : in Picture_Type; Background_Start_X, Background_Start_Y, Width, Height : in Integer) is
    Org_X : Integer := Background_Start_X;
    Org_Y : Integer := Background_Start_Y;
    Bottom_Y : Integer := (Org_Y + Height - 2);
    Right_X  : Integer := (Org_X + Width - 2 );
    ----------------------------------------------------------------------------
    procedure Put_Top_Bottom_Bushes(Temp_X, Temp_Y: in Integer) is
      Number_Of_Bushes : Integer;
      Copy_X           : Integer := Temp_X;
    begin
      Number_Of_Bushes := Integer(Width/2);
      for I in 1..Number_Of_Bushes loop
        Goto_XY(Copy_X, Temp_Y);
        Put_Picture(Picture, Copy_X, Temp_Y, 2, 2);
        Copy_X := Copy_X + 2;
      end loop;
    end Put_Top_Bottom_Bushes;
    ----------------------------------------------------------------------------
    procedure Put_Middle_Bushes(Temp_X, Temp_Y : in Integer) is
      Number_Of_Bushes : Integer;
      Copy_Y           : Integer := Temp_Y + 2;
    begin
      Number_Of_Bushes := Integer((Height - 2 - 2)/2);
      for I in 1..Number_Of_Bushes loop
        Goto_XY(Temp_X, Copy_Y);
        Put_Picture(Picture, Temp_X, Copy_Y, 2, 2);
        Copy_Y := Copy_Y + 2;
      end loop;
    end Put_Middle_Bushes;
    ----------------------------------------------------------------------------
  begin
    Put_Top_Bottom_Bushes(Org_X, Org_Y);
    Put_Top_Bottom_Bushes(Org_X, Bottom_Y);
    Put_Middle_Bushes(Org_X, Org_Y);
    Put_Middle_Bushes(Right_X, Org_Y);
  end Put_Bushes;
--------------------------------------------------------------------------------
  procedure Check_Out_Of_Bounds(X_Gra, Y_Gra, Width, Height, Size_Of_Water : in Integer; Running : in out Boolean) is
    Background_Start_X : Integer := 40;
    Background_Start_Y : Integer := 10;
  begin
    if (X_Gra <= Size_Of_Water + Background_Start_X or X_Gra >= Width - 2*Size_Of_Water) or
       (Y_Gra <= Size_Of_Water + Background_Start_Y or Y_Gra >= Height - 2*Size_Of_Water) then
         Running := False;
    end if;
  end Check_Out_Of_Bounds;
--------------------------------------------------------------------------------
  procedure Setup_Terminal is
  begin
    Set_Buffer_Mode(Off);
    Set_Echo_Mode(Off);
    Cursor_Invisible;
  end Setup_Terminal;
--------------------------------------------------------------------------------
  procedure Start_Up_Screen(Name : out String) is
  begin
    Set_Background_Colour(White);
    Put_Line("THIS IS ADA SNAKE!");
    Put("ENTER YOUR NAME: ");
    Get(Name);
    Skip_Line;
    Reset_Colours_And_Text_Modes;
  end Start_Up_Screen;
--------------------------------------------------------------------------------
  procedure Put_End_Screen(X_Start, Y_Start, X_Range, Y_Range : in Integer) is
    Org_X : Integer := X_Start;
    Org_Y : Integer := Y_Start;
    Temp_X : Integer := X_Start;
  begin
    Set_Background_Colour(White);
    for I in 1..Y_Range loop
      for J in 1..X_Range loop
        Goto_XY(Org_X, Org_Y);
        Put(" ");
        Org_X := Org_X + 1;
      end loop;
      Org_X := Temp_X;
      Org_Y := Org_Y + 1;
    end loop;
  end Put_End_Screen;
--------------------------------------------------------------------------------
  procedure Exit_Game is
  begin
    Set_Echo_Mode(On);
    Set_Buffer_Mode(On);
    Cursor_Visible;
    Reset_Colours_And_Text_Modes;
    Reset_To_Original_Window_Settings;
  end Exit_Game;
--------------------------------------------------------------------------------
end graphics;
