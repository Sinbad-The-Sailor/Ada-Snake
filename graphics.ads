--Ada Standard Libraries
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
--TJa Libraries
with TJa.Window.Text;                   use TJa.Window.Text;
with TJa.Keyboard;                      use TJa.Keyboard;
with TJa.Window.Elementary;             use TJa.Window.Elementary;

package graphics is
--------------------------------------------------------------------------------
  Scaling_Factor : constant Integer := 2;
--------------------------------------------------------------------------------
  type Picture_Type is
    array (Positive Range <>, Positive Range <>) of Integer;
--------------------------------------------------------------------------------
  procedure Transform_To_Graphical(X_Pos, Y_pos : in Integer; G_X, G_Y : out Integer);
  procedure Get_Picture_Dimensions(File_Name : in String; X_Dim, Y_dim : out Integer);
  procedure Load_Picture(File_Name : in String; Picture : out Picture_Type);
  procedure Put_Picture(Picture : in Picture_Type; X_Pos, Y_Pos, X_Range, Y_Range : in Integer);
  procedure Fix_Picture(Picture : in Picture_Type; X_Pos, Y_Pos, X_Range, Y_Range : in Integer);
  procedure Put_Bushes(Picture : in Picture_Type; Background_Start_X, Background_Start_Y, Width, Height : in Integer);
  procedure Check_Out_Of_Bounds(X_Gra, Y_Gra, Width, Height, Size_Of_Water : in Integer; Running : in out Boolean);
  procedure Setup_Terminal;
  procedure Start_Up_Screen(Name : out String);
  procedure Put_End_Screen(X_Start, Y_Start, X_Range, Y_Range : in Integer);
  procedure Exit_Game;
--------------------------------------------------------------------------------
private
end graphics;
