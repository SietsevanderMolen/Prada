package Sys is
   procedure Create_Directory (Path : String);
   function Get_Temp_Dir return String;
   procedure Remove_Directory (Path : String);
   function FindUID return String;
end Sys;