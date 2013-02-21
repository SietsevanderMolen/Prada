with GNAT.OS_Lib;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure PradaInstall is
   procedure ParseCommandLine;

   procedure ParseCommandLine is
   begin
      Put (Command_Name & " ");
      for Arg in 1 .. Argument_Count loop
         Put (Argument (Arg) & " ");
      end loop;
      New_Line;
   end ParseCommandLine;
begin
   ParseCommandLine;

   GNAT.OS_Lib.OS_Exit (0);
end PradaInstall;
