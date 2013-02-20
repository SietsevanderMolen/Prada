with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
--  with GNAT.Command_Line;  use GNAT.Command_Line;

procedure PradaInstall is
   procedure ParseCommandLine is
   begin
      null;
   end ParseCommandLine;
begin
   ParseCommandLine;
   GNAT.OS_Lib.OS_Exit(0);
end PradaInstall;
