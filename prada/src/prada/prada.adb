with Ada.Text_IO;
with GNAT.OS_Lib;

procedure Prada
is
   package ATIO renames Ada.Text_IO;

   procedure ShowHelp;

   procedure ShowHelp
   is
   begin
      ATIO.Put_Line ("Prada: No functions currently implemented");
   end ShowHelp;

begin
   ShowHelp;
   GNAT.OS_Lib.OS_Exit (0);
end Prada;
