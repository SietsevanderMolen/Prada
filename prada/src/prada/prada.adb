with GNAT.OS_Lib;
with GNAT.Command_Line;
with POSIX.Unsafe_Process_Primitives;

procedure Prada is
   package GCL renames GNAT.Command_Line;
   package PUPP renames POSIX.Unsafe_Process_Primitives;
   procedure ParseCommandLine;
   procedure RunPradaInstall;

   Config : GCL.Command_Line_Configuration;

   InstallPackage : aliased Boolean := False;
   UpdateSystem   : aliased Boolean := False;

   procedure ParseCommandLine
   is
   begin
      GCL.Define_Switch (
         Config, InstallPackage'Access,
         "-S", Help => "Install a package");
      GCL.Define_Switch (
         Config, UpdateSystem'Access,
         "-Su", Help => "Update system");

      GCL.Getopt (Config);
   exception
      when GCL.Invalid_Switch =>
         GNAT.OS_Lib.OS_Exit (1);
      when GCL.Exit_From_Command_Line =>
         GNAT.OS_Lib.OS_Exit (0);
   end ParseCommandLine;

   procedure RunPradaInstall
   is
      Arguments : POSIX.POSIX_String_List;
   begin
      POSIX.Append (Arguments, POSIX.To_POSIX_String (GCL.Get_Argument));
      PUPP.Exec_Search ("./exe/pradainstall", Arguments);
   end RunPradaInstall;
begin
   ParseCommandLine;

   if InstallPackage then
      RunPradaInstall;
   elsif UpdateSystem then
      null; -- Update called
   end if;

   GNAT.OS_Lib.OS_Exit (0);
end Prada;
