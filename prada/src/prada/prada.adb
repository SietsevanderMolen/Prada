with GNAT.OS_Lib;
with Ada.Text_IO;
with GNAT.Command_Line;

procedure Prada is
   package ATIO renames Ada.Text_IO;
   package GCL renames GNAT.Command_Line;

   procedure ParseCommandLine;
   procedure ShowHelp;

   procedure ParseCommandLine
   is
      No_Parameter : exception;
   begin
      loop
         case GCL.Getopt ("h S?") is
            when 'h' => ShowHelp;
            when 'S' =>
               if GNAT.Command_Line.Parameter = "" then
                  ATIO.Put_Line ("S given");
               else
                  raise GCL.Invalid_Parameter;
               end if;
            when others => exit;
         end case;
      end loop;
   exception
      when GCL.Invalid_Switch =>
         ATIO.Put_Line ("Prada: Switch '" & GCL.Full_Switch & "' not allowed");
         ShowHelp;
      when GCL.Invalid_Parameter =>
         if GCL.Parameter = "" then
            ATIO.Put_Line ("Prada: '" & GCL.Full_Switch & "' needs a parameter");
         else
            ATIO.Put_Line ("Prada: '" & GCL.Parameter & "' is an invalid parameter for switch '" & GCL.Full_Switch  & "'");
         end if;
         ShowHelp;
   end ParseCommandLine;

   procedure ShowHelp is
   begin
      ATIO.Put_Line ("Prada: No functions currently implemented");
   end ShowHelp;
begin
   ParseCommandLine;
   GNAT.OS_Lib.OS_Exit (0);
end Prada;
