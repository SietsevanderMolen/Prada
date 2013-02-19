with GNAT.Command_Line;
with GNAT.OS_Lib;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with QuickSearch;
with Search;
with Install;
with Update;

procedure prada is
   type Run_Mode is (Nop, DoSearch, DoQuickSearch,
                     DoInstall, DoUpdate);
   mode    : Run_Mode;
   query   : Ada.Strings.Unbounded.Unbounded_String;

   procedure DisplayHelp;
   --  display a short description on how to use the program
   procedure ParseCommandLine;
   --  Parse every switch on the command line

   procedure DisplayHelp is
   begin
      Ada.Text_IO.Put_Line ("usage: prada [option] [package]");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("   -S        - installs a package");
      Ada.Text_IO.Put_Line ("   -Ss|-Ssq  - searches for package");
      Ada.Text_IO.Put_Line ("   -Su       - updates installed aur packages");
      Ada.Text_IO.Put_Line ("   -h        - outputs this message");
      GNAT.OS_Lib.OS_Exit (0);
   end DisplayHelp;

   procedure ParseCommandLine is
   begin
      loop
         case GNAT.Command_Line.Getopt ("S Si Ss Ssq Su h") is
            when ASCII.NUL =>
               exit;
            when 'S' => --  Install commands
               if GNAT.Command_Line.Full_Switch = "S" then
                  mode := DoInstall;
               elsif GNAT.Command_Line.Full_Switch = "Su" then
                  mode := DoUpdate;
               elsif GNAT.Command_Line.Full_Switch = "Ss" then
                  mode := DoSearch;
               elsif GNAT.Command_Line.Full_Switch = "Ssq" then
                  mode := DoQuickSearch;
               else
                  raise GNAT.Command_Line.Invalid_Parameter;
               end if;
            when 'h' => --  Help commands
               DisplayHelp;
            when others =>
               raise GNAT.Command_Line.Invalid_Parameter;
         end case;
      end loop;

      --  parse search query
      loop
         declare
            S : constant String := GNAT.Command_Line.Get_Argument;
         begin
            exit when S'Length = 0;

            Ada.Strings.Unbounded.Append (query, S & ' ');
         end;
      end loop;

      exception
         when GNAT.Command_Line.Invalid_Parameter =>
            Ada.Text_IO.Put_Line ("Prada: Parameter '" &
               GNAT.Command_Line.Parameter & "' not valid for command '" &
               GNAT.Command_Line.Full_Switch & "'");
            GNAT.OS_Lib.OS_Exit (1);
         when GNAT.Command_Line.Invalid_Switch =>
            Ada.Text_IO.Put_Line ("Prada: Option '-" &
               GNAT.Command_Line.Full_Switch & "' is not valid.");
            GNAT.OS_Lib.OS_Exit (1);
   end ParseCommandLine;
begin
   ParseCommandLine;

   if mode = DoSearch then
      Search.Search (query);
   elsif mode = DoQuickSearch then
      QuickSearch.Search (query);
   elsif mode = DoInstall then
      Install.Install (query);
   elsif mode = DoUpdate then
      Update.Update;
   elsif mode = Nop then
      DisplayHelp;
   end if;
end prada;
