with GNAT.Command_Line;
with GNAT.OS_Lib;
with AurReplies; use AurReplies;
with AurInterface;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure prada is
   type Run_Mode is (Nop, Search, SearchQuick, Install, Info);

   results : AurReply;
   mode    : Run_Mode;
   query   : Ada.Strings.Unbounded.Unbounded_String;

   procedure DisplayHelp;
   --  display a short description on how to use the program
   procedure ParseCommandLine;
   --  Parse every switch on the command line

   procedure DisplayHelp is
   begin
      Ada.Text_IO.Put_Line ("usage: prada [option] [package] [package] [...]");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("   -Ss|-Ssq    - searches for package");
      Ada.Text_IO.Put_Line ("   -h|--help   - outputs this message");
      GNAT.OS_Lib.OS_Exit (0);
   end DisplayHelp;

   procedure ParseCommandLine is
   begin
      loop
         case GNAT.Command_Line.Getopt ("S Si Ss Ssq h -help") is
            when ASCII.NUL =>
               exit;
            when 'S' =>
               if GNAT.Command_Line.Full_Switch = "S" then
                  mode := Install;
               elsif GNAT.Command_Line.Full_Switch = "Si" then
                  mode := Info;
               elsif GNAT.Command_Line.Full_Switch = "Ss" then
                  mode := Search;
               elsif GNAT.Command_Line.Full_Switch = "Ssq" then
                  mode := SearchQuick;
               else
                  raise GNAT.Command_Line.Invalid_Switch;
               end if;
            when 'h' => DisplayHelp;
            when '-' =>
               if GNAT.Command_Line.Full_Switch = "-help" then
                  DisplayHelp;
               end if;
            when others => null;
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
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Invalid_Parameter =>
            Ada.Text_IO.Put_Line ("Prada: Option -'" &
              GNAT.Command_Line.Full_Switch & "' is not valid.");
            GNAT.OS_Lib.OS_Exit (1);
   end ParseCommandLine;
begin
   ParseCommandLine;

   if mode = Search then
      results := AurInterface.search (query);
      results.PrettyPrintResults;
   elsif mode = SearchQuick then
      Ada.Text_IO.Put_Line ("Quick Search!");
   elsif mode = Install then
      Ada.Text_IO.Put_Line ("Install!");
   elsif mode = Info then
      Ada.Text_IO.Put_Line ("Info!");
   end if;
end prada;
