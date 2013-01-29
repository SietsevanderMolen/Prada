with GNAT.Command_Line;  use GNAT.Command_Line;
--  with AurReplies; use AurReplies;
--  with AurInterface;
with Ada.Text_IO;

procedure prada is
   --  results : AurReply;
   procedure Display_Help;

   procedure Display_Help is
   begin
      Ada.Text_IO.Put_Line ("usage: prada [option] [package] [package] [...]");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("   -Ss|-Ssq    - searches for package");
      --  Ada.Text_IO.Put_Line ("   -Si         - outputs info for package");
   end Display_Help;
begin
   loop
      case Getopt ("h -help") is
         when 'h' => Display_Help;
         when '-' =>
            if Full_Switch = "-help" then
               Display_Help;
            end if;
         when others => exit;
      end case;
   end loop;
exception
   when Invalid_Switch | Invalid_Parameter =>
      Ada.Text_IO.Put_Line ("Prada: Option '" &
                             Full_Switch & "' is not valid.");
      --  results := AurInterface.search ("aa");
end prada;
