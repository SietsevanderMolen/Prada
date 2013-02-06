with Ada.Text_IO;

package body Util is
   function UserConfirm
      (Prompt : Unbounded_String)
      return Boolean
   is
      Answer : Character;
   begin
      Ada.Text_IO.Put (To_String (Prompt));
      loop
         Ada.Text_IO.Get_Immediate (Answer);
         case Answer is
            when 'Y' | 'y' | ASCII.LF =>
               Ada.Text_IO.New_Line;
               return True;
            when others  =>
               return False;
         end case;
      end loop;
   end UserConfirm;
end Util;
