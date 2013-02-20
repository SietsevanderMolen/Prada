with Ada.Text_IO;

procedure Prada
is
   package ATIO renames Ada.Text_IO;

   function SayHi
      return String;

   function SayHi
      return String
   is
   begin
      return "This does nothing.";
   end SayHi;

begin
   ATIO.Put_Line (SayHi);
end Prada;
