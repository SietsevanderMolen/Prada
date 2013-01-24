with GNAT.Expect;
with Ada.Text_IO;

procedure aurhelper is
   Fd : GNAT.Expect.Process_Descriptor;
   Timeout : constant Integer := 10000; -- 10 sec
   Result : GNAT.Expect.Expect_Match;
begin
   GNAT.Expect.Non_Blocking_Spawn (Fd, "/bin/echo", (1 => new String'
   ("from Expect")));
   GNAT.Expect.Expect (Fd, Result, ".*", Timeout);
   Ada.Text_IO.Put_Line ("Seen: " & GNAT.Expect.Expect_Out (Fd));

   Ada.Text_IO.Put_Line ("Hello, World again with Ada!");
end aurhelper;
