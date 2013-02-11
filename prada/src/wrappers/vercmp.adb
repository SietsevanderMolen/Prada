with GNAT.Expect;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Vercmp is
   function Run
      (LocalVersion : String;
       RemoteVersion : String)
      return Integer
   is
      Fd           : GNAT.Expect.Process_Descriptor;
      Timeout      : constant Integer := 1000; --  1 sec
      Output       : GNAT.Expect.Expect_Match;
      Result       : Unbounded_String;
      VercmpResult : Integer;
   begin
      GNAT.Expect.Non_Blocking_Spawn
          (Fd, "/usr/bin/vercmp",
            (1 => new String'(RemoteVersion),
             2 => new String'(LocalVersion)));
      GNAT.Expect.Expect (Fd, Output, "(.)*", Timeout);
      Result := To_Unbounded_String (GNAT.Expect.Expect_Out (Fd));
      VercmpResult := Integer'Value (To_String (Result));
      return VercmpResult;
   exception
      --  Re-raise but with a bit more helpful message
      when GNAT.Expect.Invalid_Process =>
         raise GNAT.Expect.Invalid_Process
            with "Can not find 'vercmp' executable, "
               & "is it installed in /usr/bin/vercmp?";
      when Constraint_Error =>
         raise Constraint_Error
            with """/usr/bin/vercmp " & String'(RemoteVersion) & " "
               & String'(LocalVersion) & """ returned """ & To_String (Result)
               & """, expected an int > " & Integer'Image (Integer'First)
               & " & < " & Integer'Image (Integer'Last);
   end Run;
end Vercmp;
