with AUnit.Assertions; use AUnit.Assertions;
with GNAT.OS_Lib; use GNAT.OS_Lib;

--  with PradaInstall;

package body PradaInstall_Test is
   procedure Test_NoInputReturnCode (T : in out Test) is
      Result : Integer;
      Arguments : constant Argument_List :=
         (1 => new String'("")
      );
      pragma Unreferenced (T);
   begin
      Spawn
         (Program_Name           => "./exe/pradainstall",
          Args                   => Arguments,
          Output_File_Descriptor => Standout,
          Return_Code            => Result
         );
      Assert (Result = 0,
         "Return code should be 0 when no input is given. Was: "
         & Integer'Image (Result));
   end Test_NoInputReturnCode;
end PradaInstall_Test;
