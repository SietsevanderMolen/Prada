with AUnit.Test_Caller;
with PradaInstall_Test;

package body PradaInstall_Test_Suite is

   package Caller is new AUnit.Test_Caller (PradaInstall_Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
         (Caller.Create ("Return code when no input",
         PradaInstall_Test.Test_NoInputReturnCode'Access));
      return Ret;
   end Suite;

end PradaInstall_Test_Suite;
