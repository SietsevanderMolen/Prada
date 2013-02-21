with AUnit.Test_Caller;
with PradaUpdate_Test;

package body PradaUpdate_Test_Suite is

   package Caller is new AUnit.Test_Caller (PradaUpdate_Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
         (Caller.Create ("Return code when no input",
         PradaUpdate_Test.Test_NoInputReturnCode'Access));
      return Ret;
   end Suite;

end PradaUpdate_Test_Suite;
