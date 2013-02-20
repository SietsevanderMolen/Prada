with AUnit.Test_Caller;
with Prada_Test;

package body Prada_Test_Suite is

   package Caller is new AUnit.Test_Caller (Prada_Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
         (Caller.Create ("Return code when no input",
         Prada_Test.Test_NoInputReturnCode'Access));
      return Ret;
   end Suite;

end Prada_Test_Suite;
