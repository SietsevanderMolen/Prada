with PradaUpdate_Test_Suite;
with PradaInstall_Test_Suite;
with Prada_Test_Suite;

package body Main_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (PradaUpdate_Test_Suite.Suite);
      Ret.Add_Test (PradaInstall_Test_Suite.Suite);
      Ret.Add_Test (Prada_Test_Suite.Suite);
      return Ret;
   end Suite;

end Main_Suite;
