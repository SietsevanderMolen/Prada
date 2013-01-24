with AUnit.Test_Caller;
with JSON_Parser_Test;

package body JSON_Parser_Test_Suite is

   package Caller is new AUnit.Test_Caller (JSON_Parser_Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
         (Caller.Create ("Borked input error",
         JSON_Parser_Test.Test_NoJSONError'Access));

      Ret.Add_Test
         (Caller.Create ("Query too small error",
         JSON_Parser_Test.Test_QueryTooSmallError'Access));

      return Ret;
   end Suite;

end JSON_Parser_Test_Suite;
