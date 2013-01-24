with AUnit.Test_Fixtures;

package JSON_Parser_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_NoJSONError (T : in out Test);
   --  Test how well the program handles borked json

end JSON_Parser_Test;
