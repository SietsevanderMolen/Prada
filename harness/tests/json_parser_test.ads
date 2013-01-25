with AUnit.Test_Fixtures;

package JSON_Parser_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_NoJSONError (T : in out Test);
   --  Test how well the program handles borked json
   --
   procedure Test_QueryTooSmallError (T : in out Test);
   --  Test how well the program handles small query errors

   procedure Test_TooManyResultsError (T : in out Test);
   --  Test how well the program handles too many results errors

   procedure Test_UnexpectedRequestType (T : in out Test);
   --  Test how well the program handles wrong request type errors
end JSON_Parser_Test;
