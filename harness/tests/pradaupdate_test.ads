with AUnit.Test_Fixtures;

package PradaUpdate_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_NoInputReturnCode (T : in out Test);
   --  Test if the program gives the right exit code when no input is given
end PradaUpdate_Test;
