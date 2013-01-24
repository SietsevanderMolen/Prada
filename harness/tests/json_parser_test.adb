with AUnit.Assertions; use AUnit.Assertions;
with jsonparser;
with GNATCOLL.JSON;

package body JSON_Parser_Test is
   ---------------
   -- Test_Push --
   ---------------
   procedure Test_NoJSONError (T : in out Test) is
      use GNATCOLL.JSON;
      pragma Unreferenced (T);
      jsonstring : constant String := "borked";
      json : JSON_Value;
   begin
      json := Read (Strm     => jsonstring,
                    Filename => "");
      jsonparser.ParseJSON (json => json);
      Assert (False, "No Invalid_JSON_Stream exception raised");
   exception
      when GNATCOLL.JSON.Invalid_JSON_Stream =>
         null;
   end Test_NoJSONError;

end JSON_Parser_Test;
