with AUnit.Assertions; use AUnit.Assertions;
with jsonparser;
with GNATCOLL.JSON;

package body JSON_Parser_Test is
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

   procedure Test_QueryTooSmallError (T : in out Test) is
      use GNATCOLL.JSON;
      pragma Unreferenced (T);
      jsonstring : constant String :=
"{""type"":""error"",""resultcount"":0,""results"":""Query arg too small""}";
      json : JSON_Value;
   begin
      json := Read (Strm     => jsonstring,
                    Filename => "");
      jsonparser.ParseJSON (json => json);
      Assert (False, "No argument too small exception raised");
   exception
      when jsonparser.QueryTooSmallFail =>
         null;
   end Test_QueryTooSmallError;

   procedure Test_TooManyResultsError (T : in out Test) is
      use GNATCOLL.JSON;
      pragma Unreferenced (T);
      jsonstring : constant String :=
         "{""type"":""error"",""resultcount"":0,""" &
         "results"":""Too many package results.""}";
      json : JSON_Value;
   begin
      json := Read (Strm     => jsonstring,
                    Filename => "");
      jsonparser.ParseJSON (json => json);
      Assert (False, "No argument too small exception raised");
   exception
      when jsonparser.TooManyResultsFail =>
         null;
   end Test_TooManyResultsError;

   procedure Test_UnexpectedRequestType (T : in out Test) is
      use GNATCOLL.JSON;
      pragma Unreferenced (T);
      jsonstring : constant String :=
         "{""type"":""error"",""resultcount"":0,""" &
         "results"":""Incorrect request type specified.""}";
      json : JSON_Value;
   begin
      json := Read (Strm     => jsonstring,
                    Filename => "");
      jsonparser.ParseJSON (json => json);
      Assert (False, "No unexpected request type exception raised");
   exception
      when jsonparser.IncorrectReqFail =>
         null;
   end Test_UnexpectedRequestType;
end JSON_Parser_Test;