with AUnit.Assertions; use AUnit.Assertions;
with JSONParser; use JSONParser;
with AURParser; use AURParser;

package body JSON_Parser_Test is
   procedure Test_QueryTooSmallError (T : in out Test) is
      jsonstring : constant String :=
"{""type"":""error"",""resultcount"":0,""results"":""Query arg too small""}";
      json : JSON_Value;
      pragma Unreferenced (T);
   begin
      json := ParseJSON (Strm     => jsonstring);
      ParseAURResponse (json);
      Assert (False, "No argument too small exception raised");
   exception
      when AURParser.QueryTooSmallFail =>
         null;
   end Test_QueryTooSmallError;

   procedure Test_TooManyResultsError (T : in out Test) is
      jsonstring : constant String :=
         "{""type"":""error"",""resultcount"":0,""" &
         "results"":""Too many package results.""}";
      json : JSON_Value;
      pragma Unreferenced (T);
   begin
      json := ParseJSON (Strm     => jsonstring);
      ParseAURResponse (json);
      Assert (False, "No argument too small exception raised");
   exception
      when AURParser.TooManyResultsFail =>
         null;
   end Test_TooManyResultsError;

   procedure Test_UnexpectedRequestType (T : in out Test) is
      jsonstring : constant String :=
         "{""type"":""error"",""resultcount"":0,""" &
         "results"":""Incorrect request type specified.""}";
      json : JSON_Value;
      pragma Unreferenced (T);
   begin
      json := ParseJSON (Strm     => jsonstring);
      ParseAURResponse (json);
      Assert (False, "No unexpected request type exception raised");
   exception
      when AURParser.IncorrectReqFail =>
         null;
   end Test_UnexpectedRequestType;

   procedure Test_UnexpectedReturnType (T : in out Test) is
      jsonstring : constant String :=
         "{""type"":""thisiswrong"",""resultcount"":0,""" &
         "results"":""this is not parsed.""}";
      json : JSON_Value;
      pragma Unreferenced (T);
   begin
      json := ParseJSON (Strm     => jsonstring);
      ParseAURResponse (json);
      Assert (False, "No unexpected return type exception raised");
   exception
      when AURParser.UnknownReturnTypeFail =>
         null;
   end Test_UnexpectedReturnType;
end JSON_Parser_Test;
