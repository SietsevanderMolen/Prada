with JSONParser; use JSONParser;

package AURParser is

   TooManyResultsFail    : exception;
   IncorrectReqFail      : exception;
   QueryTooSmallFail     : exception;
   UnknownReturnTypeFail : exception;

   procedure ParseAURResponse
      (json : in JSON_Value);

   procedure ParseSingleAURResult
      (json : in JSON_Value);

   procedure ErrorHandler
      (msg  : in String);
end AURParser;
