with GNATCOLL.JSON;

package jsonparser is
   TooManyResultsFail    : exception;
   IncorrectReqFail      : exception;
   QueryTooSmallFail     : exception;
   UnknownReturnTypeFail : exception;
   use GNATCOLL.JSON;
      procedure ParseJSON
         (json : in JSON_Value);

      procedure ParseSingleResultJSON
         (json : in JSON_Value);

      procedure ErrorHandler
         (msg  : in String);
end jsonparser;
