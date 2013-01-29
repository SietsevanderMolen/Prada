with JSONParser; use JSONParser;
with AurReplies; use AurReplies;
with AurPackages; use AurPackages;

package AurReplyFactory is

   TooManyResultsFail    : exception;
   IncorrectReqFail      : exception;
   QueryTooSmallFail     : exception;
   UnknownAurErrorFail   : exception;
   UnknownReturnTypeFail : exception;

   function createAurReply
       (json : in String) return AurReply;
private
   function CreateAurPackage
      (json : in JSON_Value) return AurPackage;
end AurReplyFactory;
