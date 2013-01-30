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
       (json   : in String) return AurReply;
   --   Creates an aurreply from a given json string
private
   function CreateAurPackage
      (json : in JSON_Value;
        refnum : in Natural) return AurPackage;
   --  Creates a aurpackage from a given JSON_Value
   --  Used for the individual package results
end AurReplyFactory;
