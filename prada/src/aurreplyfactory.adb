with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body AurReplyFactory is
   -------------------------------------
   --  Parses a single search result  --
   -------------------------------------
   function CreateAurPackage
      (json : in JSON_Value) return AurPackage
   is
      res         : AurPackage;
   begin
      res := AurPackages.Create (
         Get (Val => json, Field => "Name"),
         Get (Val => json, Field => "Maintainer"),
         Get (Val => json, Field => "ID"),
         Get (Val => json, Field => "Version"),
         Get (Val => json, Field => "CategoryID"),
         Get (Val => json, Field => "Description"),
         Get (Val => json, Field => "License"),
         Get (Val => json, Field => "NumVotes"),
         Get (Val => json, Field => "OutOfDate"),
         Get (Val => json, Field => "FirstSubmitted"),
         Get (Val => json, Field => "LastModified"),
         Get (Val => json, Field => "URL"),
         Get (Val => json, Field => "URLPath")
      );
      return res;
   end CreateAurPackage;

   function createAurReply
      (json : String) return AurReply
   is
      jsonreply   : JSON_Value;
      returntype  : Unbounded_String;
      temp        : AurReply;
      tempresults : AurReplies.AurPackageContainer.Vector;
      errormsg    : Unbounded_String;
   begin
      --  First convert the json string to a JSON_Value object
      jsonreply := JSONParser.ParseJSON (Strm => json);

      --  Get the return type
      returntype := Get (Val => jsonreply, Field => "type");

      --  First check if the aur returned an error
      if returntype = "error" then
         errormsg := Get (Val => jsonreply, Field => "results");
         if errormsg = "Too many package results." then
            raise TooManyResultsFail with "Too many results for query";
         elsif errormsg = "Query arg too small" then
            raise QueryTooSmallFail with "The given query is too small";
         elsif errormsg = "Incorrect request type specified." then
            raise IncorrectReqFail with "Incorrect request type";
         else
            raise UnknownAurErrorFail with "Aur returned an unexpected error";
         end if;
      --  Then check for the other expected return types
      elsif returntype = "search" or
            returntype = "msearch" or
            returntype = "multiinfo" then
         --  Parse array of results
         declare
            resultCount : constant Natural := Get (Val => jsonreply,
                                                   Field => "resultcount");
            A_reply_Array : constant JSON_Array := Get (Val => jsonreply,
                                                        Field => "results");
            A_reply_Value : JSON_Value;
            Array_Length : constant Natural := Length (A_reply_Array);
         begin
            for J in 1 .. Array_Length loop
               A_reply_Value := Get (Arr   => A_reply_Array,
                                    Index => J);
               tempresults.Append (CreateAurPackage (A_reply_Value));
            end loop;

            --  Create the actual AurReply object
            temp := AurReplies.Create (returntype, resultCount, tempresults);
            return temp;
         end;
      else
         --  Any other return types must be wrong
         raise UnknownReturnTypeFail with "Unknown return type: "
                                         & To_String (returntype);
      end if;
   end createAurReply;
end AurReplyFactory;
