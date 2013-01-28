with Results; use Results;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body AURParser is
   ---------------------------------------------
   --  Handles the errors the rpc can return  --
   ---------------------------------------------
   procedure ErrorHandler
      (msg : in String)
   is
   begin
      if msg = "Too many package results." then
         raise TooManyResultsFail with "Too many results for query";
      elsif msg = "Query arg too small" then
         raise QueryTooSmallFail with "The given query is too small";
      elsif msg = "Incorrect request type specified." then
         raise IncorrectReqFail with "Incorrect request type";
      end if;
   end ErrorHandler;

   -------------------------
   --  aur JSON Iterator  --
   -------------------------
   procedure ParseAURResponse
      (json : in JSON_Value)
   is
      returntype        : Unbounded_String;
   begin
      --  Get the return type
      returntype := Get (Val => json, Field => "type");

      --  First check if the aur returned an error
      if returntype = "error" then
         --  Handle the error
         ErrorHandler (Get (Val => json, Field => "results"));
      --  Then check for the other expected return types
      elsif returntype = "search" or
            returntype = "msearch" or
            returntype = "multiinfo" then
         --  Parse array of results
         declare
            A_JSON_Array : constant JSON_Array := Get (Val => json,
                                                       Field => "results");
            A_JSON_Value : JSON_Value;
            Array_Length : constant Natural := Length (A_JSON_Array);
         begin
            for J in 1 .. Array_Length loop
               A_JSON_Value := Get (Arr   => A_JSON_Array,
                                    Index => J);
               ParseSingleAURResult (A_JSON_Value);
            end loop;
         end;
      else
         --  Any other return types must be wrong
         raise UnknownReturnTypeFail with "Unknown return type: "
                                         & To_String (returntype);
      end if;
   end ParseAURResponse;

   -------------------------------------
   --  Parses a single search result  --
   -------------------------------------
   procedure ParseSingleAURResult
      (json : in JSON_Value)
   is
      res         : Result;
   begin
      res := Results.Create (
         Get (Val => json, Field => "Description"),
         Get (Val => json, Field => "ID"),
         Get (Val => json, Field => "License"),
         Get (Val => json, Field => "Name"),
         Get (Val => json, Field => "NumVotes"),
         Get (Val => json, Field => "OutOfDate"),
         Get (Val => json, Field => "URL"),
         Get (Val => json, Field => "URLPath"),
         Get (Val => json, Field => "Version")
      );
      PrettyPrint (res);
   end ParseSingleAURResult;
end AURParser;
