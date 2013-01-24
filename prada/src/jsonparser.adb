with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Results; use Results;

package body jsonparser is
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

   ---------------------
   --  JSON Iterator  --
   ---------------------
   procedure ParseJSON
      (json : in JSON_Value)
   is
      numresults          : Natural;
      MalformedJSONFail   : exception;
   begin
      --  Check this json for trouble
      PreParseChecks (json);

      --  The rpc returned an array of search results
      --  Save number of results for checking
      numresults := Get (Val => json, Field => "resultcount");

      --  Parse array of results
      declare
         A_JSON_Array : constant JSON_Array := Get (Val => json,
                                                    Field => "results");
         A_JSON_Value : JSON_Value;
         Array_Length : constant Natural := Length (A_JSON_Array);
      begin
         if Array_Length = numresults then
            for J in 1 .. Array_Length loop
               A_JSON_Value := Get (Arr   => A_JSON_Array,
                                    Index => J);
               ParseSingleResultJSON (A_JSON_Value);
            end loop;
         else
            raise MalformedJSONFail with
               "Resultcount doesn't conform to rpc promises";
         end if;
      end;
   end ParseJSON;

   -------------------------------------
   --  Parses a single search result  --
   -------------------------------------
   procedure ParseSingleResultJSON
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
   end ParseSingleResultJSON;

   procedure PreParseChecks
      (json : in JSON_Value)
   is
      returntype            : Unbounded_String;
      numresults            : Natural;
      NI_Fail               : exception;
      UnknownReturnTypeFail : exception;
   begin
      --  Get the return type
      returntype := Get (Val => json, Field => "type");
      --  Get amount of returned results
      numresults := Get (Val => json, Field => "resultcount");

      --  First make sure that this is an expected return type
      if returntype = "search" or
         returntype = "msearch" or
         returntype = "multiinfo"
      then
         --  Everything ok, parse this
         null;
      elsif returntype = "error" then
         --  Handle the error
         ErrorHandler (Get (Val => json, Field => "results"));
      elsif returntype = "info" then
         --  This shouldn't be seen
         raise NI_Fail with
            "rpc returned info, but this is not implemented";
      else
         --  Any other return types must be wrong
         raise UnknownReturnTypeFail with "Unknown return type";
      end if;

      --  There are no results
      if numresults = 0 then
         null; --  Handle me!
      end if;
   end PreParseChecks;
end jsonparser;
