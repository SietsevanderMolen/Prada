with GNATCOLL.Traces; use GNATCOLL.Traces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with GNAT.Encode_UTF8_String;
with Results; use Results;

package body helpers is
   ---------------------
   --  JSON Iterator  --
   ---------------------
   procedure ParseJSON
      (json : in JSON_Value)
   is
      Stream1         : constant Trace_Handle := Create ("aurhelper");
      returntype      : Unbounded_String;
      numresults      : Natural;
      errormsg        : Unbounded_String;
      RPC_fail        : exception;
   begin
      returntype := Get (Val => json, Field => "type");

      --  The rpc returned a search result
      if returntype = "search" then
         --  Save number of results for checking
         numresults := Get (Val => json, Field => "resultcount");

         if numresults > 1 then
            --  Parse array of results
            declare
               A_JSON_Array : constant JSON_Array :=
                           Get (Val => json, Field => "results");
               A_JSON_Value : JSON_Value;
               Array_Length : constant Natural := Length (A_JSON_Array);
            begin
               for J in 1 .. Array_Length loop
                  A_JSON_Value := Get (Arr   => A_JSON_Array,
                                       Index => J);
                  ParseSingleResultJSON (A_JSON_Value);
               end loop;
            end;
         else
            null; --  No results! Handle me!
         end if;
      --  The rpc returned an error
      elsif returntype = "error" then
         errormsg := Get (Val => json, Field => "results");
         raise RPC_Fail with To_String (errormsg);
      end if;

      --  Handle the exceptions, log and propagate them
      exception
         --   when Fail : Con_Fail | Mime_Fail =>
            --   Trace (Stream1, Exception_Message (Fail));
            --   raise;
         when Fail : others =>
            Trace (Stream1, Exception_Message (Fail));
            raise;
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

   ----------------------------------
   --  Fixes escaping of a string  --
   ----------------------------------
   function Un_Escape_String (Text : String) return UTF8_Unbounded_String is
      First : Integer;
      Last  : Integer;
      Unb   : Unbounded_String;
      Idx   : Natural;

   begin
      First := Text'First;
      Last  := Text'Last;

      --  Trim blancks and the double quotes

      while First <= Text'Last and then Text (First) = ' ' loop
         First := First + 1;
      end loop;
      if First <= Text'Last and then Text (First) = '"' then
         First := First + 1;
      end if;

      while Last >= Text'First and then Text (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if Last >= Text'First and then Text (Last) = '"' then
         Last := Last - 1;
      end if;

      Idx := First;
      while Idx <= Last loop
         if Text (Idx) = '\' then
            Idx := Idx + 1;

            if Idx > Text'Last then
               raise Invalid_JSON_Stream with
               "Unexpected escape character at end of line";
            end if;

            case Text (Idx) is
               when 'u' | 'U' =>
                  declare
                     I : constant Short_Integer :=
                        Short_Integer'Value
                           ("16#" & Text (Idx + 1 .. Idx + 4) & "#");
                        function Unch is new Ada.Unchecked_Conversion
                           (Short_Integer, Wide_Character);
                  begin
                     Append
                        (Unb,
                        GNAT.Encode_UTF8_String.Encode_Wide_String
                           ("" & Unch (I)));
                     Idx := Idx + 4;
                  end;

               when '"' =>
                  --  Keep \"
                  Append (Unb, '\' & '"');
               when '\' =>
                  Append (Unb, '\');
               when '/' =>
                  Append (Unb, '/');
               when 'b' =>
                  Append (Unb, ASCII.BS);
               when 'f' =>
                  Append (Unb, ASCII.FF);
               when 'n' =>
                  Append (Unb, ASCII.LF);
               when 'r' =>
                  Append (Unb, ASCII.CR);
               when 't' =>
                  Append (Unb, ASCII.HT);
               when others =>
                  raise Invalid_JSON_Stream with
                  "Unexpected escape sequence '\" & Text (Idx) & "'";
            end case;

         else
            Append
               (Unb, Text (Idx));
         end if;

         Idx := Idx + 1;
      end loop;

      return Unb;
   end Un_Escape_String;
end helpers;
