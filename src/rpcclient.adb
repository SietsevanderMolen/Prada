with AWS.Client, AWS.Response, AWS.Messages;
use  AWS, AWS.Messages;
with Ada; use Ada;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with helpers; use helpers;

package body rpcclient is

   function Get_JSON (query : in String) return Unbounded_String
   is
      Stream1   : constant Trace_Handle := Create ("rpcclient");
      Page      : AWS.Response.Data;
      S         : Messages.Status_Code;
      Con_Fail  : exception;
      Mime_Fail : exception;
      url       : Unbounded_String;
      str       : Unbounded_String;

   begin
      Parse_Config_File;   --  parses default ./.gnatdebug
      --  Create the search url where we can reach the rpc
      url  := "https://aur.archlinux.org/rpc.php?type=search&arg="
               & To_Unbounded_String (query);
      Page := Client.Get (To_String (url));
      S    := AWS.Response.Status_Code (Page);

      Trace (Stream1, To_String (url), "Called url ");

      --  First check for connection/general errors
      if S not in Success then
         raise Con_Fail with
            "Unable to retrieve data => Status Code: "
            & Image (S) & " Reason: " & Reason_Phrase (S);
      --  I wonder if this is seriously necessary, but I'll add it anyway
      elsif AWS.Response.Content_Type (Page) /= "application/json" then
         raise Mime_Fail with
         "Wrong mimetype. Found " &
         AWS.Response.Content_Type (Page) &
         " but expected application/json.";
      end if;

      --  Get actual json message
      str := AWS.Response.Message_Body (Page);

      --  Fix the escaping of the string
      --  because within a json string \" is escaped
      --  I guess that this is essentially a bug upstream.
      --  Contact adacore?
      str := Un_Escape_String (To_String (str));
      return str;
   end Get_JSON;
end rpcclient;
