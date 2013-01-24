with AWS.Client, AWS.Response, AWS.Messages;
with Ada.Exceptions; use Ada.Exceptions;
use  Ada, AWS, AWS.Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with helpers; use helpers;

package body rpcclient is

   function Get_JSON (query : in String) return JSON_Value
   is
      Stream1          : constant Trace_Handle := Create ("rpcclient");
      Page             : AWS.Response.Data;
      S                : Messages.Status_Code;
      Con_Fail         : exception;
      Mime_Fail        : exception;
      url              : Unbounded_String;
      str              : Unbounded_String;
      resultlist       : JSON_Value;

   begin
      Parse_Config_File;   --  parses default ./.gnatdebug
      --  Create the search url where we can reach the rpc
      url  := "https://aur.archlinux.org/rpc.php?type=search&arg="
               & To_Unbounded_String (query);
      Page := Client.Get (To_String (url));
      S    := AWS.Response.Status_Code (Page);

      --  First check for errors
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

      --  Convert to a json object
      resultlist := Read (Strm     => To_String (str),
                          Filename => "");

      --  Iterate the Test_JSON object.
      --  The Handler procedure is responsible for
      --  outputting the contents.
      Ada.Text_IO.Put_Line ("--> Test_JSON Start <--");
      Map_JSON_Object (Val   => resultlist,
                       CB    => JSONHandler'Access);
      Ada.Text_IO.Put_Line ("--> Test_JSON End <--");

      --  Log
      Trace (Stream1, To_String (url), "Called url ");
      Trace (Stream1, Integer'Image (Length (str)), "Rec str of len");

      --  Let's not forget to return our json object
      return resultlist;

      --  Handle the exceptions, basically just propagate them
      exception
         --   when Fail : Con_Fail | Mime_Fail =>
            --   Trace (Stream1, Exception_Message (Fail));
            --   raise;
         when Fail : others =>
            Trace (Stream1, Exception_Message (Fail));
            raise;
   end Get_JSON;
end rpcclient;
