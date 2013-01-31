with AurReplyFactory; use AurReplyFactory;
with AWS.Client, AWS.Response, AWS.Messages;
use  AWS, AWS.Messages;
with Ada; use Ada;

package body AurInterface is
   function info
      (query : in Unbounded_String) return AurReply
   is
      json : Unbounded_String;
   begin
      json := PerformAurQuery ("info", query);
      return createAurReply (To_String (json));
   end info;

   function msearch
      (query : in Unbounded_String) return AurReply
   is
      json : Unbounded_String;
   begin
      json := PerformAurQuery ("msearch", query);
      return createAurReply (To_String (json));
   end msearch;

   function multiinfo
      (query : in Unbounded_String) return AurReply
   is
      json : Unbounded_String;
   begin
      json := PerformAurQuery ("multiinfo", query);
      return createAurReply (To_String (json));
   end multiinfo;

   function PerformAurQuery
      (qtype : in String;
       arg   : in Unbounded_String)
      return Unbounded_String
   is
      Page      : AWS.Response.Data;
      S         : Messages.Status_Code;
      Con_Fail  : exception;
      Mime_Fail : exception;
      url       : Unbounded_String;
      str       : Unbounded_String;
   begin
      --  Create the search url where we can reach the rpc
      url  := "https://aur.archlinux.org/rpc.php?type=" &
      To_Unbounded_String (qtype) & "&arg=" & arg;
      Page := Client.Get (To_String (url));
      S    := AWS.Response.Status_Code (Page);

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
      return str;
   end PerformAurQuery;

   function searchaur
      (query : in Unbounded_String) return AurReply
   is
      json    : Unbounded_String;
   begin
      json := PerformAurQuery ("search", query);
      return createAurReply (To_String (json));
   end searchaur;
end AurInterface;
