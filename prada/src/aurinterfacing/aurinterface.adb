with AurReplyFactory; use AurReplyFactory;
with AWS.Client, AWS.Response, AWS.Messages;
use  AWS, AWS.Messages;
with Ada; use Ada;
with Interfaces.C; use Interfaces.C;
with Ada.Strings.Unbounded.Hash;

package body AurInterface is
   AurURL : constant String := "https://aur.archlinux.org/";

   procedure DownloadFile
      (Url  : Unbounded_String;
      Dest : Unbounded_String
      )
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
      pragma Unreferenced (Ret_Val);
   begin
      Ret_Val := Sys (To_C (To_String (Url & " > " & Dest)));
   end DownloadFile;

   function GetAurURL return String
   is
   begin
      return AurURL;
   end GetAurURL;

   function info
      (query : in Unbounded_String) return AurReply
   is
      json      : Unbounded_String;
   begin
      json := PerformAurQuery ("info", "&arg=" & query);
      return createAurReply (To_String (json));
   end info;

   function msearch
      (query : in Unbounded_String) return AurReply
   is
      json : Unbounded_String;
   begin
      json := PerformAurQuery ("msearch", "&arg=" & query);
      return createAurReply (To_String (json));
   end msearch;

   function multiinfo
      (packages : in PackageMap.Map)
         return AurReply
   is
      use PackageMap;
      curs : Cursor := First (packages);

      json : Unbounded_String;
      AurQuery  : Unbounded_String;
   begin
      while Has_Element (curs) loop
         AurQuery := AurQuery & "&arg[]="
            & Key (curs);
         Next (curs);
      end loop;

      json := PerformAurQuery ("multiinfo", AurQuery);
      return createAurReply (To_String (json));
   end multiinfo;

   function Name_Hashed (id : Unbounded_String)
   return Hash_Type
   is
   begin
      return Ada.Strings.Unbounded.Hash (id);
   end Name_Hashed;

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
      url  := AurURL & "rpc.php?type=" &
      To_Unbounded_String (qtype) & arg;
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
      json := PerformAurQuery ("search", "&arg=" & query);
      return createAurReply (To_String (json));
   end searchaur;
end AurInterface;
