with AurReplyFactory; use AurReplyFactory;
with GNAT.Expect;
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
      (query : in Unbounded_String)
      return AurReply
   is
      json : constant String := PerformAurQuery ("info", "&arg=" & query);
   begin
      return createAurReply (json);
   end info;

   function msearch
      (query : in Unbounded_String)
      return AurReply
   is
         json : constant String :=
            PerformAurQuery ("msearch", "&arg=" & query);
   begin
      return createAurReply (json);
   end msearch;

   function multiinfo
      (packages : in PackageMap.Map)
         return AurReply
   is
      use PackageMap;
      curs : Cursor := First (packages);
      AurQuery  : Unbounded_String;
   begin
      while Has_Element (curs) loop
         AurQuery := AurQuery & "&arg[]="
            & Key (curs);
         Next (curs);
      end loop;

      declare
         json : constant String := PerformAurQuery ("multiinfo", AurQuery);
      begin
         return createAurReply (json);
      end;
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
      return String
   is
      Status : aliased Integer;
      Url    :  constant String :=
         AurURL & "rpc.php?type=" & qtype & To_String (arg);
      Output : constant String :=
         GNAT.Expect.Get_Command_Output
            ("/usr/bin/curl",
               (1 => new String'("-LfGs"),
                2 => new String'(Url)
               ),
            "", Status'Access);
   begin
      return Output;

      --  --  First check for connection/general errors
      --  if S not in AWS.Messages.Success then
         --  raise Con_Fail with
            --  "Unable to retrieve data => Status Code: "
            --  & Image (S) & " Reason: " & Reason_Phrase (S);
      --  --  I wonder if this is seriously necessary, but I'll add it anyway
      --  end if;
   end PerformAurQuery;

   function searchaur
      (query : in Unbounded_String) return AurReply
   is
      json : constant String := PerformAurQuery ("search", "&arg=" & query);
   begin
      return createAurReply (json);
   end searchaur;
end AurInterface;
