with Ada.Text_IO;
with AurReplies;
with Search;
with GNAT.Expect; use GNAT.Expect;
with Interfaces.C; use Interfaces.C;
with Ada.Environment_Variables;

package body Install is
   function RequestTempFolder (Pkg : AurPackages.AurPackage) return String
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
      TmpDir  : Unbounded_String;
      pragma Unreferenced (Ret_Val);
   begin
      --  Set the temp dir as /tmp if it's not specd in $TMPDIR
      if Ada.Environment_Variables.Exists (Name => "TMPDIR") then
         TmpDir := To_Unbounded_String
            (Ada.Environment_Variables.Value (Name => "TMPDIR"));
      else
         TmpDir := To_Unbounded_String ("/tmp");
      end if;

      --  Use the systems TmpDir and uid to create our specific temp folder
      TmpDir := TmpDir & "/pradatmp-" & FindUID
               & "/" & To_String (Pkg.GetName);

      --  Delete dir to purge it's contents; discard error if it doesn't exist
      Ret_Val := Sys (To_C ("rm -rf " & To_String (TmpDir) & " >/dev/null"));
      --  Make the actual directory again
      Ret_Val := Sys (To_C ("mkdir -p " & To_String (TmpDir) & " >/dev/null"));
      return To_String (TmpDir);
   end RequestTempFolder;

   procedure DownloadPKG (Pkg : AurPackages.AurPackage)
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
   begin
      Ret_Val := Sys (To_C ( To_String (
         ("wget https://aur.archlinux.org/" & Pkg.GetURLPath
        & " -c -O " & RequestTempFolder (Pkg) & "/" & Pkg.GetName & ".tar.gz"))));
   end DownloadPKG;

   function FindUID return String
   is
      Fd      : Process_Descriptor;
      Timeout : constant Integer := 1000; -- 1 sec
      Result  : Expect_Match;
   begin
      GNAT.Expect.Non_Blocking_Spawn (Fd, "/usr/bin/id", (1 => new String'
         ("-u")));
      GNAT.Expect.Expect (Fd, Result, ".*", Timeout);
      return GNAT.Expect.Expect_Out (Fd);
   end FindUID;

   procedure Install
      (Query : Unbounded_String)
   is
      results : AurReplies.AurReply;
      Subs    : GNAT.String_Split.Slice_Set;
   begin
      results := Search.Search (Query);

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
         ("Type number to install. Separate each number with a space.");
      Ada.Text_IO.Put ("Numbers: ");

      --  Split the string by spaces so we keep the numbers
      Subs := SplitInput (To_Unbounded_String
         (Ada.Text_IO.Get_Line));

      for I in 1 .. GNAT.String_Split.Slice_Count (Subs) loop
         --  Loop though the substrings
         declare
            Sub : constant String := GNAT.String_Split.Slice (Subs, I);
            --  Pull the next substring out into a string for easy handling.
         begin
            for i in results.getResults.First_Index ..
               results.getResults.Last_Index loop
               --  If this is a number we want to install, go for it
               if AurPackages.GetRefNum (results.getResults.Element (i))
                  = Integer'Value (Sub) then
                  InstallPackage (results.getResults.Element (i));
               end if;
            end loop;
         end;
      end loop;
   end Install;

   procedure InstallPackage
      (Pkg        : AurPackages.AurPackage)
   is
   begin
      DownloadPKG (Pkg);
   end InstallPackage;

   function SplitInput
      (input : Unbounded_String)
      return GNAT.String_Split.Slice_Set
   is
      Subs : GNAT.String_Split.Slice_Set;
      Data : constant String := To_String (input);
   begin
      GNAT.String_Split.Create
         (S          => Subs,
         From       => Data,
         Separators => " ",
         Mode       => GNAT.String_Split.Multiple);
      return Subs;
   end SplitInput;

end Install;
