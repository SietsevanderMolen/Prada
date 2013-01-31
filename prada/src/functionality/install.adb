with Ada.Text_IO;
with AurReplies;
with Search;
with GNAT.Expect; use GNAT.Expect;
with Ada.Environment_Variables;
with Interfaces.C; use Interfaces.C;
with AurInterface;

package body Install is

   procedure BuildPKG
      (Pkg : AurPackages.AurPackage)
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
      Pragma Unreferenced (Ret_Val);
   begin
      Ret_Val := Sys (To_C (
         "cd " & RequestBuildFolder (Pkg) & " && makepkg -i"));
   end BuildPKG;

   procedure DownloadPKG (Pkg : AurPackages.AurPackage)
   is
      url : Unbounded_String;
      dst : Unbounded_String;
   begin
      url := "curl -Lfs " & AurInterface.GetAurURL & Pkg.GetURLPath;
      dst := RequestTempFolder (Pkg) & "/" & Pkg.GetName & ".tar.gz";
      AurInterface.DownloadFile (url, dst);
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
      Ada.Text_IO.New_Line;

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
      PurgeTempFolder (Pkg);
      PurgeBuildFolder (Pkg);
      DownloadPKG (Pkg);
      UnzipPKG (Pkg);
      BuildPKG (Pkg);
   end InstallPackage;

   procedure PurgeBuildFolder (Pkg : AurPackages.AurPackage)
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
      TmpDir  : Unbounded_String;
      pragma Unreferenced (Ret_Val);
   begin
      --  Delete dir to purge it's contents; discard error if it doesn't exist
      Ret_Val := Sys (To_C ("rm -rf "
         & RequestBuildFolder (Pkg) & " >/dev/null"));
      --  Make the actual directory again
      Ret_Val := Sys (To_C ("mkdir -p "
         & RequestBuildFolder (Pkg) & " >/dev/null"));
   end PurgeBuildFolder;

   procedure PurgeTempFolder (Pkg : AurPackages.AurPackage)
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
      TmpDir  : Unbounded_String;
      pragma Unreferenced (Ret_Val);
   begin
      --  Delete dir to purge it's contents; discard error if it doesn't exist
      Ret_Val := Sys (To_C ("rm -rf "
         & RequestTempFolder (Pkg) & " >/dev/null"));
      --  Make the actual directory again
      Ret_Val := Sys (To_C ("mkdir -p "
         & RequestTempFolder (Pkg) & " >/dev/null"));
   end PurgeTempFolder;

   function RequestBuildFolder (Pkg : AurPackages.AurPackage) return String
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
      TmpDir := TmpDir & "/pradabld-" & FindUID
               & "/" & To_String (Pkg.GetName);
      return To_String (TmpDir);
   end RequestBuildFolder;

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
      return To_String (TmpDir);
   end RequestTempFolder;

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

   procedure UnzipPKG
      (Pkg : AurPackages.AurPackage)
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
      Pragma Unreferenced (Ret_Val);
   begin
      Ret_Val := Sys (To_C (To_String ("tar xf " & RequestTempFolder (Pkg)
      & "/" & Pkg.GetName & ".tar.gz -C " & RequestBuildFolder (Pkg)
      & " --strip-components=1")));
   end UnzipPKG;
end Install;
