with Ada.Text_IO;
with AurReplies;
with Search;
with Interfaces.C; use Interfaces.C;
with Sys;
with AurInterface;

package body Install is

   procedure BuildPKG
      (Pkg : AurPackages.AurPackage)
   is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
      Ret_Val : Integer;
      pragma Unreferenced (Ret_Val);
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
   begin
      --  Remove the directory
      Sys.Remove_Directory (RequestBuildFolder (Pkg));
      --  Make the actual directory again
      Sys.Create_Directory_Recursive (RequestBuildFolder (Pkg));
   end PurgeBuildFolder;

   procedure PurgeTempFolder (Pkg : AurPackages.AurPackage)
   is
   begin
      --  Remove the directory
      Sys.Remove_Directory (RequestTempFolder (Pkg));
      --  Make the actual directory again
      Sys.Create_Directory_Recursive (RequestTempFolder (Pkg));
   end PurgeTempFolder;

   function RequestBuildFolder (Pkg : AurPackages.AurPackage) return String
   is
      TmpDir  : Unbounded_String := To_Unbounded_String (Sys.Get_Temp_Dir);
   begin
      --  Use the systems TmpDir and uid to create our specific temp folder
      TmpDir := TmpDir & "/pradabld-" & Sys.FindUID
               & "/" & To_String (Pkg.GetName);
      return To_String (TmpDir);
   end RequestBuildFolder;

   function RequestTempFolder (Pkg : AurPackages.AurPackage) return String
   is
      TmpDir  : Unbounded_String := To_Unbounded_String (Sys.Get_Temp_Dir);
   begin

      --  Use the systems TmpDir and uid to create our specific temp folder
      TmpDir := TmpDir & "/pradatmp-" & Sys.FindUID
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
      pragma Unreferenced (Ret_Val);
   begin
      Ret_Val := Sys (To_C (To_String ("tar xf " & RequestTempFolder (Pkg)
      & "/" & Pkg.GetName & ".tar.gz -C " & RequestBuildFolder (Pkg)
      & " --strip-components=1")));
   end UnzipPKG;
end Install;
