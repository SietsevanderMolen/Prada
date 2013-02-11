with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AurPackages;
with GNAT.String_Split;

package Install is
   procedure Install
      (Query : Unbounded_String);
   --  Shows the user a list of packages and asks which one to install
   --
   procedure InstallPackage
      (Pkg : AurPackages.AurPackage);
   --  Installs a single package
private
   function SplitInput
      (input : Unbounded_String)
      return GNAT.String_Split.Slice_Set;
   --  Split a string by spaces

   function RequestTempFolder (Pkg : AurPackages.AurPackage) return String;
   --  Create a temporary folder for this download/installation

   function RequestBuildFolder (Pkg : AurPackages.AurPackage) return String;
   --  Create a temporary folder for this build/installation

   procedure PurgeTempFolder (Pkg : AurPackages.AurPackage);
   --  Purges the temporary folder for this download/installation

   procedure PurgeBuildFolder (Pkg : AurPackages.AurPackage);
   --  Purges the temporary folder for this build/installation

   procedure DownloadPKG (Pkg : AurPackages.AurPackage);

   procedure UnzipPKG (Pkg : AurPackages.AurPackage);

   procedure BuildPKG (Pkg : AurPackages.AurPackage);
end Install;
