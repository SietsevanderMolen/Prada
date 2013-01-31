with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AurPackages;
with GNAT.String_Split;

package Install is
   procedure Install
      (Query : Unbounded_String);
   --  Shows the user a list of packages and asks which one to install
private
   procedure InstallPackage
      (Pkg        : AurPackages.AurPackage);
   --  Installs a single package

   function SplitInput
      (input : Unbounded_String)
      return GNAT.String_Split.Slice_Set;
   --  Split a string by spaces

   function CreateTempFolder (Name : String) return Integer;
   --  Create a temporary folder for this build/installation

   function FindUID return String;
   --  Finds the user id for the user parenting this process
end Install;
