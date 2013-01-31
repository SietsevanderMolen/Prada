with Ada.Strings.Unbounded;
with AurPackages;
with GNAT.String_Split;

package Install is
   procedure Install
      (Query : Ada.Strings.Unbounded.Unbounded_String);
   --  Shows the user a list of packages and asks which one to install
private
   procedure InstallPackage
      (Pkg : AurPackages.AurPackage);
   --  Installs a single package

   function SplitInput
      (input : Ada.Strings.Unbounded.Unbounded_String)
      return GNAT.String_Split.Slice_Set;
   --  Split a string by spaces
end Install;
