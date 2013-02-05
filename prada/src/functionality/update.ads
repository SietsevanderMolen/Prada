with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AurPackages;
with GNAT.String_Split;

package Update is
   procedure Update;
   --  Updates all aur packages on this system
private
   function GetInstalledPackages
      return String;
   --  Gets all the installed packages on the system

   function SplitInputByNewline
      (Input : String)
      return GNAT.String_Split.Slice_Set;
   --  Split a string by newlines

   function CanBeUpdated
      (PkgName : String;
       Version : String)
      return Boolean;
   --  Return true if this package is newer on the aur
end Update;
