with AurInterface;
with GNAT.String_Split;

package Pacman is
   function GetInstalledAurPackages
      return AurInterface.PackageMap.Map;
   --  Gets all the installed aur packages on the system

   function SplitInputByNewline
      (Input : String)
      return GNAT.String_Split.Slice_Set;
   --  Split a string by newlines
end Pacman;
