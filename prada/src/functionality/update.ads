with GNAT.String_Split;
with AurReplies;
with AurInterface; use AurInterface;

package Update is
   procedure Update;
   --  Updates all aur packages on this system
private
   function FilterUpdatable
      (InstalledPackages : AurInterface.PackageMap.Map;
       Reply : AurReplies.AurReply)
   return AurInterface.PackageMap.Map;
   --  Checks for updates and returns a list of updatable packages

   function GetInstalledPackages
      return AurInterface.PackageMap.Map;
   --  Gets all the installed packages on the system

   function SplitInputByNewline
      (Input : String)
      return GNAT.String_Split.Slice_Set;
   --  Split a string by newlines

   function CanBeUpdated
      (LocalVersion  : String;
      RemoteVersion : String)
      return Integer;
      --  Return true if this package is newer on the aur
end Update;
