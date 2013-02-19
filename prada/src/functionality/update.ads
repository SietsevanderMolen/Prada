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
   --  Checks for updates and returns a list of packages with updates
end Update;
