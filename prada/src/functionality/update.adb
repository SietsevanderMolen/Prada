with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Pacman;
with Vercmp;
with AurPackages;
with Install;
with Util;

package body Update is
   function FilterUpdatable
       (InstalledPackages : AurInterface.PackageMap.Map;
        Reply : AurReplies.AurReply)
   return AurInterface.PackageMap.Map
   is
      UpdatablePackages : AurInterface.PackageMap.Map;
   begin
      for i in Reply.getResults.First_Index .. Reply.getResults.Last_Index loop
         declare
            Pkg : constant AurPackages.AurPackage
               := Reply.getResults.Element (Integer (i));
            RemoteV : constant Unbounded_String
               := Pkg.GetVersion;
            LocalV : constant Unbounded_String
               := InstalledPackages.Element (Pkg.GetName);
            Updatable : constant Integer range -1 .. 1
               := Vercmp.Run (To_String (LocalV), To_String (RemoteV));
         begin
            if Updatable > 0 then
               --  BAM aur version is newer
               UpdatablePackages.Insert
                  (Pkg.GetName, Pkg.GetVersion);
            elsif Updatable = 0 then
               --  Package in repository is like local
               null;
            elsif Updatable < 0 then
               --  Package in repository is older than local
               null;
            end if;
         end;
      end loop;
      return UpdatablePackages;
   end FilterUpdatable;

   procedure Update is
      Reply       : AurReplies.AurReply;
      InstalledPackages : AurInterface.PackageMap.Map;
      UpdatablePackages : AurInterface.PackageMap.Map;
   begin
      Ada.Text_IO.Put_Line (":: Checking for installed packages");
      InstalledPackages := Pacman.GetInstalledAurPackages;

      Ada.Text_IO.Put_Line (":: Checking for updates");
      Reply := AurInterface.multiinfo (InstalledPackages);
      UpdatablePackages := FilterUpdatable (InstalledPackages, Reply);

      Reply := AurInterface.multiinfo (UpdatablePackages);

      Ada.Text_IO.Put
         ("Aur Targets    (" & Reply.getResults.Length'Img & "): ");
      for i in Reply.getResults.First_Index ..
         Reply.getResults.Last_Index loop
            Ada.Text_IO.Put
               (To_String (Reply.getResults.Element (i).GetName) & " ");
      end loop;
      Ada.Text_IO.New_Line;

      if Util.UserConfirm (To_Unbounded_String
         ("Proceed with installation? [Y/n] ")) then
         --  Iterate over packages and call install method
         for i in Reply.getResults.First_Index ..
            Reply.getResults.Last_Index loop
            Install.InstallPackage (Reply.getResults.Element (i));
         end loop;

         Ada.Text_IO.Put_Line (":: Done updating");
      end if;
   end Update;
end Update;
