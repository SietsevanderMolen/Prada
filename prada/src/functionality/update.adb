with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Expect;
with AurPackages;
with Install;
with Util;

package body Update is
   function CanBeUpdated
      (LocalVersion : String;
       RemoteVersion : String)
      return Integer
   is
      Fd           : GNAT.Expect.Process_Descriptor;
      Timeout      : constant Integer := 1000; --  1 sec
      Output       : GNAT.Expect.Expect_Match;
      Result       : Unbounded_String;
      VercmpResult : Integer;
   begin
      GNAT.Expect.Non_Blocking_Spawn
          (Fd, "/usr/bin/vercmp",
            (1 => new String'(RemoteVersion),
             2 => new String'(LocalVersion)));
      GNAT.Expect.Expect (Fd, Output, "(.)*", Timeout);
      Result := To_Unbounded_String (GNAT.Expect.Expect_Out (Fd));
      VercmpResult := Integer'Value (To_String (Result));
      return VercmpResult;
   exception
      --  Re-raise but with a bit more helpful message
      when GNAT.Expect.Invalid_Process =>
         raise GNAT.Expect.Invalid_Process
            with "Can not find 'vercmp' executable, "
               & "is it installed in /usr/bin/vercmp?";
      when Constraint_Error =>
         raise Constraint_Error
            with """/usr/bin/vercmp " & String'(RemoteVersion) & " "
               & String'(LocalVersion) & """ returned """ & To_String (Result)
               & """, expected an int > " & Integer'Image (Integer'First)
               & " & < " & Integer'Image (Integer'Last);
   end CanBeUpdated;

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
               := CanBeUpdated (To_String (LocalV), To_String (RemoteV));
         begin
            if Updatable > 0 then
               --  BAM aur version is newer
               UpdatablePackages.Insert
                  (Pkg.GetName, Pkg.GetVersion);
            elsif Updatable = 0 then
               --  Package in aur is like local
               null;
            elsif Updatable < 0 then
               --  Package in aur is older than local
               null;
            end if;
         end;
      end loop;
      return UpdatablePackages;
   end FilterUpdatable;

   function GetInstalledPackages
      return AurInterface.PackageMap.Map
   is
      Fd           : GNAT.Expect.Process_Descriptor;
      Timeout      : constant Integer := 60000; --  1 sec
      Result       : GNAT.Expect.Expect_Match;
      Subs         : GNAT.String_Split.Slice_Set;
      PacmanOutput : Unbounded_String;
      PkgList      : AurInterface.PackageMap.Map;
   begin
      GNAT.Expect.Non_Blocking_Spawn
         (Fd, "/usr/bin/pacman", (1 => new String'("-Qm")));
      GNAT.Expect.Expect (Fd, Result, "(.|\n)*", Timeout);

      PacmanOutput := To_Unbounded_String (GNAT.Expect.Expect_Out (Fd));

      --  Subtract 1 here, because PacmanOutput contains a newline at the end
      Subs := SplitInputByNewline (Slice (PacmanOutput, 1,
         Ada.Strings.Unbounded.Length (PacmanOutput) - 1));

      for I in 1 .. GNAT.String_Split.Slice_Count (Subs) loop
         --  Loop though the substrings
         declare
            --  Pull the next substring into an unbounded string for easy use
            Sub : constant Unbounded_String
               := To_Unbounded_String (GNAT.String_Split.Slice (Subs, I));
            --  Find the name
            Name : constant Unbounded_String
               := To_Unbounded_String (Slice (Sub, 1, Index (Sub, " ") - 1));
            --  Find the version
            Version : constant Unbounded_String
               := To_Unbounded_String (Slice
                  (Sub, Index (Sub, " ") + 1, Length (Sub)));
         begin
            PkgList.Insert
                (Name, Version);
         end;
      end loop;

      return PkgList;

   exception
      --  Re-raise but with a bit more helpful message
      when GNAT.Expect.Invalid_Process =>
         raise GNAT.Expect.Invalid_Process
            with "Can not find 'pacman' executable, "
               & "is it installed in /usr/bin/pacman?";
   end GetInstalledPackages;

   function SplitInputByNewline (Input : String)
      return GNAT.String_Split.Slice_Set
   is
      Subs     : GNAT.String_Split.Slice_Set;
      Splitter : constant String := "" & ASCII.LF;
   begin
      GNAT.String_Split.Create
         (S          => Subs,
         From       => Input,
         Separators => Splitter,
         Mode       => GNAT.String_Split.Multiple);
      return Subs;
   end SplitInputByNewline;

   procedure Update is
      Reply       : AurReplies.AurReply;
      InstalledPackages : AurInterface.PackageMap.Map;
      UpdatablePackages : AurInterface.PackageMap.Map;
   begin
      Ada.Text_IO.Put_Line (":: Checking for installed packages");
      InstalledPackages := GetInstalledPackages;

      Ada.Text_IO.Put_Line (":: Checking for updates");
      Reply := AurInterface.multiinfo (InstalledPackages);
      UpdatablePackages := FilterUpdatable (InstalledPackages, Reply);

      Reply := AurInterface.multiinfo (UpdatablePackages);

      Ada.Text_IO.Put ("Aur Targets    (" & Reply.getResults.length'img & "): ");
      for i in Reply.getResults.First_Index ..
         Reply.getResults.Last_Index loop
            Ada.Text_IO.Put (To_String (Reply.getResults.Element (i).GetName) & " ");
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
