with GNAT.Expect;
with AurPackages;
with Ada.Text_IO;

package body Update is
   function CanBeUpdated
      (LocalVersion : String;
       RemoteVersion : String)
      return Integer
   is
      Fd       : GNAT.Expect.Process_Descriptor;
      Timeout  : constant Integer := 1000; --  1 sec
      Output   : GNAT.Expect.Expect_Match;
      Result   : Unbounded_String;
   begin
      GNAT.Expect.Non_Blocking_Spawn
         (Fd, "/usr/bin/vercmp",
            (1 => new String'(RemoteVersion),
             2 => new String'(LocalVersion)));
      GNAT.Expect.Expect (Fd, Output, "(.)*", Timeout);
      Result := To_Unbounded_String (GNAT.Expect.Expect_Out (Fd));
      return Integer'Value (To_String (Result));
   end CanBeUpdated;

   procedure FilterUpdatable
       (InstalledPackages : AurInterface.PackageMap.Map;
        Reply : AurReplies.AurReply)
   is
   begin
      for i in Reply.getResults.First_Index .. Reply.getResults.Last_Index loop
         declare
            Pkg : constant AurPackages.AurPackage
               := Reply.getResults.Element (Integer (i));
            RemoteV : constant Unbounded_String
               := Pkg.GetVersion;
            LocalV : constant Unbounded_String
               := InstalledPackages.Element (Pkg.GetName);
            Updatable : constant Integer
               := CanBeUpdated (To_String (LocalV), To_String (RemoteV));
         begin
            case Updatable is
               when 1 => --  Update available :D
                  Ada.Text_IO.Put (To_String (Pkg.GetName));
                  Ada.Text_IO.Put (" " & To_String (LocalV));
                  Ada.Text_IO.Put_Line (" < " & To_String (RemoteV));
               when 0 => --  Aur version is equal to the installed one
                  null;
               when -1 => --  Aur version is older
                  null;
               when others =>
                  null;
            end case;
         end;
      end loop;
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
            Sub : constant Unbounded_String := To_Unbounded_String
                  (GNAT.String_Split.Slice (Subs, I));
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
   begin
      Ada.Text_IO.Put_Line ("Checking for installed packages");
      InstalledPackages := GetInstalledPackages;

      Ada.Text_IO.Put_Line ("Checking for updates");
      Reply := AurInterface.multiinfo (InstalledPackages);

      FilterUpdatable (InstalledPackages, Reply);

      Ada.Text_IO.Put_Line ("Done updating");
   end Update;
end Update;
