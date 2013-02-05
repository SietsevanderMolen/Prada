with GNAT.Expect;
with Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Characters;
with Ada.Characters.Latin_1;

package body Update is
   function CanBeUpdated
      (PkgName : String;
       Version : String)
      return Boolean
   is
   begin
      return True;
   end CanBeUpdated;

   function GetInstalledPackages
      return String
   is
      Fd      : GNAT.Expect.Process_Descriptor;
      Timeout : constant Integer := 60000; --  1 sec
      Result  : GNAT.Expect.Expect_Match;
   begin
      GNAT.Expect.Non_Blocking_Spawn
         (Fd, "/usr/bin/pacman", (1 => new String'("-Qm")));
      GNAT.Expect.Expect (Fd, Result, "(.|\n)*", Timeout);
      return GNAT.Expect.Expect_Out (Fd);
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
      Subs        : GNAT.String_Split.Slice_Set;
      PkgList     : String := GetInstalledPackages;
   begin
      Ada.Text_IO.Put_Line ("Checking for installed packages");
      --  Subtract 1 here, because PkgList contains a newline at the end
      Subs := SplitInputByNewline (PkgList (PkgList'First .. PkgList'Last - 1));
      Ada.Text_IO.Put_Line ("Checking for updates");

      for I in 1 .. GNAT.String_Split.Slice_Count (Subs) loop
         --  Loop though the substrings
         declare
            --  Pull the next substring out into an unbounded string for easy use
            Sub : Unbounded_String := To_Unbounded_String
                  (GNAT.String_Split.Slice (Subs, I));
            --  Find the name
            Name : String := Slice (Sub, 1, Index (Sub, " "));
            --  Find the version
            Version : String := Slice (Sub, Index (Sub, " ")+1, Length (Sub));
         begin
            if CanBeUpdated (Name, Version) then
               Ada.Text_IO.Put_Line ("Name " & Name);
               Ada.Text_IO.Put_Line ("Version " & Version);
            end if;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Done updating");
   end Update;
end Update;
