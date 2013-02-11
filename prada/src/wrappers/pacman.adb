with GNAT.Expect;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Pacman is
   function GetInstalledAurPackages
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
   end GetInstalledAurPackages;

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
end Pacman;
