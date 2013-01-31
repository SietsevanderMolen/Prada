with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;
with AurInterface;
with AurReplies; use AurReplies;

package body Search is
   procedure PrettyPrint (Pkg : AurPackages.AurPackage) is
   begin
      --  Print reference number. Width=2 because no more than 99 results
      --  are usually shown anyway
      Ada.Integer_Text_IO.Put (Pkg.GetRefNum, Width => 2);
      Ada.Text_IO.Put (
      --  A space to separate the refnum and name
         " " &
      --  Change to blue colour
         Ada.Characters.Latin_1.ESC & "[34;1m" &
      --  Print aur identifier
         "aur/" &
      --  Disable blue color
         Ada.Characters.Latin_1.ESC & "[0;1m" &
      --  Print name
         Ada.Strings.Unbounded.To_String (Pkg.GetName) &
      --  A space to separate the name and version
         " " &
      --  Change to gray colour
         Ada.Characters.Latin_1.ESC & "[30;1m" &
      --  Print version
         Ada.Strings.Unbounded.To_String (Pkg.GetVersion) &
      --  Reset colour
         Ada.Characters.Latin_1.ESC & "[0m"
      );
      --  Only print if package is out of date
      if Pkg.GetOutofdate > 1 then
         Ada.Text_IO.Put (
            " " &
            Ada.Characters.Latin_1.ESC & "[31;1m" &
            "Out of date" &
            Ada.Characters.Latin_1.ESC & "[0m"
            );
      end if;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (
         --  Spaces to indent descriptions
         "    " &
         --  Description
         Ada.Strings.Unbounded.To_String (Pkg.GetDescription)
      );
      Ada.Text_IO.New_Line;
   end PrettyPrint;

   procedure PrettyPrintResults (
      Reply : AurReplies.AurReply
   ) is
   begin
      for i in Reply.getResults.First_Index .. Reply.getResults.Last_Index loop
            PrettyPrint (Reply.getResults.Element (Integer (i)));
      end loop;
   end PrettyPrintResults;

   procedure Search
      (Query : in Ada.Strings.Unbounded.Unbounded_String)
   is
      results : AurReplies.AurReply;
   begin
      results := AurInterface.searchaur (Query);
      PrettyPrintResults (results);
   end Search;

   function Search
      (Query : in Ada.Strings.Unbounded.Unbounded_String) return AurReply
   is
      results : AurReplies.AurReply;
   begin
      results := AurInterface.searchaur (Query);
      PrettyPrintResults (results);
      return results;
   end Search;
end Search;
