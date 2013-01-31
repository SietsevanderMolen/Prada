with Ada.Text_IO;
with AurInterface;
with AurReplies; use AurReplies;

package body QuickSearch is
   procedure QuickPrint (Pkg : AurPackages.AurPackage) is
   begin
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Pkg.GetName));
   end QuickPrint;

   procedure QuickPrintResults (
      Reply : AurReplies.AurReply
   ) is
   begin
      for i in Reply.getResults.First_Index .. Reply.getResults.Last_Index loop
            QuickPrint (Reply.getResults.Element (Integer (i)));
      end loop;
   end QuickPrintResults;

   procedure Search
      (Query : in Ada.Strings.Unbounded.Unbounded_String)
   is
      results : AurReplies.AurReply;
   begin
      results := AurInterface.searchaur (Query);
      QuickPrintResults (results);
   end Search;
end QuickSearch;
