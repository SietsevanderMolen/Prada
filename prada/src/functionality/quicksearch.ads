with AurReplies;
with AurPackages;
with Ada.Strings.Unbounded;

package QuickSearch is
   procedure Search
      (Query : Ada.Strings.Unbounded.Unbounded_String);
private
   procedure QuickPrint (Pkg : AurPackages.AurPackage);
   --  Quick prints a single result at the given position eg. just the title
   --
   procedure QuickPrintResults (Reply : AurReplies.AurReply);
   --  Quick print all the results in this reply eg. just the title
end QuickSearch;
