with AurReplies;
with AurPackages;
with Ada.Strings.Unbounded;

package Search is
   procedure Search
      (Query : Ada.Strings.Unbounded.Unbounded_String);
   --  Searches for the given query and presents the results

   function Search
      (Query : Ada.Strings.Unbounded.Unbounded_String)
         return AurReplies.AurReply;
private
   procedure PrettyPrint (Pkg : AurPackages.AurPackage);
   --  Pretty prints a single result

   procedure PrettyPrintResults (Reply : AurReplies.AurReply);
   --  Pretty print all the results in the given reply
end Search;
