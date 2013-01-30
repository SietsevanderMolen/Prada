with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with AurPackages; use AurPackages;

package AurReplies is
   type AurReply is tagged private;
   package AurPackageContainer is new Vectors (Natural, AurPackage);
   use AurPackageContainer;

   function Create (
      replyType   : Unbounded_String;
      resultCount : Natural;
      results : Vector
   ) return AurReply;

   function getType (reply : AurReply) return Unbounded_String;
   --  Get the return type of this reply
   function getResultCount (reply : AurReply) return Natural;
   --  Get the amount of results from this reply
   function getResults (reply : AurReply) return Vector;
   --  Get the vector with results from this reply

   procedure PrettyPrintResults (Reply : AurReply);
   --  Pretty print all the results in this reply
   --
   procedure QuickPrintResults (Reply : AurReply);
   --  Quick print all the results in this reply eg. just the title

private
   type AurReply is tagged
      record
         replyType   : Unbounded_String;
         resultCount : Natural;
         results     : Vector;
      end record;

   procedure PrettyPrintResult (Position : Cursor);
   --  Pretty prints a single result at the given position
   --
   procedure QuickPrintResult (Position : Cursor);
   --  Quick prints a single result at the given position eg. just the title
end AurReplies;
