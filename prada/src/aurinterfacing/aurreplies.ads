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

private
   type AurReply is tagged
      record
         replyType   : Unbounded_String;
         resultCount : Natural;
         results     : Vector;
      end record;
end AurReplies;
