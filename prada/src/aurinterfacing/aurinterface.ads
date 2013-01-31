with AurReplies; use AurReplies;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AurInterface is
   package QueryContainer is new Vectors
      (Natural, Ada.Strings.Unbounded.Unbounded_String);
   use QueryContainer;

   function GetAurURL return String;

   function msearch
      (query : in Unbounded_String) return AurReply;
   function info
      (query : in Unbounded_String) return AurReply;
   function multiinfo
      (query : in Unbounded_String) return AurReply;

   function searchaur
    (query : in Unbounded_String) return AurReply;

   procedure DownloadFile
   (Url  : in Unbounded_String;
    Dest : in Unbounded_String
   );
private
   function PerformAurQuery
       (qtype : in String;
        arg : in Unbounded_String)
       return Unbounded_String;
end AurInterface;
