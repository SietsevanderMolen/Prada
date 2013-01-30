with AurReplies; use AurReplies;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AurInterface is
   function search
      (query : in Unbounded_String) return AurReply;
   function msearch
      (query : in Unbounded_String) return AurReply;
   function info
      (query : in Unbounded_String) return AurReply;
   function multiinfo
      (query : in Unbounded_String) return AurReply;
private
   function PerformAurQuery
       (qtype : in String;
        arg : in Unbounded_String)
       return Unbounded_String;
end AurInterface;
