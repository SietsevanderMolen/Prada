with AurReplies; use AurReplies;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AurInterface is
   function Name_Hashed (id : Unbounded_String) return Hash_Type;
   --  you need to provide this to every hashed container

   package PackageMap is new Ada.Containers.Hashed_Maps
      (Key_Type => Unbounded_String,
      Element_Type => Unbounded_String,
      Hash => Name_Hashed,
      Equivalent_Keys => "=");

   function GetAurURL return String;

   function msearch
      (query : in Unbounded_String) return AurReply;
   function info
      (query : in Unbounded_String) return AurReply;
   function multiinfo
      (packages : in PackageMap.Map) return AurReply;

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
