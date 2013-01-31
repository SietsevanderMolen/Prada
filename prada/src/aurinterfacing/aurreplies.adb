package body AurReplies is
   function Create (
      replyType   : Unbounded_String;
      resultCount : Natural;
      results     : Vector
   ) return AurReply is
      Temp : AurReply;
   begin
      Temp.replyType := replyType;
      Temp.resultCount := resultCount;
      Temp.results := results;
      return Temp;
   end Create;

   function getResultCount (
      reply : AurReply
   ) return Natural is
   begin
      return reply.resultCount;
   end getResultCount;

   function getResults (
      reply : AurReply
   ) return Vector is
   begin
      return reply.results;
   end getResults;

   function getType (
      reply : AurReply
   ) return Unbounded_String is
   begin
      return reply.replyType;
   end getType;

   procedure InstallPackage (
      Pkg : AurPackage
   ) is
   begin
      Pkg.Install;
   end InstallPackage;
end AurReplies;
