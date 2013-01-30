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

   procedure PrettyPrintResult (
      Position : Cursor
      ) is
      Result : constant AurPackage := Element (Position => Position);
   begin
      PrettyPrint (Result);
   end PrettyPrintResult;

   procedure PrettyPrintResults (
      Reply : AurReply
      ) is
   begin
      Reply.results.Iterate (Process   => PrettyPrintResult'Access);
   end PrettyPrintResults;

   procedure QuickPrintResult (
      Position : Cursor
      ) is
      Result : constant AurPackage := Element (Position => Position);
   begin
      QuickPrint (Result);
   end QuickPrintResult;

   procedure QuickPrintResults (
      Reply : AurReply
      ) is
   begin
      Reply.results.Iterate (Process   => QuickPrintResult'Access);
   end QuickPrintResults;
end AurReplies;
