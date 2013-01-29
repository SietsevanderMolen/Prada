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
end AurReplies;
