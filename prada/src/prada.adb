with AurReplies; use AurReplies;
with AurInterface;

procedure prada is
   results : AurReply;
begin
   results := AurInterface.search ("vim-endwise");
   --  Try to convert to a json object
   --  resultlist := ParseJSON (Strm     => To_String (str));

   --  Parse the json object
   --  ParseAURResponse (json => resultlist);
end prada;
