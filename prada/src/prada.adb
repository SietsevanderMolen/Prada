with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with JSONParser; use JSONParser;
with AURParser; use AURParser;
with rpcclient;
with Ada.Text_IO; use Ada.Text_IO;

procedure prada is
   str        : Unbounded_String;
   resultlist : JSON_Value;
begin
   str := rpcclient.Get_JSON ("vim");

   --  Try to convert to a json object
   resultlist := ParseJSON (Strm     => To_String (str));

   --  Parse the json object
   ParseAURResponse (json => resultlist);
end prada;
