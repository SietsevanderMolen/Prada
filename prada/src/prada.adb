with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with JSONParser; use JSONParser;
with AURParser; use AURParser;
with Ada.Exceptions; use Ada.Exceptions;
with rpcclient;

procedure prada is
   Stream1    : constant Trace_Handle := Create ("prada");
   str        : Unbounded_String;
   resultlist : JSON_Value;
begin
   Parse_Config_File;   --  parses default ./.gnatdebug

   str := rpcclient.Get_JSON ("ada");

   --  Try to convert to a json object
   resultlist := ParseJSON (Strm     => To_String (str));

   --  Parse the json object
   ParseAURResponse (json => resultlist);

   exception
      when Fail : others =>
         Trace (Stream1, Exception_Message (Fail));
         raise;
end prada;
