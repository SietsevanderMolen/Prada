with GNATCOLL.JSON; use GNATCOLL.JSON;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with jsonparser; use jsonparser;
with Ada.Exceptions; use Ada.Exceptions;
with rpcclient;

procedure prada is
   Stream1    : constant Trace_Handle := Create ("prada");
   str        : Unbounded_String;
   resultlist : JSON_Value;
begin
   Parse_Config_File;   --  parses default ./.gnatdebug

   str := rpcclient.Get_JSON ("dell");

   --  Try to convert to a json object
   resultlist := Read (Strm     => To_String (str),
   Filename => "");

   --  Parse the json object
   ParseJSON (json => resultlist);

   exception
      when Fail : others =>
         Trace (Stream1, Exception_Message (Fail));
         raise;
end prada;
