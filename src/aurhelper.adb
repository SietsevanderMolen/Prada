with GNATCOLL.JSON; use GNATCOLL.JSON;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with jsonparser; use jsonparser;
with Ada.Exceptions; use Ada.Exceptions;
with rpcclient;

procedure aurhelper is
   Stream1    : constant Trace_Handle := Create ("aurhelper");
   str        : Unbounded_String;
   resultlist : JSON_Value;
begin
   str := rpcclient.Get_JSON ("dell");

   --  Try to convert to a json object
   resultlist := Read (Strm     => To_String (str),
   Filename => "");

   --  Parse the json object
   ParseJSON (json => resultlist);

   --  Log
   Trace (Stream1, Integer'Image (Length (str)), "Rec str of len");

   exception
      when Fail : others =>
         Trace (Stream1, Exception_Message (Fail));
         raise;
end aurhelper;
