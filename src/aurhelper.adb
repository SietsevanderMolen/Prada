with GNATCOLL.JSON; use GNATCOLL.JSON;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with helpers; use helpers;
with rpcclient;

procedure aurhelper is
   Stream1    : constant Trace_Handle := Create ("aurhelper");
   str        : Unbounded_String;
   resultlist : JSON_Value;
begin
   str := rpcclient.Get_JSON ("a");

   --  Try to convert to a json object
   resultlist := Read (Strm     => To_String (str),
   Filename => "");

   --  Iterate the Test_JSON object.
   --  The Handler procedure is responsible for
   --  outputting the contents.
   ParseJSON (json => resultlist);

   --  Log
   Trace (Stream1, Integer'Image (Length (str)), "Rec str of len");

   --  Let's not forget to return our json object
end aurhelper;
