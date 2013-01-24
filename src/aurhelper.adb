with GNATCOLL.JSON; use GNATCOLL.JSON;
with rpcclient;

procedure aurhelper is
   str : JSON_Value;
begin
   str := rpcclient.Get_JSON ("vim");
end aurhelper;
