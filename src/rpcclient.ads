with GNATCOLL.JSON; use GNATCOLL.JSON;

package rpcclient is

   function Get_JSON (query : in String) return JSON_Value;

end rpcclient;
