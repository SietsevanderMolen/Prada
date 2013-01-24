with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package rpcclient is

   function Get_JSON (query : in String) return Unbounded_String;

end rpcclient;
