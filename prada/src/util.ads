with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Util is
   function UserConfirm
      (Prompt : Unbounded_String)
   return Boolean;
end Util;
