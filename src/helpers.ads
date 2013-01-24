with GNATCOLL.JSON;

package helpers is
   use GNATCOLL.JSON;

   --  Remove slases from json
   function Un_Escape_String
      (Text : String)
       return UTF8_Unbounded_String;
end helpers;
