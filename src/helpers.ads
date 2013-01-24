with GNATCOLL.JSON;

package helpers is
   use GNATCOLL.JSON;

   --  Remove slases from json
   function Un_Escape_String
      (Text : String)
       return UTF8_Unbounded_String;

      --  Output the contents of the Value JSON object.
      procedure ParseJSON
         (json : in JSON_Value);

      procedure ParseSingleResultJSON
         (json : in JSON_Value);
end helpers;
