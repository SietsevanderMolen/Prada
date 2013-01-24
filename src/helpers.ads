with GNATCOLL.JSON;

package helpers is
   use GNATCOLL.JSON;

   --  Remove slases from json
   function Un_Escape_String
      (Text : String)
       return UTF8_Unbounded_String;

      --  Output the contents of the Value JSON object.
      procedure JSONHandler
         (Name  : in UTF8_String;
          Value : in JSON_Value);

      procedure SingleJSONResultHandler
         (Value : in JSON_Value);
end helpers;
