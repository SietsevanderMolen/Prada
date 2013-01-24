with GNATCOLL.JSON;

package helpers is

   use GNATCOLL.JSON;

   --  Strip characters from a string
   function StripCharacters
      (The_String     : String;
       The_Characters : String)
       return String;

   --  Output the contents of the Value JSON object.
   procedure Handler
      (Name  : in UTF8_String;
       Value : in JSON_Value);
end helpers;
