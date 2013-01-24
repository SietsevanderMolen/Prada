with GNATCOLL.JSON;

package jsonparser is
   use GNATCOLL.JSON;
      procedure PreParseChecks
         (json : in JSON_Value);

      procedure ParseJSON
         (json : in JSON_Value);

      procedure ParseSingleResultJSON
         (json : in JSON_Value);

      procedure ErrorHandler
         (msg  : in String);
end jsonparser;
