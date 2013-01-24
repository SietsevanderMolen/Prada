with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with GNAT.Encode_UTF8_String;

package body helpers is
   ----------------------------------
   --  Fixes escaping of a string  --
   ----------------------------------
   function Un_Escape_String (Text : String) return UTF8_Unbounded_String is
      First   : Integer;
      Last    : Integer;
      Unb     : Unbounded_String;
      Idx     : Natural;

   begin
      First := Text'First;
      Last  := Text'Last;

      --  Trim blanks and the double quotes

      while First <= Text'Last and then Text (First) = ' ' loop
         First := First + 1;
      end loop;
      if First <= Text'Last and then Text (First) = '"' then
         First := First + 1;
      end if;

      while Last >= Text'First and then Text (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if Last >= Text'First and then Text (Last) = '"' then
         Last := Last - 1;
      end if;

      Idx := First;
      while Idx <= Last loop
         if Text (Idx) = '\' then
            Idx := Idx + 1;

            if Idx > Text'Last then
               raise Invalid_JSON_Stream with
               "Unexpected escape character at end of line";
            end if;

            case Text (Idx) is
               when 'u' | 'U' =>
                  declare
                     I : constant Short_Integer :=
                        Short_Integer'Value
                           ("16#" & Text (Idx + 1 .. Idx + 4) & "#");
                        function Unch is new Ada.Unchecked_Conversion
                           (Short_Integer, Wide_Character);
                  begin
                     Append
                        (Unb,
                        GNAT.Encode_UTF8_String.Encode_Wide_String
                           ("" & Unch (I)));
                     Idx := Idx + 4;
                  end;

               when '"' =>
                  --  Escape "
                  Append (Unb, '\' & '"');
               when '\' =>
                  --  Escape \
                  Append (Unb, '\' & '\');
               when '/' =>
                  Append (Unb, '/');
               when 'b' =>
                  Append (Unb, ASCII.BS);
               when 'f' =>
                  Append (Unb, ASCII.FF);
               when 'n' =>
                  Append (Unb, ASCII.LF);
               when 'r' =>
                  Append (Unb, ASCII.CR);
               when 't' =>
                  Append (Unb, ASCII.HT);
               when others =>
                  raise Invalid_JSON_Stream with
                  "Unexpected escape sequence '\" & Text (Idx) & "'";
            end case;

         else
            Append
               (Unb, Text (Idx));
         end if;

         Idx := Idx + 1;
      end loop;

      return Unb;
   end Un_Escape_String;
end helpers;
