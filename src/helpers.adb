with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Unchecked_Conversion;
with GNAT.Encode_UTF8_String;
with Results; use Results;

package body helpers is
   ---------------------
   --  JSON Iterator  --
   ---------------------
   procedure JSONHandler
      (Name  : in UTF8_String;
       Value : in JSON_Value)
   is
      use Ada.Text_IO;
   begin
      if Ada.Strings.Equal_Case_Insensitive (Name, "type") then
         Put_Line (Get (Value));
      elsif Ada.Strings.Equal_Case_Insensitive (Name, "resultcount") then
         Put_Line (Integer'Image (Get (Value)));
      elsif Ada.Strings.Equal_Case_Insensitive (Name, "results") then
         declare
            A_JSON_Array : constant JSON_Array := Get (Val => Value);
            A_JSON_Value : JSON_Value;
            Array_Length : constant Natural := Length (A_JSON_Array);
         begin
            Put (Name & "(array):[");
            for J in 1 .. Array_Length loop
               A_JSON_Value := Get (Arr   => A_JSON_Array,
                                    Index => J);
               SingleJSONResultHandler (A_JSON_Value);
            end loop;
            Put ("]");
            New_Line;
         end;
      end if;
   end JSONHandler;

   -------------------------------------
   --  Parses a single search result  --
   -------------------------------------
   procedure SingleJSONResultHandler
      (Value : in JSON_Value)
   is
      use Ada.Text_IO;
      res         : Result;
   begin
      res := Results.Create (
         Get (Val => Value, Field => "Description"),
         Get (Val => Value, Field => "ID"),
         Get (Val => Value, Field => "License"),
         Get (Val => Value, Field => "Name"),
         Get (Val => Value, Field => "NumVotes"),
         Get (Val => Value, Field => "OutOfDate"),
         Get (Val => Value, Field => "URL"),
         Get (Val => Value, Field => "URLPath"),
         Get (Val => Value, Field => "Version")
      );
      PrettyPrint (res);
   end SingleJSONResultHandler;

   ----------------------------------
   --  Fixes escaping of a string  --
   ----------------------------------
   function Un_Escape_String (Text : String) return UTF8_Unbounded_String is
      First : Integer;
      Last  : Integer;
      Unb   : Unbounded_String;
      Idx   : Natural;

   begin
      First := Text'First;
      Last  := Text'Last;

      --  Trim blancks and the double quotes

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
                  --  Keep \"
                  Append (Unb, '\' & '"');
               when '\' =>
                  Append (Unb, '\');
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
