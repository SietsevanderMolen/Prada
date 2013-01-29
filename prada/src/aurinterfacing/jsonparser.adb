with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with GNAT.Encode_UTF8_String;

-------------------
-- GNATCOLL.JSON --
-------------------
package body JSONParser is
   ------------
   -- Append --
   ------------
   procedure Append (Arr : in out JSON_Array; Val : JSON_Value) is
   begin
      Arr.Vals.Append (Val);
   end Append;

   ------------
   -- Create --
   ------------
   function Create return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Kind := JSON_Null_Type;
      return Ret;
   end Create;

   ------------
   -- Create --
   ------------
   function Create (Val : Integer) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Kind      := JSON_Int_Type;
      Ret.Int_Value := Val;
      return Ret;
   end Create;

   ------------
   -- Create --
   ------------
   function Create (Val : Unbounded_String) return JSON_Value is
      Ret : JSON_Value;
   begin
      Ret.Kind := JSON_String_Type;
      Ret.Str_Value := Val;
      return Ret;
   end Create;

   ---------
   -- Get --
   ---------
   function Get (Val : JSON_Value) return Integer is
   begin
      return Val.Int_Value;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return String is
   begin
      return To_String (Val.Str_Value);
   end Get;

   ---------
   -- Get --
   ---------
   function Get (Val : JSON_Value) return Unbounded_String is
   begin
      return Val.Str_Value;
   end Get;

   ---------
   -- Get --
   ---------
   function Get (Val : JSON_Value) return JSON_Array
   is
   begin
      return Val.Arr_Value.all;
   end Get;

   ---------
   -- Get --
   ---------
   function Get
     (Val   : JSON_Value;
      Field : String) return JSON_Value
   is
      use Names_Pkg;
      J : constant Names_Pkg.Cursor := Val.Obj_Value.Vals.Find (Field);
   begin
      if Has_Element (J) then
         return Element (J);
      end if;

      return JSON_Null;
   end Get;

   ---------
   -- Get --
   ---------
   function Get (Arr : JSON_Array; Index : Positive) return JSON_Value is
   begin
      return Arr.Vals.Element (Index);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : String) return Integer is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get (Val : JSON_Value; Field : String) return String
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get
     (Val : JSON_Value; Field : String) return Unbounded_String
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   function Get (Val : JSON_Value; Field : String) return JSON_Array is
   begin
      return Get (Get (Val, Field));
   end Get;

   ----------
   -- Kind --
   ----------
   function Kind (Val : JSON_Value) return JSON_Value_Type is
   begin
      return Val.Kind;
   end Kind;

   ------------
   -- Length --
   ------------

   function Length (Arr : JSON_Array) return Natural is
   begin
      return Natural (Arr.Vals.Length);
   end Length;

   function ParseJSON
      (Strm     :        String;
      Idx      : access Natural;
      Col      : access Natural;
      Line     : access Natural) return JSON_Value;

   ---------------
   -- ParseJSON --
   ---------------
   function ParseJSON
      (Strm     :        String;
      Idx      : access Natural;
      Col      : access Natural;
      Line     : access Natural) return JSON_Value
   is
      function Un_Escape_String (Text : String) return Unbounded_String;

      procedure Next_Char (N : Natural := 1);

      procedure Skip_Blancks;
      --  Does Idx + 1 until a non-blanck character is found

      function Read_String return Unbounded_String;
      --  Reads a string

      procedure Next_Char (N : Natural := 1) is
      begin
         if N > 1 then
            for J in 1 .. N - 1 loop
               Next_Char;
            end loop;
         end if;

         Idx.all := Idx.all + 1;
         if Idx.all not in Strm'Range then
            Col.all := Col.all + 1;
         elsif Strm (Idx.all) = ASCII.CR then
            Col.all := 0;
         elsif Strm (Idx.all) = ASCII.LF then
            Col.all := 1;
            Line.all := Line.all + 1;
         else
            Col.all := Col.all + 1;
         end if;
      end Next_Char;

      -----------------
      -- Read_String --
      -----------------

      function Read_String return Unbounded_String is
         Prev : Natural;

      begin
         Prev := Idx.all;
         while Idx.all < Strm'Last loop
            Next_Char;
            exit when Strm (Idx.all) = '"'
            and then Strm (Idx.all - 1) /= '\';
         end loop;

         if Strm (Idx.all) /= '"' then
            raise InvalidJSONException with
               ("Invalid string: cannot find ending """);
         end if;

         --  Skip the trailing '"'
         Next_Char;

         return Un_Escape_String (Strm (Prev .. Idx.all - 1));
      end Read_String;

      ------------------
      -- Skip_Blancks --
      ------------------

      procedure Skip_Blancks is
      begin
         while Idx.all <= Strm'Last loop
            exit when Strm (Idx.all) /= ' '
            and then Strm (Idx.all) /= ASCII.HT
            and then Strm (Idx.all) /= ASCII.CR
            and then Strm (Idx.all) /= ASCII.LF;

            Next_Char;
         end loop;
      end Skip_Blancks;

      ----------------------
      -- Un_Escape_String --
      ----------------------

      function Un_Escape_String (Text : String) return Unbounded_String is
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
                  raise InvalidJSONException with
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
                     Append (Unb, '"');
                  when '\' =>
                     Append (Unb, '\');
                  when '/' =>
                     Append (Unb, '\');
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
                     raise InvalidJSONException with
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

   begin
      Skip_Blancks;

      if Idx.all not in Strm'Range then
         raise InvalidJSONException with
            ("Nothing to read from stream");
      end if;

      case Strm (Idx.all) is
         when 'n' | 'N' =>
            --  null
            if To_Lower (Strm (Idx.all .. Idx.all + 3)) /= "null" then
               raise InvalidJSONException with
                  ("Invalid token");
            end if;

            Next_Char (4);

            return Create;

         when '-' | '0' .. '9' =>
            --  Numerical value

            declare
               Unb  : Unbounded_String;
            begin
               --  Potential initial '-'
               if Strm (Idx.all) = '-' then
                  Append (Unb, Strm (Idx.all));
                  Next_Char;

                  if Idx.all > Strm'Last
                     or else Strm (Idx.all) not in '0' .. '9'
                  then
                     raise InvalidJSONException with
                        ("Expecting a digit after the initial '-' when " &
                        "decoding a number");
                  end if;
               end if;

               while Idx.all in Strm'Range loop
                  exit when Idx.all not in Strm'Range
                  or else Strm (Idx.all) not in '0' .. '9';

                  Append (Unb, Strm (Idx.all));
                  Next_Char;
               end loop;

               return Create (Integer'Value (To_String (Unb)));
            end;

         when '"' =>
            return Create (Read_String);

         when '[' =>
            declare
               Arr   : constant JSON_Array_Access := new JSON_Array;
               First : Boolean := True;

            begin
               --  Skip '['
               Next_Char;

               while Idx.all < Strm'Last loop
                  Skip_Blancks;

                  if Idx.all > Strm'Last then
                     raise InvalidJSONException with
                        ("Uncomplete JSON array");
                  end if;

                  exit when Strm (Idx.all) = ']';

                  if not First then
                     if Strm (Idx.all) /= ',' then
                        raise InvalidJSONException with
                           ("Expected ',' in the array value");
                     end if;

                     --  Skip the coma
                     Next_Char;
                  end if;

                  First := False;
                  Append (Arr.all, ParseJSON (Strm, Idx, Col, Line));
               end loop;

               if Idx.all > Strm'Last or else Strm (Idx.all) /= ']' then
                  raise InvalidJSONException with
                     ("Unfinished array, expecting ending ']'");
               end if;

               Next_Char;

               declare
                  Ret : JSON_Value;
               begin
                  Ret.Kind      := JSON_Array_Type;
                  Ret.Arr_Value := Arr;
                  return Ret;
               end;
            end;

         when '{' =>
            declare
               First : Boolean := True;
               Ret   : JSON_Value;

            begin
               --  Allocate internal container
               Ret.Kind      := JSON_Object_Type;
               Ret.Obj_Value := new JSON_Object_Internal;

               --  Skip '{'
               Next_Char;

               while Idx.all < Strm'Last loop
                  Skip_Blancks;

                  if Idx.all > Strm'Last then
                     raise InvalidJSONException with
                        ("Unterminated object value");
                  end if;

                  exit when Strm (Idx.all) = '}';

                  if not First then
                     if Strm (Idx.all) /= ',' then
                        raise InvalidJSONException with
                           ("Expected ',' as object value separator");
                     end if;

                     --  Skip the coma
                     Next_Char;
                  end if;

                  First := False;
                  Skip_Blancks;

                  declare
                     Name : constant Unbounded_String := Read_String;
                  begin
                     Skip_Blancks;

                     if Idx.all > Strm'Last then
                        raise InvalidJSONException with
                           ("Unterminated object value");
                     end if;

                     if Strm (Idx.all) /= ':' then
                        raise InvalidJSONException with
                           ("Expected a value after the name in a JSON object"
                          & " at index" & Idx.all'Img);
                     end if;

                     --  Skip the semi-colon
                     Next_Char;

                     declare
                        Item : constant JSON_Value :=
                           ParseJSON (Strm, Idx, Col, Line);
                     begin
                        Ret.Obj_Value.Vals.Include
                           (Key      => To_String (Name),
                        New_Item => Item);
                     end;
                  end;
               end loop;

               if Idx.all > Strm'Last
                  or else Strm (Idx.all) /= '}'
               then
                  raise InvalidJSONException with
                     ("Unterminated object value");
               end if;

               Next_Char;

               return Ret;
            end;

         when others =>
            raise InvalidJSONException with
            "Invalid token";
      end case;
   end ParseJSON;

   ---------------
   -- ParseJSON --
   ---------------
   function ParseJSON (Strm : String) return JSON_Value is
      Idx  : aliased Natural := Strm'First;
      Col  : aliased Natural := 1;
      Line : aliased Natural := 1;
   begin
      return ParseJSON (Strm, Idx'Access, Col'Access, Line'Access);
   end ParseJSON;
end JSONParser;
