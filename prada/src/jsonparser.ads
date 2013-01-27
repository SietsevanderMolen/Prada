with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

package JSONParser is
   InvalidJSONException : exception;

   type JSON_Value_Type is
      (JSON_Null_Type,
      JSON_Boolean_Type,
      JSON_Int_Type,
      JSON_Float_Type,
      JSON_String_Type,
      JSON_Array_Type,
      JSON_Object_Type);

   subtype JSON_Elementary_Value_Type is JSON_Value_Type range
   JSON_Null_Type .. JSON_String_Type;
   subtype JSON_Container_Value_Type is JSON_Value_Type range
   JSON_Array_Type .. JSON_Object_Type;

   type JSON_Value is tagged private;
   type JSON_Array is private;

   JSON_Null : constant JSON_Value;
   Empty_Array : constant JSON_Array;

   --  Utility functions used to translate a JSON value into an ordinary object
   function Kind (Val : JSON_Value) return JSON_Value_Type;

   function ParseJSON (Strm, Filename : String) return JSON_Value;

   function Create return JSON_Value;

   function Create (Val : Integer) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Int_Type);
   --  Creates an integer-typed JSON value

   function Create (Val : Unbounded_String) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_String_Type);
   --  Creates a string-typed JSON value

   function Get (Val : JSON_Value) return Integer;
   pragma Precondition (Kind (Val) = JSON_Int_Type);

   function Get (Val : JSON_Value) return String;
   pragma Precondition (Kind (Val) = JSON_String_Type);

   function Get (Val : JSON_Value) return Unbounded_String;
   pragma Precondition (Kind (Val) = JSON_String_Type);

   function Get (Val : JSON_Value) return JSON_Array;
   pragma Precondition (Kind (Val) = JSON_Array_Type);

   function Get (Val : JSON_Value; Field : String) return JSON_Value;
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   function Get (Val : JSON_Value; Field : String) return Integer;
   pragma Precondition
      (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Int_Type);

   function Get (Val : JSON_Value; Field : String) return JSON_Array;
   pragma Precondition
      (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Array_Type);

   function Get (Val : JSON_Value; Field : String) return String;
   pragma Precondition
      (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_String_Type);

   function Get
      (Val : JSON_Value; Field : String) return Unbounded_String;
      pragma Precondition
         (Kind (Val) = JSON_Object_Type
         and then Kind (Get (Val, Field)) = JSON_String_Type);

   --  Array handling
   function Length (Arr : JSON_Array) return Natural;
   function Get (Arr : JSON_Array; Index : Positive) return JSON_Value;
   procedure Append (Arr : in out JSON_Array; Val : JSON_Value);

private
   type JSON_Array_Access is access all JSON_Array;
   type JSON_Object_Internal;
   type JSON_Object_Access is access all JSON_Object_Internal;

   type Counter is access Natural;

   type JSON_Value is new Ada.Finalization.Controlled with record
      Cnt        : Counter := null;
      Kind       : JSON_Value_Type := JSON_Null_Type;

      Bool_Value : Boolean;
      Int_Value  : Integer;
      Flt_Value  : Float;
      Str_Value  : Unbounded_String;
      Arr_Value  : JSON_Array_Access;
      Obj_Value  : JSON_Object_Access;
   end record;

   package Vect_Pkg is new Ada.Containers.Vectors
      (Index_Type   => Positive,
      Element_Type => JSON_Value);

   package Names_Pkg is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
      Element_Type    => JSON_Value,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type JSON_Array is record
      Vals : Vect_Pkg.Vector;
   end record;

   JSON_Null : constant JSON_Value :=
      (Ada.Finalization.Controlled with
      Kind => JSON_Null_Type,
      others => <>);
   --  Can't call Create, because we would need to see the body of
   --  Initialize and Adjust.

   Empty_Array : constant JSON_Array :=
      (Vals => Vect_Pkg.Empty_Vector);

   type JSON_Object_Internal is record
      Vals  : Names_Pkg.Map;
   end record;
end JSONParser;
