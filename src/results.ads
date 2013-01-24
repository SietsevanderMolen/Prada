with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Results is
   type Result is tagged private;

   function Create (
      Description : Unbounded_String;
      ID          : Positive;
      License     : Unbounded_String;
      Name        : Unbounded_String;
      NumVotes    : Natural;
      OutOfDate   : Natural;
      URL         : Unbounded_String;
      URLPath     : Unbounded_String;
      Version     : Unbounded_String
   ) return Result;

   procedure PrettyPrint (this : Result);

private
      type Result is tagged
         record
            Description : Unbounded_String;
            ID          : Positive;
            License     : Unbounded_String;
            Name        : Unbounded_String;
            NumVotes    : Natural;
            OutOfDate   : Natural;
            URL         : Unbounded_String;
            URLPath     : Unbounded_String;
            Version     : Unbounded_String;
         end record;
end Results;
