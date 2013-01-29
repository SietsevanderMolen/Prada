with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AurPackages is
   type AurPackage is tagged private;

   function Create (
      Name        : Unbounded_String;
      Maintainer  : Unbounded_String;
      ID          : Positive;
      Version     : Unbounded_String;
      CategoryID  : Natural;
      Description : Unbounded_String;
      License     : Unbounded_String;
      NumVotes    : Natural;
      OutOfDate   : Natural;
      Submitted   : Positive;
      Modified    : Positive;
      URL         : Unbounded_String;
      URLPath     : Unbounded_String
   ) return AurPackage;

   procedure PrettyPrint (this : AurPackage);

private
   type AurPackage is tagged
      record
         Name        : Unbounded_String;
         Maintainer  : Unbounded_String;
         ID          : Positive;
         Version     : Unbounded_String;
         CategoryID  : Natural;
         Description : Unbounded_String;
         License     : Unbounded_String;
         NumVotes    : Natural;
         OutOfDate   : Natural;
         Submitted   : Positive;
         Modified    : Positive;
         URL         : Unbounded_String;
         URLPath     : Unbounded_String;
      end record;
end AurPackages;
