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
      URLPath     : Unbounded_String;
      RefNum      : Natural
   ) return AurPackage;

   function GetName (this : AurPackage) return Unbounded_String;
   --  Gets the name of this package
   function GetRefNum (this : AurPackage) return Natural;
   --  Gets the reference number of this package
   function GetVersion (this : AurPackage) return Unbounded_String;
   --  Gets the version of this package
   function GetOutofdate (this : AurPackage) return Natural;
   --  Gets the outofdate field for this package
   function GetDescription (this : AurPackage) return Unbounded_String;
   --  Gets the description for this package

   procedure Install (this : AurPackage);
   --  Installs this package

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
         RefNum      : Natural;
      end record;
end AurPackages;
