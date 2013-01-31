with Ada.Text_IO;

package body AurPackages is
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
      ) return AurPackage is
      Temp : AurPackage;
   begin
      Temp.Name := Name;
      Temp.Maintainer := Maintainer;
      Temp.ID := ID;
      Temp.Version := Version;
      Temp.CategoryID := CategoryID;
      Temp.Description := Description;
      Temp.License := License;
      Temp.NumVotes := NumVotes;
      Temp.OutOfDate := OutOfDate;
      Temp.Submitted := Submitted;
      Temp.Modified := Modified;
      Temp.URL := URL;
      Temp.URLPath := URLPath;
      Temp.RefNum := RefNum;
      return Temp;
   end Create;

   function GetDescription (
      this : AurPackage) return Unbounded_String is
   begin
      return this.Description;
   end GetDescription;

   function GetName (
      this : AurPackage) return Unbounded_String is
   begin
      return this.Name;
   end GetName;

   function GetOutofdate (
      this : AurPackage) return Natural is
   begin
      return this.OutOfDate;
   end GetOutofdate;

   function GetRefNum (
      this : AurPackage) return Natural is
   begin
      return this.RefNum;
   end GetRefNum;

   function GetVersion (
      this : AurPackage) return Unbounded_String is
   begin
      return this.Version;
   end GetVersion;

   procedure Install (this : AurPackage) is
   begin
      Ada.Text_IO.Put_Line (To_String (this.URLPath));
   end Install;
end AurPackages;
