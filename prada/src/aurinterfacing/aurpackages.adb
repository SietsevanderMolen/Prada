with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

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
      URLPath     : Unbounded_String
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
      return Temp;
   end Create;

   procedure PrettyPrint (this : AurPackage) is
   begin
      Put_Line (To_Unbounded_String ("Name: ") & this.Name);
      Put_Line (To_Unbounded_String ("Description: ") & this.Description);
      Put_Line (To_Unbounded_String ("URL: ") & this.URL);
   end PrettyPrint;

   procedure QuickPrint (this : AurPackage) is
   begin
      Put_Line (this.Name);
   end QuickPrint;
end AurPackages;
