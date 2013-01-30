with Ada.Text_IO;
with Ada.Characters.Latin_1;

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

   procedure PrettyPrint (this : AurPackage) is
   begin
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (
         Natural'Image (this.RefNum) & " " &
         Ada.Characters.Latin_1.ESC & "[34;1m" & "aur/" &
         Ada.Characters.Latin_1.ESC & "[0;1m" & this.Name & " " &
         Ada.Characters.Latin_1.ESC & "[30;1m" & this.Version
      ));
      Ada.Text_IO.Put (Ada.Characters.Latin_1.ESC & "[0m");
      Ada.Text_IO.Put_Line (To_String ("     " & this.Description));
   end PrettyPrint;

   procedure QuickPrint (this : AurPackage) is
   begin
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (this.Name));
   end QuickPrint;
end AurPackages;
