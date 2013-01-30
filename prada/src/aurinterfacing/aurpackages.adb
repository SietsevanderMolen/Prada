with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;

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
      --  Print reference number. Width=2 because no more than 99 results
      --  are usually shown anyway
      Ada.Integer_Text_IO.Put (this.RefNum, Width => 2);
      Ada.Text_IO.Put (
      --  A space to separate the refnum and name
         " " &
      --  Change to blue colour
         Ada.Characters.Latin_1.ESC & "[34;1m" &
      --  Print aur identifier
         "aur/" &
      --  Disable blue color
         Ada.Characters.Latin_1.ESC & "[0;1m" &
      --  Print name
         To_String (this.Name) &
      --  A space to separate the name and version
         " " &
      --  Change to gray colour
         Ada.Characters.Latin_1.ESC & "[30;1m" &
      --  Print version
         To_String (this.Version) &
      --  Reset colour
         Ada.Characters.Latin_1.ESC & "[0m");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (
         --  Spaces to indent descriptions
         To_String ("     " &
         --  Description
         this.Description)
      );
   end PrettyPrint;

   procedure QuickPrint (this : AurPackage) is
   begin
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (this.Name));
   end QuickPrint;
end AurPackages;
