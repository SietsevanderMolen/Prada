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

   function GetName (
      this : AurPackage) return Unbounded_String is
   begin
      return this.Name;
   end GetName;

   function GetRefNum (
      this : AurPackage) return Natural is
   begin
      return this.RefNum;
   end GetRefNum;

   procedure Install (this : AurPackage) is
   begin
      Ada.Text_IO.Put_Line (To_String (this.URLPath));
   end Install;

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
         Ada.Characters.Latin_1.ESC & "[0m"
      );
      --  Only print if package is out of date
      if this.OutOfDate > 1 then
         Ada.Text_IO.Put (
            " " &
            Ada.Characters.Latin_1.ESC & "[31;1m" &
            "Out of date" &
            Ada.Characters.Latin_1.ESC & "[0m"
            );
      end if;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (
         --  Spaces to indent descriptions
         "    " &
         --  Description
         To_String (this.Description)
      );
      Ada.Text_IO.New_Line;
   end PrettyPrint;
end AurPackages;
