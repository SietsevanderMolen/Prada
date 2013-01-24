with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

package body Results is
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
   ) return Result is
      Temp : Result;
   begin
      Temp.Description := Description;
      Temp.ID := ID;
      Temp.License := License;
      Temp.Name := Name;
      Temp.NumVotes := NumVotes;
      Temp.OutOfDate := OutOfDate;
      Temp.URL := URL;
      Temp.URLPath := URLPath;
      Temp.Version := Version;
      return Temp;
   end Create;

   procedure PrettyPrint (this : Result) is
   begin
      Put_Line (To_Unbounded_String ("Name: ") & this.Name);
      Put_Line (To_Unbounded_String ("Description: ") & this.Description);
      Put_Line (To_Unbounded_String ("URL: ") & this.URL);
   end PrettyPrint;
end Results;
