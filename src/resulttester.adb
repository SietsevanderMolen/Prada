with results; use results;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure aurhelper is
    r : Result;
begin
    r := Create (
        To_Unbounded_String ("oeu"),
        23,
        To_Unbounded_String ("aoeuo"),
        To_Unbounded_String ("prada"),
        33,
        True,
        To_Unbounded_String ("oaeuao"),
        To_Unbounded_String ("aeouo"),
        To_Unbounded_String ("3.2"));
    Put_Line ("Teeeeeest");
    PrettyPrint (r);
end aurhelper;
