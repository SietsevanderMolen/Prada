with AUnit.Reporter.Text;
with AUnit.Run;
with Main_Suite; use Main_Suite;

procedure Test_Prada is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
end Test_Prada;
