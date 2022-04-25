---------------------------------------------------------------------------
-- FILE    : cubedos_check.adb
-- SUBJECT : Main procedure of the CubedOS library unit test program.
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
---------------------------------------------------------------------------
with AUnit.Run;
with AUnit.Reporter.Text;

with CubedLib_Suite;

procedure CubedOS_Check is
   procedure Run is new AUnit.Run.Test_Runner(CubedLib_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run(Reporter);
end CubedOS_Check;
