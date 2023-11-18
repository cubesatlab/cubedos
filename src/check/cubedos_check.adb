---------------------------------------------------------------------------
-- FILE    : cubedos_check.adb
-- SUBJECT : Main procedure of the CubedOS library unit test program.
-- AUTHOR  : (C) Copyright 2024 by Vermont State University
--
---------------------------------------------------------------------------
with AUnit.Run;
with AUnit.Reporter.Text;

with CubedLib_Suite;
-- The unit test program cannot execute infinitely so that it will work cleanly under Jenkins.
--with CubedOS_Messaging_Suite;

procedure CubedOS_Check is
   procedure Run_Lib_Tests is new AUnit.Run.Test_Runner(CubedLib_Suite.Suite);
   --procedure Run_Messaging_Tests is new AUnit.Run.Test_Runner(CubedOS_Messaging_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run_Lib_Tests(Reporter);
--   Run_Messaging_Tests(Reporter);
end CubedOS_Check;
