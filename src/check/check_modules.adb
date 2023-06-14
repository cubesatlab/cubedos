---------------------------------------------------------------------------
-- FILE    : check_modules.adb
-- SUBJECT : Package containing tests for cubedos modules and message system.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--
---------------------------------------------------------------------------

with Check_Message_Passing;

-- Runs unit tests on the modules + message system.
-- Conforms to the ravenscar profile
-- When you run this, silence means everything is ok.
procedure Check_Modules is
begin
   Check_Message_Passing.Run_Tests;
end Check_Modules;
