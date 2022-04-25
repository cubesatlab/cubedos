---------------------------------------------------------------------------
-- FILE    : cubedlib_suite.adb
-- SUBJECT : The main test suite of the CubedOS library unit test program.
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
---------------------------------------------------------------------------

with Check_Lib_XDR;

package body CubedLib_Suite is
   use AUnit.Test_Suites;

   -- The suite itself.
   Suite_Object : aliased Test_Suite;

   -- The various tests in this suite. Low level tests should be done first.
   Test_1 : aliased Check_Lib_XDR.Lib_XDR_Test;

   -- Function to return an access to the configured suite
   function Suite return Access_Test_Suite is
   begin
      Add_Test(Suite_Object'Access, Test_1'Access);
      return Suite_Object'Access;
   end Suite;

end CubedLib_Suite;
