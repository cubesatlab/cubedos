---------------------------------------------------------------------------
-- FILE    : cubedos_messaging_suite.adb
-- SUBJECT : The main test suite of the CubedOS message manger unit test program.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--
---------------------------------------------------------------------------

-- The check packages that are children of library packages are organized that way so they will have
-- access to the private section of the parent library package. The check packages that are *not*
-- children do not need access to the private section of the package they are testing.
--
with CubedOS.Lib.Bounded_Strings.Check;

with Check_Message_Passing;

package body CubedOS_Messaging_Suite is
   use AUnit.Test_Suites;

   -- The suite itself.
   Suite_Object : aliased Test_Suite;

   -- The various tests in this suite. Low level tests should be done first since low level packages
   -- might be used during the testing of the higher level packages.
   --
   Test_0 : aliased Check_Message_Passing.Message_Passing_Test;

   -- Function to return an access to the configured suite
   function Suite return Access_Test_Suite is
   begin
      Add_Test(Suite_Object'Access, Test_0'Access);
      return Suite_Object'Access;
   end Suite;

end CubedOS_Messaging_Suite;
