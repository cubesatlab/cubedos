---------------------------------------------------------------------------
-- FILE    : cubedlib_suite.adb
-- SUBJECT : The main test suite of the CubedOS library unit test program.
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
---------------------------------------------------------------------------

-- The check packages that are children of library packages are organized that way so they will have
-- access to the private section of the parent library package. The check packages that are *not*
-- children do not need access to the private section of the package they are testing.
--
with CubedOS.Lib.Bounded_Strings.Check;

with Check_Trivial;
with Check_Lib_CRC;
with Check_Lib_XDR;
with Check_Bounded_Queue;

package body CubedLib_Suite is
   use AUnit.Test_Suites;

   -- The suite itself.
   Suite_Object : aliased Test_Suite;

   -- The various tests in this suite. Low level tests should be done first since low level packages
   -- might be used during the testing of the higher level packages.
   --
   Test_0 : aliased Check_Trivial.Trivial_Test; -- The trivial test doesn't do anything!
   Test_1 : aliased Check_Lib_CRC.Lib_CRC_Test;
   Test_2 : aliased Check_Lib_XDR.Lib_XDR_Test;
   Test_3 : aliased CubedOS.Lib.Bounded_Strings.Check.Lib_Bounded_Strings_Test;
   Test_4 : aliased Check_Bounded_Queue.Queue_Test;

   -- Function to return an access to the configured suite
   function Suite return Access_Test_Suite is
   begin
      Add_Test(Suite_Object'Access, Test_0'Access);
      Add_Test(Suite_Object'Access, Test_1'Access);
      Add_Test(Suite_Object'Access, Test_2'Access);
      Add_Test(Suite_Object'Access, Test_3'Access);
      Add_Test(Suite_Object'Access, Test_4'Access);
      return Suite_Object'Access;
   end Suite;

end CubedLib_Suite;
