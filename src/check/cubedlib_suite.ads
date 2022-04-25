---------------------------------------------------------------------------
-- FILE    : cubedlib_suite.ads
-- SUBJECT : The main test suite of the CubedOS library unit test program.
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical COllege
--
---------------------------------------------------------------------------
with AUnit.Test_Suites;

package CubedLib_Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end CubedLib_Suite;
