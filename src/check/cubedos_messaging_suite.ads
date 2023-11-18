---------------------------------------------------------------------------
-- FILE    : cubedos_messaging_suite.ads
-- SUBJECT : The main test suite of the CubedOS message manager unit test program.
-- AUTHOR  : (C) Copyright 2024 by Vermont State University
--
---------------------------------------------------------------------------
with AUnit.Test_Suites;

package CubedOS_Messaging_Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end CubedOS_Messaging_Suite;
