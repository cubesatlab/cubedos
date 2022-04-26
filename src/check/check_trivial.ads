---------------------------------------------------------------------------
-- FILE    : check_trivial.ads
-- SUBJECT : Package containing tests of nothing in particular.
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
-- The "trivial" test is just a place holder. It can be used as a template for other tests.
--
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_Trivial is

   type Trivial_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Trivial_Test);
   function Name(T : in Trivial_Test) return AUnit.Message_String;

end Check_Trivial;
