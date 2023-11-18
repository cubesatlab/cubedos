--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-bounded_strings-check.ads
-- SUBJECT: Package containing unit tests of bounded strings.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package CubedOS.Lib.Bounded_Strings.Check is

   type Lib_Bounded_Strings_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Lib_Bounded_Strings_Test);
   function Name(T : in Lib_Bounded_Strings_Test) return AUnit.Message_String;

end CubedOS.Lib.Bounded_Strings.Check;
