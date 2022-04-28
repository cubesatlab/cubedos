--------------------------------------------------------------------------------
-- FILE   : check_lib_crc.ads
-- SUBJECT: Specification of a CRC unit test package
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_Lib_CRC is

   type Lib_CRC_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Lib_CRC_Test);
   function Name(T : in Lib_CRC_Test) return AUnit.Message_String;

end Check_Lib_CRC;
