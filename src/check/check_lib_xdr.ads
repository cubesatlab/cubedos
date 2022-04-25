--------------------------------------------------------------------------------
-- FILE   : check_lib_xdr.ads
-- SUBJECT: Specification of an XDR encoding/decoding unit test package.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_Lib_XDR is

   type Lib_XDR_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Lib_XDR_Test);
   function Name(T : Lib_XDR_Test) return AUnit.Message_String;

end Check_Lib_XDR;
