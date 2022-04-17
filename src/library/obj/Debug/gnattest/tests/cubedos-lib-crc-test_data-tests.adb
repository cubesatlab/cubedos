--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into CubedOS.Lib.CRC.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body CubedOS.Lib.CRC.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_CRC_Calculation (Gnattest_T : in out Test);
   procedure Test_CRC_Calculation_f3619f (Gnattest_T : in out Test) renames Test_CRC_Calculation;
--  id:2.2/f3619f172951e56f/CRC_Calculation/1/0/
   procedure Test_CRC_Calculation (Gnattest_T : in out Test) is
   --  cubedos-lib-crc.ads:11:4:CRC_Calculation
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_CRC_Calculation;
--  end read only


--  begin read only
   procedure Test_Continuation_CRC_Calculation (Gnattest_T : in out Test);
   procedure Test_Continuation_CRC_Calculation_3923bc (Gnattest_T : in out Test) renames Test_Continuation_CRC_Calculation;
--  id:2.2/3923bc2857ce0448/Continuation_CRC_Calculation/1/0/
   procedure Test_Continuation_CRC_Calculation (Gnattest_T : in out Test) is
   --  cubedos-lib-crc.ads:14:4:Continuation_CRC_Calculation
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Continuation_CRC_Calculation;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end CubedOS.Lib.CRC.Test_Data.Tests;
