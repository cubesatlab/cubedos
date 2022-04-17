--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into CubedOS.Lib.Compressors.Trivial_Compressor_Test_Data.

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
package body CubedOS.Lib.Compressors.Trivial_Compressor_Test_Data.Trivial_Compressor_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Make (Gnattest_T : in out Test_Trivial_Compressor);
   procedure Test_Make_03a8d9 (Gnattest_T : in out Test_Trivial_Compressor) renames Test_Make;
--  id:2.2/03a8d9c3ee0ad438/Make/1/0/
   procedure Test_Make (Gnattest_T : in out Test_Trivial_Compressor) is
   --  cubedos-lib-compressors.ads:43:4:Make
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Make;
--  end read only


--  begin read only
   procedure Test_Compress (Gnattest_T : in out Test_Trivial_Compressor);
   procedure Test_Compress_589d69 (Gnattest_T : in out Test_Trivial_Compressor) renames Test_Compress;
--  id:2.2/589d69aba16f117e/Compress/1/0/
   procedure Test_Compress (Gnattest_T : in out Test_Trivial_Compressor) is
   --  cubedos-lib-compressors.ads:45:4:Compress
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Compress;
--  end read only


--  begin read only
   procedure Test_Flush (Gnattest_T : in out Test_Trivial_Compressor);
   procedure Test_Flush_83a46c (Gnattest_T : in out Test_Trivial_Compressor) renames Test_Flush;
--  id:2.2/83a46c867a3e2521/Flush/1/0/
   procedure Test_Flush (Gnattest_T : in out Test_Trivial_Compressor) is
   --  cubedos-lib-compressors.ads:52:4:Flush
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Flush;
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
end CubedOS.Lib.Compressors.Trivial_Compressor_Test_Data.Trivial_Compressor_Tests;
