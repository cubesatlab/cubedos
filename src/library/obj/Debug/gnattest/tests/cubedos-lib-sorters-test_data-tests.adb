--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into CubedOS.Lib.Sorters.Test_Data.

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
package body CubedOS.Lib.Sorters.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Less_Than_Or_Equal (Gnattest_T : in out Test);
   procedure Test_Less_Than_Or_Equal_23fb90 (Gnattest_T : in out Test) renames Test_Less_Than_Or_Equal;
--  id:2.2/23fb9002ef7789af/Less_Than_Or_Equal/1/0/
   procedure Test_Less_Than_Or_Equal (Gnattest_T : in out Test) is
   --  cubedos-lib-sorters.ads:14:4:"<="
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Less_Than_Or_Equal;
--  end read only


--  begin read only
   procedure Test_Insertion_Sort (Gnattest_T : in out Test);
   procedure Test_Insertion_Sort_54fe2c (Gnattest_T : in out Test) renames Test_Insertion_Sort;
--  id:2.2/54fe2c9922c7d9b2/Insertion_Sort/1/0/
   procedure Test_Insertion_Sort (Gnattest_T : in out Test) is
   --  cubedos-lib-sorters.ads:18:4:Insertion_Sort
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Insertion_Sort;
--  end read only


--  begin read only
   procedure Test_Push_Heap (Gnattest_T : in out Test);
   procedure Test_Push_Heap_81fd6b (Gnattest_T : in out Test) renames Test_Push_Heap;
--  id:2.2/81fd6bbd5f03b73b/Push_Heap/1/0/
   procedure Test_Push_Heap (Gnattest_T : in out Test) is
   --  cubedos-lib-sorters.ads:27:4:Push_Heap
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Push_Heap;
--  end read only


--  begin read only
   procedure Test_Pop_Heap (Gnattest_T : in out Test);
   procedure Test_Pop_Heap_4a26dc (Gnattest_T : in out Test) renames Test_Pop_Heap;
--  id:2.2/4a26dce573bec13b/Pop_Heap/1/0/
   procedure Test_Pop_Heap (Gnattest_T : in out Test) is
   --  cubedos-lib-sorters.ads:36:4:Pop_Heap
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Pop_Heap;
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
end CubedOS.Lib.Sorters.Test_Data.Tests;
