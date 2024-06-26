--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-crc-check.adb
-- SUBJECT: Body of a CRC unit test package.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with AUnit.Assertions;
with CubedOS.Lib;
with CubedOS.Lib.CRC;

use AUnit.Assertions;
use CubedOS.Lib;
use CubedOS.Lib.CRC;

package body Check_Lib_CRC is

   procedure Test_CRC_Output(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      Test_Table_One : Octet_Array(1 .. 35539);
      Test_Table_Follow : Octet_Array(1 .. 30000);
      Test_Table_Two : constant Octet_Array(1 .. 10) :=
        [16#11#, 16#15#, 16#C3#, 16#F0#, 16#00#, 16#05#, 16#20#, 16#DC#, 16#55#, 16#F5#];
      CRC_Output : Double_Octet;
   begin
      -- Initializes Test_Table_One
      for I in Integer range 1 .. 35539 loop
         Test_Table_One(I) := 16#10#;
      end loop;
      -- Initializes Test_Table_Follow
      for I in Integer range 1 .. 30000 loop
         Test_Table_Follow(I) := 16#10#;
      end loop;
      CRC_Output := CRC_Calculation(Test_Table_One);
      CRC_Output := Continuation_CRC_Calculation(Test_Table_Follow, CRC_Output);
      Assert(CRC_Output = 16#89DB#, "Bad CRC Value" & Double_Octet'Image(CRC_Output));

      CRC_Output := CRC_Calculation(Test_Table_Two);
      Assert(CRC_Output = 16#9878#, "Bad CRC Value");
   end Test_CRC_Output;


   procedure Register_Tests(T : in out Lib_CRC_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_CRC_Output'Access, "Output");
   end Register_Tests;


   function Name(T : in Lib_CRC_Test) return AUnit.Message_String is
      pragma Unreferenced(T);

   begin
      return AUnit.Format("Lib.CRC");
   end Name;



end Check_Lib_CRC;
