--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-crc-check.adb
-- SUBJECT: Body of a CRC test.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Assertions;
with Ada.Text_IO;

use Ada.Assertions;
use Ada.Text_IO;

package body CubedOS.Lib.CRC.Check is

   procedure Test_CRC_Output is
      Test_Table_One : Octet_Array(1 .. 35539);
      Test_Table_Follow : Octet_Array(1 .. 30000);
      Test_Table_Two : constant Octet_Array(1 .. 10) :=
        (16#11#, 16#15#, 16#C3#, 16#F0#, 16#00#, 16#05#, 16#20#, 16#DC#, 16#55#, 16#F5#);
      CRC_Output : Double_Octet;
   begin
      ---initializes Test_Table_One---
      for I in Integer range 1 .. 35539 loop
               Test_Table_One(I) := 16#10#;
      end loop;
      ---initializes Test_Table_Follow---
      for I in Integer range 1 .. 30000 loop
               Test_Table_Follow(I) := 16#10#;
      end loop;
      CRC_Output := CRC_Calculation(Test_Table_One);
      CRC_Output := Continuation_CRC_Calculation(Test_Table_Follow, CRC_Output);
      Assert(CRC_Output = 16#89DB#, "Bad CRC Value" & Double_Octet'Image(CRC_Output));

      CRC_Output := CRC_Calculation(Test_Table_Two);
      Assert(CRC_Output = 16#9878#, "Bad CRC Value");
   end Test_CRC_Output;


   procedure Run_Tests is
   begin
      Put("CRC: Output"); Test_CRC_Output; Put_Line(" (Ok)");
   end Run_Tests;


end CubedOS.Lib.CRC.Check;
