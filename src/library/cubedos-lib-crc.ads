--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-crc.adb
-- SUBJECT: Specification of a package for handling CRC calculations
-- AUTHOR : (C) Copyright 2016 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package CubedOS.Lib.CRC is

   function CRC_Calculation
     (Buffer : in Octet_Array) return Double_Octet;

   function Continuation_CRC_Calculation
     (Buffer : in Octet_Array;
      Seed   : in Double_Octet) return Double_Octet;

end CubedOS.Lib.CRC;
