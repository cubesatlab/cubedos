--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-compressors.adb
-- SUBJECT: Implementation of a package containing compression algorithms.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body CubedOS.Lib.Compressors is

   function Make return Trivial_Compressor is
      Result : Trivial_Compressor;
   begin
      Result.Operation_Count := 0;
      return Result;
   end Make;


   procedure Compress
     (C             : in out Trivial_Compressor;
      Data_In       : in  Uncompressed_Block;
      Data_In_Size  : in  Block_Size_Type;
      Data_Out      : out Compressed_Block;
      Data_Out_Size : out Block_Size_Type)
   is
   begin
      Data_Out := (others => 0);
      for I in 1 .. Data_In_Size loop
         Data_Out(I) := Data_In(I);
      end loop;
      Data_Out_Size := Data_In_Size;
      C.Operation_Count := C.Operation_Count + 1;
   end Compress;


   procedure Flush
     (C             : in out Trivial_Compressor;
      Data_Out      : out Compressed_Block;
      Data_Out_Size : out Block_Size_Type)
   is
   begin
      Data_Out := (others => 0);
      Data_Out_Size := 0;
      C.Operation_Count := C.Operation_Count + 1;
   end Flush;

end CubedOS.Lib.Compressors;
