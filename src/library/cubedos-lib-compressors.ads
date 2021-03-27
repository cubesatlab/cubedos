--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-compressors.ads
-- SUBJECT: Specification of a package containing compression algorithms.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package CubedOS.Lib.Compressors is
   pragma Elaborate_Body;

   type Compressor is interface;

   Maximum_Block_Size : constant := 1024;
   subtype Block_Index_Type is Positive range 1 .. Maximum_Block_Size;
   subtype Block_Size_Type is Natural range 0 .. Maximum_Block_Size;
   type Uncompressed_Block is array(Block_Index_Type) of Octet;
   type Compressed_Block is array(Block_Index_Type) of Octet;

   procedure Compress
     (C             : in out Compressor;
      Data_In       : in  Uncompressed_Block;
      Data_In_Size  : in  Block_Size_Type;
      Data_Out      : out Compressed_Block;
      Data_Out_Size : out Block_Size_Type) is abstract;

   procedure Flush
     (C             : in out Compressor;
      Data_Out      : out Compressed_Block;
      Data_Out_Size : out Block_Size_Type) is abstract;


   -- Trivial Compressor
   ---------------------
   -- The Trivial_Compressor doesn't actually compress anything! It exists only to check
   -- syntax and behavior, and to check SPARK compatibilty.

   type Trivial_Compressor is new Compressor with
      record
         Operation_Count : Natural;
      end record;

   function Make return Trivial_Compressor;

   overriding procedure Compress
     (C             : in out Trivial_Compressor;
      Data_In       : in  Uncompressed_Block;
      Data_In_Size  : in  Block_Size_Type;
      Data_Out      : out Compressed_Block;
      Data_Out_Size : out Block_Size_Type);

   overriding procedure Flush
     (C             : in out Trivial_Compressor;
      Data_Out      : out Compressed_Block;
      Data_Out_Size : out Block_Size_Type);

end CubedOS.Lib.Compressors;
