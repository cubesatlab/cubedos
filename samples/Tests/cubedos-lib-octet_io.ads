--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-octet_io.ads
-- SUBJECT: Specification of a package for simple octet IO, using Ada.Sequential_IO.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------

private with Ada.Sequential_IO;

generic
   Device_Name : String;
package CubedOS.Lib.Octet_IO
  with SPARK_Mode => On
is

   type File_Type is limited private;
   type File_Mode_Type is (Read, Write);
   type Open_Error_Type is
     (None, File_Already_Open, File_Does_Not_Exist, Invalid_Permissions, Unspecified_Error);
   type Close_Error_Type is
     (None, File_Not_Open, Unspecified_Error);
   type Read_Error_Type is
     (None, Invalid_File_Mode, Read_Not_Possible, Invalid_Data_Type, Unspecified_Error);
   type Write_Error_Type is
     (None, Invalid_File_Mode, File_Capacity_Exceeded, Unspecified_Error);

   -- File IO procedures
   procedure Open
     (File      : out File_Type;
      Mode      : in  File_Mode_Type;
      File_Name : in  String;
      IO_Error  : out Open_Error_Type)
     with
       Global => null,
       Depends => ((File, IO_Error) => (Mode, File_Name));

   procedure Read
     (File         : in  File_Type;
      Result_Octet : out Octet;
      IO_Error     : out Read_Error_Type)
     with
       Global => null,
       Depends => ((Result_Octet, IO_Error) => File);

   procedure Write
     (File        : in out File_Type;
      Write_Octet : in  Octet;
      IO_Error    : out Write_Error_Type)
     with
       Global => null,
       Depends => ((File, IO_Error) => (File, Write_Octet));

   procedure Close
     (File     : in out File_Type;
      IO_Error : out Close_Error_Type)
     with
       Global => null,
       Depends => ((File, IO_Error) => File);

private
   pragma SPARK_Mode(Off);

   package Real_IO is new Ada.Sequential_IO(Octet);
   type File_Type is
      record
         Handle : Real_IO.File_Type;
         Mode   : File_Mode_Type;
      end record;

end CubedOS.Lib.Octet_IO;

