--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-octet_io.adb
-- SUBJECT: Implementation of a package for simple octet IO, using
--          Ada.Sequential_IO on the backend.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(Off);

with Ada.IO_Exceptions; use Ada.IO_Exceptions;

package body CubedOS.Lib.Octet_IO is

   procedure Open
     (File      : out File_Type;
      Mode      : in  File_Mode_Type;
      File_Name : in  String;
      IO_Error  : out Open_Error_Type)
   is
   begin
      IO_Error := None;

      case Mode is
         when Read =>
            Real_IO.Open(File.Handle, Real_IO.In_File, File_Name);
            File.Mode := Read;
         when Write =>
            Real_IO.Open(File.Handle, Real_IO.Out_File, File_Name);
            File.Mode := Write;
      end case;

   exception
      when Status_Error =>
         IO_Error := File_Already_Open;
      when Name_Error =>
         IO_Error := File_Does_Not_Exist;
      when Use_Error =>
         IO_Error := Invalid_Permissions;
      when others =>
         IO_Error := Unspecified_Error;
   end Open;


   procedure Read
     (File         : in  File_Type;
      Result_Octet : out Octet;
      IO_Error     : out Read_Error_Type)
   is
   begin
      IO_Error := None;

      --if this read check doesn't pass we probably should let user know
      if File.Mode = Read then
         Real_IO.Read(File.Handle, Result_Octet);
      end if;

   exception
      when Mode_Error =>
         IO_Error := Invalid_File_Mode;
      when End_Error =>
         IO_Error := Read_Not_Possible;
      when Data_Error =>
         IO_Error := Invalid_Data_Type;
      when others =>
         IO_Error := Unspecified_Error;
   end Read;


   procedure Write
     (File        : in out File_Type;
      Write_Octet : in  Octet;
      IO_Error    : out Write_Error_Type)
   is
   begin
      IO_Error := None;

      --if this write check doesn't pass we probably should let user know
      if File.Mode = Write then
         Real_IO.Write(File.Handle, Write_Octet);
      end if;

   exception
      when Mode_Error =>
         IO_Error := Invalid_File_Mode;
      when Use_Error =>
         IO_Error := File_Capacity_Exceeded;
      when others =>
         IO_Error := Unspecified_Error;
   end Write;


   procedure Close(File : in out File_Type; IO_Error : out Close_Error_Type) is
   begin
      IO_Error := None;

      Real_IO.Close(File.Handle);

   exception
      when Status_Error =>
         IO_Error := File_Not_Open;
      when others =>
         IO_Error := Unspecified_Error;
   end Close;

end CubedOS.Lib.Octet_IO;
