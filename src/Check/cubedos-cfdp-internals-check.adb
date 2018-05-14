--------------------------------------------------------------------------------
-- FILE   : cubedos-cfdp-internals-check.adb
-- SUBJECT: Package containing unit tests of internal CFDP subprograms.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Assertions;
with Ada.Text_IO;

use Ada.Assertions;
use Ada.Text_IO;

package body CubedOS.CFDP.Internals.Check is

   procedure Test_Encode_Fixed_Header is

      Header : constant Fixed_Header_Record :=
        (Version            => 0,
         PDU                => File_Directive,
         Direction          => Toward_Receiver,
         Transmission_Mode  => Unacknowledged,
         CRC_Flag           => CRC_Not_Present,
         Data_Length        => 255,
         Source_Entity      => 16#FFFF#,
         Transaction        => 16#A055#,
         Destination_Entity => 16#0001#);

      Expected : constant Fixed_Header_Array :=
        (16#04#, 16#00#, 16#FF#, 16#11#, 16#FF#, 16#FF#, 16#A0#, 16#55#, 16#00#, 16#01#);

   begin
      Assert(Encode_Fixed_PDU_Header(Header) = Expected, "Invalid fixed PDU encoding");
   end Test_Encode_Fixed_Header;


   procedure Test_Decode_Fixed_Header is

      Header : constant Fixed_Header_Array :=
        (16#04#, 16#00#, 16#FF#, 16#11#, 16#FF#, 16#FF#, 16#A0#, 16#55#, 16#00#, 16#01#);

      Header_Information : Fixed_Header_Record;

      Expected : constant Fixed_Header_Record :=
        (Version            => 0,
         PDU                => File_Directive,
         Direction          => Toward_Receiver,
         Transmission_Mode  => Unacknowledged,
         CRC_Flag           => CRC_Not_Present,
         Data_Length        => 255,
         Source_Entity      => 16#FFFF#,
         Transaction        => 16#A055#,
         Destination_Entity => 16#0001#);

      Status : PDU_Status_Type;

   begin
      Decode_Fixed_PDU_Header(Header, Header_Information, Status);
      Assert(Status = Success, "Unable to decode fixed PDU header");
      Assert(Header_Information = Expected, "Invalid fixed PDU decoding");
   end Test_Decode_Fixed_Header;


   procedure Run_Tests is
   begin
      Put("CFDP Internals: Encode Fixed Header"); Test_Encode_Fixed_Header; Put_Line(" (Ok)");
      Put("CFDP Internals: Decode Fixed Header"); Test_Decode_Fixed_Header; Put_Line(" (Ok)");
   end Run_Tests;


end CubedOS.CFDP.Internals.Check;
