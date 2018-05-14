--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-tc_frames.ads
-- SUBJECT: Specification of a package containing CCSDS space packet encoding/decoding logic.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package CubedOS.Lib.TC_Frames is

   subtype Primary_Header is Octet_Array(1 .. 6);

   subtype Packet_Data_Index_Type is
     Natural range 1 .. 65536; -- TODO: Should this upper limit be generic?
   subtype Packet_Data_Size_Type is
     Natural range Packet_Data_Index_Type'First .. Packet_Data_Index_Type'Last;
   type Packet_Data is array(Packet_Data_Index_Type range <>) of Octet;

   type APID_Type is mod 2**11;
   type Packet_Type_Type is (Telemetry, Telecommand);
   type Segementation_Flag_Type is (Continuation_Segment, First_Segment, Last_Segment, Unsegmented);
   type Sequence_Count_Type is mod 2**14;

   -- Space Packet Decoding
   ------------------------
   -- ToDo: Change for TC Frames

   function Extract_Packet_Type(Header : Primary_Header) return Packet_Type_Type is
     (case (Header(1) and 16#10#) is
         when 16#00# => Telemetry,
         when 16#10# => Telecommand,
         when others => Telecommand) -- Should never occur!
     with Inline;


   function Extract_Secondary_Header_Flag(Header : Primary_Header) return Boolean is
     ((Header(1) and 16#08#) = 16#08#)
     with Inline;


   function Extract_APID(Header : Primary_Header) return APID_Type is
     (APID_Type
        (Shift_Left(Double_Octet(Header(1) and 16#07#), 8) or Double_Octet(Header(2))))
     with Inline;


   function Extract_Segementation_Flags(Header : Primary_Header) return Segementation_Flag_Type is
     (case (Header(3) and 16#C0#) is
         when 16#00# => Continuation_Segment,
         when 16#40# => First_Segment,
         when 16#80# => Last_Segment,
         when 16#C0# => Unsegmented,
         when others => Unsegmented) -- Should never occur!
     with Inline;


   function Extract_Sequence_Count(Header : Primary_Header) return Sequence_Count_Type is
     (Sequence_Count_Type
        (Shift_Left(Double_Octet(Header(3) and 16#3F#), 8) or Double_Octet(Header(4))))
     with Inline;


   function Extract_Data_Length(Header : Primary_Header) return Packet_Data_Size_Type is
     (Packet_Data_Size_Type
        (Natural(Shift_Left(Double_Octet(Header(5)), 8) or Double_Octet(Header(6))) + 1))
     with Inline;


   -- TC Frames Encoding
   ------------------------
   -- Non-urgent, to be implemented later

   function Format_Primary_Header
     (APID           : APID_Type;
      Packet_Type    : Packet_Type_Type;
      Sequence_Count : Sequence_Count_Type;
      Data_Length    : Packet_Data_Size_Type;
      Secondary_Header_Flag : Boolean := False;
      Segementation_Flags : Segementation_Flag_Type := Unsegmented) return Primary_Header
     with
       Post =>
       ((Format_Primary_Header'Result(1) and 16#E0#) = 16#00#) and
        (Extract_Packet_Type(Format_Primary_Header'Result) = Packet_Type) and
        (Extract_Secondary_Header_Flag(Format_Primary_Header'Result) = Secondary_Header_Flag) and
        (Extract_APID(Format_Primary_Header'Result) = APID) and
        (Extract_Segementation_Flags(Format_Primary_Header'Result) = Segementation_Flags) and
        (Extract_Sequence_Count(Format_Primary_Header'Result) = Sequence_Count) and
        (Extract_Data_Length(Format_Primary_Header'Result) = Data_Length);

end CubedOS.Lib.TC_Frames;
