--------------------------------------------------------------------------------
-- FILE   : cubedos-lib-space_packets.adb
-- SUBJECT: Body of a package containing CCSDS space packet encoding/decoding logic.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body CubedOS.Lib.Space_Packets is

   function Format_Primary_Header
     (APID           : APID_Type;
      Packet_Type    : Packet_Type_Type;
      Sequence_Count : Sequence_Count_Type;
      Data_Length    : Packet_Data_Size_Type;
      Secondary_Header_Flag : Boolean := False;
      Segementation_Flags : Segementation_Flag_Type := Unsegmented) return Primary_Header
   is
      Header : Primary_Header := (others => 0);
   begin
      Header(1) :=
        (case Packet_Type is
            when Telemetry => 16#00#,
            when Telecommand => 16#10#) or
        (if Secondary_Header_Flag then 16#08# else 16#00#) or
        (Octet(Shift_Right(Double_Octet(APID), 8)));

      Header(2) :=
        Octet(APID and 16#FF#);

      Header(3) :=
        (case Segementation_Flags is
            when Continuation_Segment => 16#00#,
            when First_Segment        => 16#40#,
            when Last_Segment         => 16#80#,
            when Unsegmented          => 16#C0#) or
        (Octet(Shift_Right(Double_Octet(Sequence_Count), 8)));

      Header(4) :=
        Octet(Sequence_Count and 16#FF#);

      Header(5) :=
        (Octet(Shift_Right(Double_Octet(Data_Length - 1), 8)));

      Header(6) :=
        Octet(Double_Octet(Data_Length - 1) and 16#FF#);

      return Header;
   end Format_Primary_Header;

end CubedOS.Lib.Space_Packets;
