--------------------------------------------------------------------------------
-- FILE   : cubedos-m0001-api.adb
-- SUBJECT: Body of a package that implements the m0001 API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use  CubedOS.Lib;
use  CubedOS.Lib.XDR;

package body CubedOS.m0001.API is

   function Trivial_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Request_ID : Request_ID_Type;
      M1 : Integer;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Trivial_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Integer(M1), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Trivial_Request_Encode;

   procedure Trivial_Request_Decode
      (Message : in  Message_Record;
      M1 : out Integer;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_M1   : XDR.XDR_Integer;
      Last : Data_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      M1 := Integer(XDR.XDR_Integer'First);
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_M1, Last);
         if Raw_M1 in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then
            M1 := Integer(Raw_M1);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Trivial_Request_Decode;

   function Trivial_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Request_ID : Request_ID_Type;
      M1 : Integer;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Trivial_Reply),
         Priority   => Priority);
      Position : Data_Index_Type;
      Last : Data_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Integer(M1), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Trivial_Reply_Encode;

   procedure Trivial_Reply_Decode
      (Message : in  Message_Record;
      M1 : out Integer;
      Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_M1   : XDR.XDR_Integer;
      Last : Data_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      M1 := Integer(XDR.XDR_Integer'First);
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_M1, Last);
         if Raw_M1 in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then
            M1 := Integer(Raw_M1);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Trivial_Reply_Decode;


end CubedOS.m0001.API;
