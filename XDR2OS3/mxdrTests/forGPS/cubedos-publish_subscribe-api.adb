--------------------------------------------------------------------------------
-- FILE   : cubedos-Publish_Subscribe-api.adb
-- SUBJECT: Body of a package that implements the Publish_Subscribe API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use CubedOS.Lib;
use CubedOS.Lib.XDR;

package body CubedOS.Publish_Subscribe.API is

   function Subscribe_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Channel : Channel_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Subscribe_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Subscribe_Request_Encode;

   procedure Subscribe_Request_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Channel   : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Channel := Channel_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
         if Raw_Channel in XDR.XDR_Unsigned(Channel_ID_Type'First) .. XDR.XDR_Unsigned(Channel_ID_Type'Last) then
            Channel := Channel_ID_Type(Raw_Channel);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Subscribe_Request_Decode;

   function Subscribe_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Subscribe_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Subscribe_Reply_Encode;

   procedure Subscribe_Reply_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status : out Status_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Channel   : XDR.XDR_Unsigned;
      Raw_Status : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Channel := Channel_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
         Position := Last + 1;
         if Raw_Channel in XDR.XDR_Unsigned(Channel_ID_Type'First) .. XDR.XDR_Unsigned(Channel_ID_Type'Last) then
            Channel := Channel_ID_Type(Raw_Channel);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Status, Last);
         if Raw_Status in Status_Type'Pos(Status_Type'First) .. Status_Type'Pos(Status_Type'Last) then
            Status := Status_Type'Val(Raw_Status);
         else
            Decode_Status := Malformed;
            Status := Status_Type'First;
         end if;
      end if;
   end Subscribe_Reply_Decode;

   function Publish_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Publish_Request),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Publish_Request_Encode;

   procedure Publish_Request_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Channel   : XDR.XDR_Unsigned;
      Raw_Size : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
         Position := Last + 1;
         if Raw_Channel in XDR.XDR_Unsigned(Channel_ID_Type'First) .. XDR.XDR_Unsigned(Channel_ID_Type'Last) then
            Channel := Channel_ID_Type(Raw_Channel);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Size, Last);
         Position := Last + 1;
         if Raw_Size in XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'First) .. XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
         if Size < Message_Data'Length then
            XDR.Decode(Message.Payload, Position, Message_Data(Message_Data'First .. Message_Data'First + Size - 1), Last);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Publish_Request_Decode;

   function Publish_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver : Module_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record := Make_Empty_Message(
         Sender_Domain   => Domain_ID,
         Receiver_Domain => Receiver_Domain,
         Sender     => ID,
         Receiver   => Receiver,
         Request_ID   => 0,
         Message_ID => Message_Type'Pos(Publish_Reply),
         Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Publish_Reply_Encode;

   procedure Publish_Reply_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status : out Status_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Channel   : XDR.XDR_Unsigned;
      Raw_Status : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Channel := Channel_ID_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
         Position := Last + 1;
         if Raw_Channel in XDR.XDR_Unsigned(Channel_ID_Type'First) .. XDR.XDR_Unsigned(Channel_ID_Type'Last) then
            Channel := Channel_ID_Type(Raw_Channel);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Status, Last);
         if Raw_Status in Status_Type'Pos(Status_Type'First) .. Status_Type'Pos(Status_Type'Last) then
            Status := Status_Type'Val(Raw_Status);
         else
            Decode_Status := Malformed;
            Status := Status_Type'First;
         end if;
      end if;
   end Publish_Reply_Decode;

   function Publish_Result_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender : Module_ID_Type;
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
      Request_ID : Request_ID_Type;
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Get_Next_Request_ID(Request_ID);
      Message := Make_Empty_Message(
         Sender_Domain   => Sender_Domain,
         Receiver_Domain => Domain_ID,
         Sender     => Sender,
         Receiver   => ID,
         Request_ID   => Request_ID,
         Message_ID => Message_Type'Pos(Publish_Result),
         Priority   => Priority);
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload, Position, Last);
      Position := Last + 1;
      Message.Size := Position;
      return Message;
   end Publish_Result_Encode;

   procedure Publish_Result_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Raw_Channel   : XDR.XDR_Unsigned;
      Raw_Size : XDR.XDR_Unsigned;
      Last : XDR_Index_Type;
   begin
      pragma Warnings
         (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Decode_Status := Success;
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
         Position := Last + 1;
         if Raw_Channel in XDR.XDR_Unsigned(Channel_ID_Type'First) .. XDR.XDR_Unsigned(Channel_ID_Type'Last) then
            Channel := Channel_ID_Type(Raw_Channel);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode(Message.Payload, Position, Raw_Size, Last);
         Position := Last + 1;
         if Raw_Size in XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'First) .. XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
         if Size < Message_Data'Length then
            XDR.Decode(Message.Payload, Position, Message_Data(Message_Data'First .. Message_Data'First + Size - 1), Last);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
   end Publish_Result_Decode;


end CubedOS.Publish_Subscribe.API;
