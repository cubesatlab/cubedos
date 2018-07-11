--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use  CubedOS.Lib;
use  CubedOS.Lib.XDR;

package body CubedOS.Publish_Subscribe.API is

   function Subscribe_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Domain   => Sender_Domain,
           Receiver_Domain => Domain_ID,
           Sender     => Sender,
           Receiver   => ID,
           Request_ID => Request_ID,
           Message_ID => Message_Type'Pos(Subscribe_Request),
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Subscribe_Request_Encode;


   function Subscribe_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Status     : Status_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Domain   => Domain_ID,
           Receiver_Domain => Receiver_Domain,
           Sender     => ID,
           Receiver   => Receiver,
           Request_ID => Request_ID,
           Message_ID => Message_Type'Pos(Subscribe_Reply),
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Subscribe_Reply_Encode;


   function Publish_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Domain   => Sender_Domain,
           Receiver_Domain => Domain_ID,
           Sender     => Sender,
           Receiver   => ID,
           Request_ID => Request_ID,
           Message_ID => Message_Type'Pos(Publish_Request),
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload, Position, Last); --need similar pre-condition to cfdp api issue...
      Message.Size := Last + 1;
      return Message;
   end Publish_Request_Encode;


   function Publish_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Status     : Status_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Domain   => Domain_ID,
           Receiver_Domain => Receiver_Domain,
           Sender     => ID,
           Receiver   => Receiver,
           Request_ID => Request_ID,
           Message_ID => Message_Type'Pos(Publish_Reply),
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
      Message.Size := Last + 1;
      return Message;
   end Publish_Reply_Encode;


   function Publish_Result_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record :=
        Make_Empty_Message
          (Sender_Domain   => Domain_ID,
           Receiver_Domain => Receiver_Domain,
           Sender     => ID,
           Receiver   => Receiver,
           Request_ID => Request_ID,
           Message_ID => Message_Type'Pos(Publish_Result),
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload, Position, Last); --same issue here as above...
      Message.Size := Last + 1;
      return Message;
   end Publish_Result_Encode;


   procedure Subscribe_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Channel := Channel_ID_Type'First;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Channel, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         Decode_Status := Success;
      end if;
   end Subscribe_Request_Decode;


   procedure Subscribe_Reply_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Status  : XDR_Unsigned;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Channel := Channel_ID_Type'First;
      Status := Failure;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Message.Payload, Position, Raw_Status, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         if Raw_Status > Status_Type'Pos(Status_Type'Last) then
            Decode_Status := Malformed;
         else
            Status := Status_Type'Val(Raw_Status);
            Decode_Status := Success;
         end if;
      end if;
   end Subscribe_Reply_Decode;


   procedure Publish_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size    : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Extended_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Size    : XDR_Unsigned;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Message.Payload, Position, Raw_Size, Last);
      Position := Last + 1;

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         if Raw_Size > XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then
            Decode_Status := Malformed;
         else
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            if Size > Message_Data'Length then
               Decode_Status := Malformed;
            else
               XDR.Decode(Message.Payload, Position, Message_Data(Message_Data'First .. Message_Data'First + Size - 1), Last);
               Decode_Status := Success;
            end if;
         end if;
      end if;
   end Publish_Request_Decode;


   procedure Publish_Reply_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Status  : XDR_Unsigned;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Channel := Channel_ID_Type'First;
      Status := Failure;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Message.Payload, Position, Raw_Status, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         if Raw_Status > Status_Type'Pos(Status_Type'Last) then
            Decode_Status := Malformed;
         else
            Status := Status_Type'Val(Raw_Status);
            Decode_Status := Success;
         end if;
      end if;
   end Publish_Reply_Decode;


   procedure Publish_Result_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size    : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Size    : XDR_Unsigned;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Last""", Reason => "The last value of Last is not needed");
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;

      Position := 0;
      XDR.Decode(Message.Payload, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Message.Payload, Position, Raw_Size, Last);
      Position := Last + 1;

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last) then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         if Raw_Size > XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then
            Decode_Status := Malformed;
         else
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            if Size > Message_Data'Length then
               Decode_Status := Malformed;
            else
               XDR.Decode(Message.Payload, Position, Message_Data(Message_Data'First .. Message_Data'First + Size - 1), Last);
               Decode_Status := Success;
            end if;
         end if;
      end if;
   end Publish_Result_Decode;

end CubedOS.Publish_Subscribe.API;
