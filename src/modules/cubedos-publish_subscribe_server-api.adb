--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use  CubedOS.Lib;
use  CubedOS.Lib.XDR;

package body CubedOS.Publish_Subscribe_Server.API is

   pragma Warnings
     (GNATprove, Off, """Last"" is set by ""Decode"" but not used",
      Reason  => "The last value of Last is not needed");

   function Subscribe_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Channel    : in Channel_ID_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record :=
        Make_Empty_Message
          (Sender_Address   => Sender_Address,
           Receiver_Address => Name_Resolver.Publish_Subscribe_Server,
           Request_ID => Request_ID,
           Message_Type => (This_Module, Message_Type'Pos(Subscribe_Request)),
           Payload_Size => Message_Manager.Max_Message_Size,
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Subscribe_Request_Encode;


   function Subscribe_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Channel    : in Channel_ID_Type;
      Status     : in Status_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record :=
        Make_Empty_Message
          (Sender_Address   => Name_Resolver.Publish_Subscribe_Server,
           Receiver_Address => Receiver_Address,
           Request_ID => Request_ID,
           Message_Type => (This_Module, Message_Type'Pos(Subscribe_Reply)),
           Payload_Size => Message_Manager.Max_Message_Size,
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Subscribe_Reply_Encode;


   function Unsubscribe_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Channel        : in Channel_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record :=
        Make_Empty_Message
          (Sender_Address => Sender_Address,
           Receiver_Address => Name_Resolver.Publish_Subscribe_Server,
           Request_ID => Request_ID,
           Message_Type => (This_Module, Message_Type'Pos(Unsubscribe_Request)),
           Payload_Size => Message_Manager.Max_Message_Size,
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Unsubscribe_Request_Encode;


   function Unsubscribe_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Channel    : in Channel_ID_Type;
      Status     : in Status_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record :=
        Make_Empty_Message
          (Sender_Address   => Name_Resolver.Publish_Subscribe_Server,
           Receiver_Address => Receiver_Address,
           Request_ID => Request_ID,
           Message_Type => (This_Module, Message_Type'Pos(Unsubscribe_Reply)),
           Payload_Size => Message_Manager.Max_Message_Size,
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Unsubscribe_Reply_Encode;


   function Publish_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID   : in Request_ID_Type;
      Channel      : in Channel_ID_Type;
      Message_Data : in CubedOS.Lib.Octet_Array;
      Priority     : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record :=
        Make_Empty_Message
          (Sender_Address => Sender_Address,
           Receiver_Address => Name_Resolver.Publish_Subscribe_Server,
           Request_ID => Request_ID,
           Message_Type => (This_Module, Message_Type'Pos(Publish_Request)),
           Payload_Size => Message_Manager.Max_Message_Size,
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Publish_Request_Encode;


   function Publish_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Channel    : in Channel_ID_Type;
      Status     : in Status_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record :=
        Make_Empty_Message
          (Sender_Address   => Name_Resolver.Publish_Subscribe_Server,
           Receiver_Address => Receiver_Address,
           Request_ID => Request_ID,
           Message_Type => (This_Module, Message_Type'Pos(Publish_Reply)),
           Payload_Size => Message_Manager.Max_Message_Size,
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload.all, Position, Last);
      return Immutable(Message);
   end Publish_Reply_Encode;


   function Publish_Result_Encode
     (Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Channel    : in Channel_ID_Type;
      Message_Data : in CubedOS.Lib.Octet_Array;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : constant Mutable_Message_Record :=
        Make_Empty_Message
          (Sender_Address => Name_Resolver.Publish_Subscribe_Server,
           Receiver_Address => Receiver_Address,
           Request_ID => Request_ID,
           Message_Type => (This_Module, Message_Type'Pos(Publish_Result)),
           Payload_Size => Message_Manager.Max_Message_Size,
           Priority   => Priority);
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
   begin
      Position := 0;
      XDR.Encode(XDR.XDR_Unsigned(Channel), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(XDR.XDR_Unsigned(Message_Data'Length), Message.Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode(Message_Data, Message.Payload.all, Position, Last);
      return Immutable(Message);
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
      Channel := Channel_ID_Type'First;

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Channel, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or
         Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last)
      then
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
      Channel := Channel_ID_Type'First;
      Status := Failure;

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Status, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or
         Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last)
      then
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


   procedure Unsubscribe_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Channel, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or
         Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         Decode_Status := Success;
      end if;
   end Unsubscribe_Request_Decode;


   procedure Unsubscribe_Reply_Decode
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
      Channel := Channel_ID_Type'First;
      Status := Failure;

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Status, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or
         Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last)
      then
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
   end Unsubscribe_Reply_Decode;


   procedure Publish_Request_Decode
     (Message       : in  Message_Record;
      Channel       : out Channel_ID_Type;
      Message_Data  : out CubedOS.Lib.Octet_Array;
      Size          : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Extended_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Size    : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Size, Last);
      Position := Last + 1;

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or
         Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         if Raw_Size > XDR_Unsigned(XDR_Size_Type'Last - 8) then
            Decode_Status := Malformed;
         else
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            if Size > Message_Data'Length then
               Decode_Status := Insufficient_Space;   -- Provided Message_Data is too small.
               Size := 0;
            else
               XDR.Decode
                 (Payload(Message).all,
                  Position,
                  Message_Data(Message_Data'First .. Message_Data'First + Size - 1),
                  Last);
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
      Channel := Channel_ID_Type'First;
      Status := Failure;

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Status, Last);

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or
         Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last)
      then
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
     (Message       : in  Message_Record;
      Channel       : out Channel_ID_Type;
      Message_Data  : out CubedOS.Lib.Octet_Array;
      Size          : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position    : XDR_Index_Type;
      Last        : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Size    : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;

      Position := 0;
      XDR.Decode(Payload(Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode(Payload(Message).all, Position, Raw_Size, Last);
      Position := Last + 1;

      if Raw_Channel < XDR_Unsigned(Channel_ID_Type'First) or
         Raw_Channel > XDR_Unsigned(Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type(Raw_Channel);
         if Raw_Size > XDR_Unsigned(XDR_Size_Type'Last - 8) then
            Decode_Status := Malformed;
         else
            Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);
            if Size > Message_Data'Length then
               Decode_Status := Insufficient_Space;   -- The given Message_Data is too small.
               Size := 0;
            else
               XDR.Decode
                 (Payload(Message).all,
                  Position,
                  Message_Data(Message_Data'First .. Message_Data'First + Size - 1),
                  Last);
               Decode_Status := Success;
            end if;
         end if;
      end if;
   end Publish_Result_Decode;

end CubedOS.Publish_Subscribe_Server.API;
