--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

   with CubedOS.Lib.XDR;
   with CubedOS.Lib; use CubedOS.Lib;
   use CubedOS.Lib.XDR;

package body CubedOS.Publish_Subscribe_Server.API is

   pragma Warnings
     (GNATprove, Off, """Last"" is set by ""Decode"" but not used",
      Reason => "The last value of Last is not needed");

   procedure Subscribe_Request_Encode
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1_023;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array (Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode (XDR.XDR_Unsigned (Channel), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Message_Type => Subscribe_Request_Msg, Payload => Payload,
         Result => Message, Priority => Priority);
      Result := Immutable (Message);
      Delete (Message);
      pragma Unused (Last, Payload, Position, Message);
   end Subscribe_Request_Encode;

   procedure Send_Subscribe_Request
     (Sender : Module_Mailbox; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Subscribe_Request_Encode
        (Sender_Address => (Message_Manager.Domain_ID, Module_ID (Sender)),
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Channel => Channel, Result => Message, Priority => Priority);
      Message_Manager.Send_Message (Sender, Message);
   end Send_Subscribe_Request;

   procedure Subscribe_Reply_Encode
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Status : Status_Type; Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1_023;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array (Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode (XDR.XDR_Unsigned (Channel), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode
        (XDR.XDR_Unsigned (Status_Type'Pos (Status)), Payload.all, Position,
         Last);
      Position := Last + 1;
      Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Message_Type => Subscribe_Reply_Msg, Payload => Payload,
         Result => Message, Priority => Priority);
      Result := Immutable (Message);
      Delete (Message);
      pragma Unused (Last, Payload, Position, Message);
   end Subscribe_Reply_Encode;

   procedure Send_Subscribe_Reply
     (Sender : Module_Mailbox; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Subscribe_Reply_Encode
        (Sender_Address => (Message_Manager.Domain_ID, Module_ID (Sender)),
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Channel => Channel, Status => Status, Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message (Sender, Message);
   end Send_Subscribe_Reply;

   procedure Unsubscribe_Request_Encode
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1_023;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array (Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode (XDR.XDR_Unsigned (Channel), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Message_Type => Unsubscribe_Request_Msg, Payload => Payload,
         Result => Message, Priority => Priority);
      Result := Immutable (Message);
      Delete (Message);
      pragma Unused (Last, Payload, Position, Message);
   end Unsubscribe_Request_Encode;

   procedure Send_Unsubscribe_Request
     (Sender : Module_Mailbox; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Unsubscribe_Request_Encode
        (Sender_Address => (Message_Manager.Domain_ID, Module_ID (Sender)),
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Channel => Channel, Result => Message, Priority => Priority);
      Message_Manager.Send_Message (Sender, Message);
   end Send_Unsubscribe_Request;

   procedure Unsubscribe_Reply_Encode
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Status : Status_Type; Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1_023;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array (Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode (XDR.XDR_Unsigned (Channel), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode
        (XDR.XDR_Unsigned (Status_Type'Pos (Status)), Payload.all, Position,
         Last);
      Position := Last + 1;
      Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Message_Type => Unsubscribe_Reply_Msg, Payload => Payload,
         Result => Message, Priority => Priority);
      Result := Immutable (Message);
      Delete (Message);
      pragma Unused (Last, Payload, Position, Message);
   end Unsubscribe_Reply_Encode;

   procedure Send_Unsubscribe_Reply
     (Sender : Module_Mailbox; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Unsubscribe_Reply_Encode
        (Sender_Address => (Message_Manager.Domain_ID, Module_ID (Sender)),
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Channel => Channel, Status => Status, Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message (Sender, Message);
   end Send_Unsubscribe_Reply;

   procedure Publish_Request_Encode
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array; Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1_023;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array (Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode (XDR.XDR_Unsigned (Channel), Payload.all, Position, Last);
      Position := Last + 1;
      Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Message_Type => Publish_Request_Msg, Payload => Payload,
         Result => Message, Priority => Priority);
      Result := Immutable (Message);
      Delete (Message);
      pragma Unused (Last, Payload, Position, Message);
   end Publish_Request_Encode;

   procedure Send_Publish_Request
     (Sender : Module_Mailbox; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Publish_Request_Encode
        (Sender_Address => (Message_Manager.Domain_ID, Module_ID (Sender)),
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Channel => Channel, Message_Data => Message_Data,
         Result => Message, Priority => Priority);
      Message_Manager.Send_Message (Sender, Message);
   end Send_Publish_Request;

   procedure Publish_Reply_Encode
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Status : Status_Type; Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1_023;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array (Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode (XDR.XDR_Unsigned (Channel), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode
        (XDR.XDR_Unsigned (Status_Type'Pos (Status)), Payload.all, Position,
         Last);
      Position := Last + 1;
      Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Message_Type => Publish_Reply_Msg, Payload => Payload,
         Result => Message, Priority => Priority);
      Result := Immutable (Message);
      Delete (Message);
      pragma Unused (Last, Payload, Position, Message);
   end Publish_Reply_Encode;

   procedure Send_Publish_Reply
     (Sender : Module_Mailbox; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Publish_Reply_Encode
        (Sender_Address => (Message_Manager.Domain_ID, Module_ID (Sender)),
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Channel => Channel, Status => Status, Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message (Sender, Message);
   end Send_Publish_Reply;

   procedure Publish_Result_Encode
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Data : CubedOS.Lib.Octet_Array; Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1_023;
      Position : Data_Index_Type;
      Last : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array (Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      XDR.Encode (XDR.XDR_Unsigned (Channel), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode
        (XDR.XDR_Unsigned (Data'Length), Payload.all, Position, Last);
      Position := Last + 1;
      XDR.Encode (Data, Payload.all, Position, Last);
      Make_Empty_Message
        (Sender_Address => Sender_Address,
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Message_Type => Publish_Result_Msg, Payload => Payload,
         Result => Message, Priority => Priority);
      Result := Immutable (Message);
      Delete (Message);
      pragma Unused (Last, Payload, Position, Message);
   end Publish_Result_Encode;

   procedure Send_Publish_Result
     (Sender : Module_Mailbox; Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type; Channel : Channel_ID_Type;
      Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      Publish_Result_Encode
        (Sender_Address => (Message_Manager.Domain_ID, Module_ID (Sender)),
         Receiver_Address => Receiver_Address, Request_ID => Request_ID,
         Channel => Channel, Data => Data, Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message (Sender, Message);
   end Send_Publish_Result;

   procedure Subscribe_Request_Decode
     (Message : in Message_Record; Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;

      Position := 0;
      XDR.Decode (Payload (Message).all, Position, Raw_Channel, Last);

      if Raw_Channel < XDR_Unsigned (Channel_ID_Type'First) or
        Raw_Channel > XDR_Unsigned (Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type (Raw_Channel);
         Decode_Status := Success;
      end if;
   end Subscribe_Request_Decode;

   procedure Subscribe_Reply_Decode
     (Message : in Message_Record; Channel : out Channel_ID_Type;
      Status : out Status_Type; Decode_Status : out Message_Status_Type)
   is
      Position : Data_Index_Type;
      Raw_Channel : XDR.XDR_Unsigned;
      Raw_Status : XDR.XDR_Unsigned;
      Last : Data_Index_Type;
   begin
      Decode_Status := Success;
      Channel := Channel_ID_Type'First;
      Status := Status_Type'First;
      Position := 0;
      if Decode_Status = Success then
         XDR.Decode (Payload (Message).all, Position, Raw_Channel, Last);
         Position := Last + 1;
         if Raw_Channel in
           XDR.XDR_Unsigned (Channel_ID_Type'First) ..
           XDR.XDR_Unsigned (Channel_ID_Type'Last)
         then
            Channel := Channel_ID_Type (Raw_Channel);
            Decode_Status := Success;
         else
            Decode_Status := Malformed;
         end if;
      end if;
      if Decode_Status = Success then
         XDR.Decode (Payload (Message).all, Position, Raw_Status, Last);
         if Raw_Status in
           Status_Type'Pos (Status_Type'First) ..
           Status_Type'Pos (Status_Type'Last)
         then
            Status := Status_Type'Val (Raw_Status);
         else
            Decode_Status := Malformed;
            Status := Status_Type'First;
         end if;
      end if;
   end Subscribe_Reply_Decode;

   procedure Unsubscribe_Request_Decode
     (Message : in Message_Record; Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;

      Position := 0;
      XDR.Decode (Payload (Message).all, Position, Raw_Channel, Last);

      if Raw_Channel < XDR_Unsigned (Channel_ID_Type'First) or
        Raw_Channel > XDR_Unsigned (Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type (Raw_Channel);
         Decode_Status := Success;
      end if;
   end Unsubscribe_Request_Decode;

   procedure Unsubscribe_Reply_Decode
     (Message : in Message_Record; Channel : out Channel_ID_Type;
      Status : out Status_Type; Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Status : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;
      Status := Failure;

      Position := 0;
      XDR.Decode (Payload (Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode (Payload (Message).all, Position, Raw_Status, Last);

      if Raw_Channel < XDR_Unsigned (Channel_ID_Type'First) or
        Raw_Channel > XDR_Unsigned (Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type (Raw_Channel);
         if Raw_Status > Status_Type'Pos (Status_Type'Last) then
            Decode_Status := Malformed;
         else
            Status := Status_Type'Val (Raw_Status);
            Decode_Status := Success;
         end if;
      end if;
   end Unsubscribe_Reply_Decode;

   procedure Publish_Request_Decode
     (Message : in Message_Record; Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Last : XDR_Extended_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Size : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;

      Position := 0;
      XDR.Decode (Payload (Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode (Payload (Message).all, Position, Raw_Size, Last);
      Position := Last + 1;

      if Raw_Channel < XDR_Unsigned (Channel_ID_Type'First) or
        Raw_Channel > XDR_Unsigned (Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type (Raw_Channel);
         if Raw_Size > XDR_Unsigned (XDR_Size_Type'Last - 8) then
            Decode_Status := Malformed;
         else
            Size := CubedOS.Lib.Octet_Array_Count (Raw_Size);
            if Size > Message_Data'Length then
               Decode_Status :=
                 Insufficient_Space;   -- Provided Message_Data is too small.
               Size := 0;
            else
               XDR.Decode
                 (Payload (Message).all, Position,
                  Message_Data
                    (Message_Data'First .. Message_Data'First + Size - 1),
                  Last);
               Decode_Status := Success;
            end if;
         end if;
      end if;
   end Publish_Request_Decode;

   procedure Publish_Reply_Decode
     (Message : in Message_Record; Channel : out Channel_ID_Type;
      Status : out Status_Type; Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Status : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;
      Status := Failure;

      Position := 0;
      XDR.Decode (Payload (Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode (Payload (Message).all, Position, Raw_Status, Last);

      if Raw_Channel < XDR_Unsigned (Channel_ID_Type'First) or
        Raw_Channel > XDR_Unsigned (Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type (Raw_Channel);
         if Raw_Status > Status_Type'Pos (Status_Type'Last) then
            Decode_Status := Malformed;
         else
            Status := Status_Type'Val (Raw_Status);
            Decode_Status := Success;
         end if;
      end if;
   end Publish_Reply_Decode;

   procedure Publish_Result_Decode
     (Message : in Message_Record; Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   is
      Position : XDR_Index_Type;
      Last : XDR_Index_Type;
      Raw_Channel : XDR_Unsigned;
      Raw_Size : XDR_Unsigned;
   begin
      Channel := Channel_ID_Type'First;
      Message_Data := (others => 0);
      Size := 0;

      Position := 0;
      XDR.Decode (Payload (Message).all, Position, Raw_Channel, Last);
      Position := Last + 1;
      XDR.Decode (Payload (Message).all, Position, Raw_Size, Last);
      Position := Last + 1;

      if Raw_Channel < XDR_Unsigned (Channel_ID_Type'First) or
        Raw_Channel > XDR_Unsigned (Channel_ID_Type'Last)
      then
         Decode_Status := Malformed;
      else
         Channel := Channel_ID_Type (Raw_Channel);
         if Raw_Size > XDR_Unsigned (XDR_Size_Type'Last - 8) then
            Decode_Status := Malformed;
         else
            Size := CubedOS.Lib.Octet_Array_Count (Raw_Size);
            if Size > Message_Data'Length then
               Decode_Status :=
                 Insufficient_Space;   -- The given Message_Data is too small.
               Size := 0;
            else
               XDR.Decode
                 (Payload (Message).all, Position,
                  Message_Data
                    (Message_Data'First .. Message_Data'First + Size - 1),
                  Last);
               Decode_Status := Success;
            end if;
         end if;
      end if;
   end Publish_Result_Decode;

end CubedOS.Publish_Subscribe_Server.API;
