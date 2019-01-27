--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe-api.ads
-- SUBJECT: Specification of a package that defines the publish/subscribe API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Publish_Subscribe.API is

   type Status_Type is (Success, Failure);
   type Channel_ID_Type is range 1 .. 16;

   type Message_Type is
     (Subscribe_Request,  -- Ask to subscribe to a channel.
      Subscribe_Reply,    -- Success/failure of subscription request.
      Publish_Request,    -- Ask to publish to a channel.
      Publish_Reply,      -- Success/failure of a publish request.
      Publish_Result);    -- Delivery of data published to a channel.

   function Subscribe_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Subscribe_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Status     : Status_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Publish_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Publish_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Status     : Status_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;

   function Publish_Result_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Channel    : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with Global => null;


   function Is_Subscribe_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Subscribe_Request));

   function Is_Subscribe_Reply(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Subscribe_Reply));

   function Is_Publish_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Publish_Request));

   function Is_Publish_Reply(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Publish_Reply));

   function Is_Publish_Result(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Publish_Result));


   procedure Subscribe_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_Subscribe_Request(Message),
     Depends => ((Channel, Decode_Status) => Message);

   procedure Subscribe_Reply_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_Subscribe_Reply(Message),
     Depends => ((Channel, Status, Decode_Status) => Message);

   procedure Publish_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size    : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
       Pre => Is_Publish_Request(Message) and
           Message_Data'First + Message_Data'Length <= Message_Data'Last,
     Depends => ((Channel, Size) => Message, (Message_Data, Decode_Status) => (Message, Message_Data));

   procedure Publish_Reply_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_Publish_Reply(Message),
     Depends => ((Channel, Status, Decode_Status) => Message);

   procedure Publish_Result_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size    : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
       Pre => Is_Publish_Result(Message) and
          Message_Data'First + Message_Data'Length <= Message_Data'Last,
     Depends => ((Channel, Size) => Message, (Message_Data, Decode_Status) => (Message, Message_Data));

end CubedOS.Publish_Subscribe.API;
