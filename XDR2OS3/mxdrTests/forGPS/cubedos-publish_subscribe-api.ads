--------------------------------------------------------------------------------
-- FILE   : cubedos-Publish_Subscribe-api.ads
-- SUBJECT: Specification of a package that defines the Publish_Subscribe API
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
-- All the subprograms in this package must be task safe. They can be simultaneously
--called from multiple tasks. If possible, make every subprogram here a pure function.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Publish_Subscribe.API is

   type Message_Type is
      (Publish_Request, 
      Publish_Result, 
      Publish_Reply, 
      Subscribe_Request, 
      Subscribe_Reply);

   type Status_Type is 
         (Success, 
         Failure);

   type Channel_ID_Type is new Lib.Quadruple_Octet;

   function Subscribe_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Channel : Channel_ID_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Subscribe_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Subscribe_Request));

   procedure Subscribe_Request_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Subscribe_Request(Message),
      Depends => ((Channel, Decode_Status) => Message);


   function Subscribe_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Subscribe_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Subscribe_Reply));

   procedure Subscribe_Reply_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status : out Status_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Subscribe_Reply(Message),
      Depends => ((Channel, Status, Decode_Status) => Message);


   function Publish_Request_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Publish_Request(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Publish_Request));

   procedure Publish_Request_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Publish_Request(Message),
      Depends => ((Channel, Message_Data, Size, Decode_Status) => Message);


   function Publish_Reply_Encode
      (Receiver_Domain : Domain_ID_Type;
      Receiver  : Module_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Publish_Reply(Message : Message_Record) return Boolean is
      (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Publish_Reply));

   procedure Publish_Reply_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status : out Status_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Publish_Reply(Message),
      Depends => ((Channel, Status, Decode_Status) => Message);


   function Publish_Result_Encode
      (Sender_Domain : Domain_ID_Type;
      Sender  : Module_ID_Type;
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority) return Message_Record
   with
      Global => null;

   function Is_Publish_Result(Message : Message_Record) return Boolean is
      (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Publish_Result));

   procedure Publish_Result_Decode
      (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Publish_Result(Message),
      Depends => ((Channel, Message_Data, Size, Decode_Status) => Message);



end CubedOS.Publish_Subscribe.API;
