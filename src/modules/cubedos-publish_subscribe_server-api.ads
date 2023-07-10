--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server-api.ads
-- SUBJECT: Specification of a package that defines the publish/subscribe API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with Name_Resolver;
with System;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package CubedOS.Publish_Subscribe_Server.API is

   This_Module : constant Module_ID_Type := Name_Resolver.Publish_Subscribe_Server;

   type Status_Type is (Success, Failure);
   type Channel_ID_Type is range 1 .. 16;

   type Message_Type is
     (Subscribe_Request,   -- Ask to subscribe to a channel.
      Subscribe_Reply,     -- Success/failure of subscription request.
      Unsubscribe_Request, -- Ask to unsubscribe from a channel.
      Unsubscribe_Reply,   -- Success/failure of unsubscription request.
      Publish_Request,     -- Ask to publish to a channel.
      Publish_Reply,       -- Success/failure of a publish request.
      Publish_Result);     -- Delivery of data published to a channel.

   Unsubscribe_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Unsubscribe_Request));
   Unsubscribe_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Unsubscribe_Reply));
   Subscribe_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Subscribe_Request));
   Publish_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Publish_Request));
   Publish_Result_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Publish_Result));
   Publish_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Publish_Reply));
   Subscribe_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Subscribe_Reply));

   This_Receives: aliased constant Message_Type_Array := (Unsubscribe_Request_Msg,
                                             Subscribe_Request_Msg,
                                                          Publish_Request_Msg);

   Mail_Target : aliased constant Module_Metadata := Declare_Receives(This_Module, This_Receives'Access);

   procedure Subscribe_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
       Pre => true
       and then Receiver_Address.Module_ID = This_Module
       and then Receives(Receiver_Address.Module_ID, Subscribe_Request_Msg),
       Post => Message_Types.Message_Type(Result) = Subscribe_Request_Msg
       and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Subscribe_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Subscribe_Request_Msg)
      ;

   procedure Subscribe_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Subscribe_Reply_Msg),
      Post => Message_Types.Message_Type(Result) = Subscribe_Reply_Msg
         and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Subscribe_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiver_Address.Module_ID, Subscribe_Reply_Msg)
      ;

   procedure Unsubscribe_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Unsubscribe_Request_Msg),
      Post => Message_Types.Message_Type(Result) = Unsubscribe_Request_Msg
         and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Unsubscribe_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Unsubscribe_Request_Msg)
      ;

   procedure Unsubscribe_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Unsubscribe_Reply_Msg),
      Post => Message_Types.Message_Type(Result) = Unsubscribe_Reply_Msg
         and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Unsubscribe_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiver_Address.Module_ID, Unsubscribe_Reply_Msg)
      ;

   procedure Publish_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Publish_Reply_Msg),
      Post => Message_Types.Message_Type(Result) = Publish_Reply_Msg
         and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Publish_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Status : Status_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiver_Address.Module_ID, Publish_Reply_Msg)
      ;

   procedure Publish_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => Message_Data'Length <= Data_Size_Type'Last - 8
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Publish_Request_Msg),
      Post => Message_Types.Message_Type(Result) = Publish_Request_Msg
       and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Publish_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => Message_Data'Length <= Data_Size_Type'Last - 8
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Publish_Request_Msg)
      ;

   procedure Publish_Result_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Data : CubedOS.Lib.Octet_Array;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Pre => true
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address.Module_ID, Publish_Result_Msg),
      Post => Message_Types.Message_Type(Result) = Publish_Result_Msg
         and Message_Types.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Publish_Result
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Channel : Channel_ID_Type;
      Data : CubedOS.Lib.Octet_Array;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes),
      Pre => true
         and then Module_ID(Sender) = This_Module
         and then Receives(Receiver_Address.Module_ID, Publish_Result_Msg)
      ;

   function Is_Subscribe_Request(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Subscribe_Request_Msg);

   function Is_Subscribe_Reply(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Subscribe_Reply_Msg);

   function Is_Unsubscribe_Request(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Unsubscribe_Request_Msg);

   function Is_Unsubscribe_Reply(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Unsubscribe_Reply_Msg);

   function Is_Publish_Request(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Publish_Request_Msg);

   function Is_Publish_Reply(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Publish_Reply_Msg);

   function Is_Publish_Result(Message : Message_Record) return Boolean is
      (Message_Types.Message_Type(Message) = Publish_Result_Msg);


   procedure Subscribe_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Depends => ((Channel, Decode_Status) => Message),
     Pre => Is_Subscribe_Request(Message) and Payload(Message) /= null;

   procedure Subscribe_Reply_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Depends => ((Channel, Status, Decode_Status) => Message),
     Pre => Is_Subscribe_Reply(Message) and Payload(Message) /= null;

   procedure Unsubscribe_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Depends => ((Channel, Decode_Status) => Message),
     Pre => Is_Unsubscribe_Request(Message) and Payload(Message) /= null;

   procedure Unsubscribe_Reply_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Depends => ((Channel, Status, Decode_Status) => Message),
     Pre => Is_Unsubscribe_Reply(Message) and Payload(Message) /= null;

   procedure Publish_Request_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size    : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Depends =>
       (Channel => Message,
        Size    => (Message, Message_Data),
        (Message_Data, Decode_Status) => (Message, Message_Data)),
     Pre => Is_Publish_Request(Message) and Payload(Message) /= null,
     Post => Size <= Message_Data'Length;

   procedure Publish_Reply_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Depends => ((Channel, Status, Decode_Status) => Message),
     Pre => Is_Publish_Reply(Message) and Payload(Message) /= null;

   procedure Publish_Result_Decode
     (Message : in  Message_Record;
      Channel : out Channel_ID_Type;
      Message_Data : out CubedOS.Lib.Octet_Array;
      Size    : out CubedOS.Lib.Octet_Array_Count;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Depends =>
       (Channel => Message,
        Size => (Message, Message_Data),
        (Message_Data, Decode_Status) => (Message, Message_Data)),
     Pre => Is_Publish_Result(Message) and Payload(Message) /= null,
     Post => Size <= Message_Data'Length;

end CubedOS.Publish_Subscribe_Server.API;
