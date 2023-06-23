--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-api.ads
-- SUBJECT: Specification of a package that defines the Time_Server API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;

with Name_Resolver;
with CubedOS.Lib;
with Message_Manager;  use Message_Manager;
with System;

package CubedOS.Time_Server.API
with Initial_Condition =>
  (Receives(This_Module, Cancel_Request_Msg)
   and Receives(This_Module, Relative_Request_Msg)
   and Receives(This_Module, Absolute_Request_Msg))
is

   This_Module : constant Module_ID_Type := Name_Resolver.Time_Server.Module_ID;
   type Message_Type is
      (Cancel_Request, 
      Tick_Reply, 
      Relative_Request, 
      Absolute_Request);

   Cancel_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Cancel_Request));
   Tick_Reply_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Tick_Reply));
   Relative_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Relative_Request));
   Absolute_Request_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos(Absolute_Request));
   type Series_Type is
      (One_Shot, 
      Periodic);

   type Series_ID_Type is new Lib.Quadruple_Octet;

   type Series_Count_Type is new Lib.Quadruple_Octet;

   procedure Relative_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Interval : Ada.Real_Time.Time_Span;
      Request_Type : Series_Type;
      Series_ID : Series_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (Proof_In => Mailbox_Metadata),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address, Relative_Request_Msg),
      Post => Message_Manager.Message_Type(Result) = Relative_Request_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Relative_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Interval : Ada.Real_Time.Time_Span;
      Request_Type : Series_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes, Proof_In => Mailbox_Metadata),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address, Relative_Request_Msg)
      ;

   function Is_Relative_Request(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Relative_Request_Msg);
   procedure Relative_Request_Decode
      (Message : in  Message_Record;
      Tick_Interval : out Ada.Real_Time.Time_Span;
      Request_Type : out Series_Type;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Relative_Request(Message) and Payload(Message) /= null,
      Depends => ((Tick_Interval, Request_Type, Series_ID, Decode_Status) => Message);


   procedure Absolute_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Time : Ada.Real_Time.Time;
      Series_ID : Series_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (Proof_In => Mailbox_Metadata),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address, Absolute_Request_Msg),
      Post => Message_Manager.Message_Type(Result) = Relative_Request_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Absolute_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Tick_Time : Ada.Real_Time.Time;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes, Proof_In => Mailbox_Metadata),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address, Absolute_Request_Msg)
      ;

   function Is_Absolute_Request(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Absolute_Request_Msg);
   procedure Absolute_Request_Decode
      (Message : in  Message_Record;
      Tick_Time : out Ada.Real_Time.Time;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Absolute_Request(Message) and Payload(Message) /= null,
      Depends => ((Tick_Time, Series_ID, Decode_Status) => Message);


   procedure Tick_Reply_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Count : Series_Count_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (Proof_In => Mailbox_Metadata),
      Pre => true
         and then Sender_Address.Module_ID = This_Module
         and then Receives(Receiver_Address, Tick_Reply_Msg),
      Post => Message_Manager.Message_Type(Result) = Relative_Request_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Tick_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Count : Series_Count_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes, Proof_In => Mailbox_Metadata),
      Pre => true
         and then Address(Sender).Module_ID = This_Module
         and then Receives(Receiver_Address, Tick_Reply_Msg)
      ;

   function Is_Tick_Reply(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Tick_Reply_Msg);
   procedure Tick_Reply_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Count : out Series_Count_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Tick_Reply(Message) and Payload(Message) /= null,
      Depends => ((Series_ID, Count, Decode_Status) => Message);


   procedure Cancel_Request_Encode
      (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Result : out Message_Record;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (Proof_In => Mailbox_Metadata),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address, Cancel_Request_Msg),
      Post => Message_Manager.Message_Type(Result) = Relative_Request_Msg
         and Message_Manager.Receiver_Address(Result) = Receiver_Address;

   procedure Send_Cancel_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Series_ID : Series_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   with
      Global => (In_Out => Mailboxes, Proof_In => Mailbox_Metadata),
      Pre => true
         and then Receiver_Address.Module_ID = This_Module
         and then Receives(Receiver_Address, Cancel_Request_Msg)
      ;

   function Is_Cancel_Request(Message : Message_Record) return Boolean is
      (Message_Manager.Message_Type(Message) = Cancel_Request_Msg);
   procedure Cancel_Request_Decode
      (Message : in  Message_Record;
      Series_ID : out Series_ID_Type;
      Decode_Status : out Message_Status_Type)
   with
      Global => null,
      Pre => Is_Cancel_Request(Message) and Payload(Message) /= null,
      Depends => ((Series_ID, Decode_Status) => Message);



end CubedOS.Time_Server.API;
