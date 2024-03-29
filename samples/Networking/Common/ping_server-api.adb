--------------------------------------------------------------------------------
-- FILE   : ping_server-api.adb
-- SUBJECT: Body of a package that implements the Ping_Server API
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package are task safe.
--
-- THIS FILE WAS GENERATED BY Merc. DO NOT EDIT!!
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Warnings(Off);

with CubedOS.Lib.XDR;
with CubedOS.Lib;
use  CubedOS.Lib;
use  CubedOS.Lib.XDR;
with CubedOS.Message_Types.Mutable; use CubedOS.Message_Types.Mutable;

package body Ping_Server.API is

   procedure Free is new Ada.Unchecked_Deallocation(String, String_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation(Octet_Array, Octet_Array_Ptr);
   procedure Ping_Request_Encode
      (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array(Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Ping_Request_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Ping_Request_Encode;
   
   procedure Send_Ping_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Status : out Status_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Request_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message, Status);
   end Send_Ping_Request;
   
   procedure Send_Ping_Request
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Request_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Ping_Request;
   
   procedure Send_Ping_Request
      (Sender : Module_Mailbox;
      Receiving_Module : Module_Metadata;
      Request_ID : Request_ID_Type;
      Status : out Status_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Request_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => (Domain_ID, Receiving_Module.Module_ID),
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message, Receiving_Module, This_Domain, Status);
   end Send_Ping_Request;
   
   procedure Send_Ping_Request
      (Sender : Module_Mailbox;
      Receiving_Module : Module_Metadata;
      Request_ID : Request_ID_Type;
      Receiving_Domain : Domain_Metadata := This_Domain;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
      Status : Status_Type := Unavailable;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Request_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => (Receiving_Domain.ID, Receiving_Module.Module_ID),
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message, Receiving_Module, Receiving_Domain, Status);
      pragma Unused(Status);
   end Send_Ping_Request;
   
   
   procedure Ping_Reply_Encode
      (Receiver_Address : in Message_Address;
      Sender_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Priority : in System.Priority := System.Default_Priority;
      Result : out  Message_Record)
   is
      subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;
      Position   : Data_Index_Type;
      Last       : Data_Index_Type;
      subtype Definite_Data_Array is Data_Array(Data_Index_Type);
      Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);
      Message : Mutable_Message_Record;
   begin
      Position := 0;
      Make_Empty_Message (
         Sender_Address   => Sender_Address,
         Receiver_Address => Receiver_Address,
         Request_ID   => Request_ID,
         Message_Type => Ping_Reply_Msg,
         Payload => Payload,
         Result => Message,
         Priority   => Priority);
      Result := Immutable(Message);
      Delete(Message);
      pragma Unused(Last, Payload, Position, Message);
   end Ping_Reply_Encode;
   
   procedure Send_Ping_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Status : out Status_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Reply_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message, Status);
   end Send_Ping_Reply;
   
   procedure Send_Ping_Reply
      (Sender : Module_Mailbox;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Reply_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => Receiver_Address,
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message);
   end Send_Ping_Reply;
   
   procedure Send_Ping_Reply
      (Sender : Module_Mailbox;
      Receiving_Module : Module_Metadata;
      Request_ID : Request_ID_Type;
      Status : out Status_Type;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Reply_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => (Domain_ID, Receiving_Module.Module_ID),
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message, Receiving_Module, This_Domain, Status);
   end Send_Ping_Reply;
   
   procedure Send_Ping_Reply
      (Sender : Module_Mailbox;
      Receiving_Module : Module_Metadata;
      Request_ID : Request_ID_Type;
      Receiving_Domain : Domain_Metadata := This_Domain;
      Priority : System.Priority := System.Default_Priority)
   is
      Message : Message_Record;
      Status : Status_Type := Unavailable;
   begin
      pragma Assert(Payload(Message) = null);
      Ping_Reply_Encode(
         Sender_Address => (This_Domain.ID, Module_ID(Sender)),
         Receiver_Address => (Receiving_Domain.ID, Receiving_Module.Module_ID),
         Request_ID => Request_ID,
         Result => Message,
         Priority => Priority);
      Message_Manager.Send_Message(Sender, Message, Receiving_Module, Receiving_Domain, Status);
      pragma Unused(Status);
   end Send_Ping_Reply;
   
   

end Ping_Server.API;
