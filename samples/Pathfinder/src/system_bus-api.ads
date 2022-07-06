--------------------------------------------------------------------------------
-- FILE   : system_bus-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be
-- simultaneously called from multiple tasks. If possible, make every
-- subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager; use Message_Manager;
with System;

package System_Bus.API is

   type Status_Type  is (Success, Failure);
   type Message_Type is (Random_Number_Request, Random_Number_Reply, Telemetry);

   -----------------
   -- Random Number
   -----------------
   function Is_Random_Number_Request
     (Message : Message_Record) return Boolean is
     (Message.Receiver = ID and
      Message.Message_ID = Message_Type'Pos (Random_Number_Request));

   function Is_Random_Number_Reply (Message : Message_Record) return Boolean is
     (Message.Sender = ID and
      Message.Message_ID = Message_Type'Pos (Random_Number_Reply));

   function Random_Number_Request_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record with
      Global => null;

   function Random_Number_Reply_Encode
     (Receiver_Domain : Domain_ID_Type; Receiver : Module_ID_Type;
      Request_ID : Request_ID_Type; Status : Status_Type := Success;
      Priority : System.Priority := Pri)
      return Message_Record with
      Global => null;

   procedure Random_Number_Reply_Decode
     (Message : in     Message_Record; Decode_Status : out Message_Status_Type;
      Value   :    out Positive) with
      Global  => null,
      Pre     => Is_Random_Number_Reply (Message),
      Depends => (Decode_Status => Message, Value => Message);

   -------------
   -- Telemetry
   -------------
   function Is_Telemetry (Message : Message_Record) return Boolean is
     (Message.Receiver = ID and
      Message.Message_ID = Message_Type'Pos (Telemetry));

   function Telemetry_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record with
      Global => null;
end System_Bus.API;
