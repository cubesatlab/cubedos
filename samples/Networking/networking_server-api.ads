--------------------------------------------------------------------------------
-- FILE   : networking_server-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;
with System;

package Networking_Server.API is

   -- Used to indicate the success or failure of the requested operation.
   type Status_Type is (Success, Failure);

   type Message_Type is (Ping_Request, Ping_Reply);

   function Ping_Request_Encode
     (Sender_Domain   : Domain_ID_Type;
      Receiver_Domain : Domain_ID_Type;
      Sender          : Module_ID_Type;
      Request_ID      : Request_ID_Type;
      Priority        : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null;

   function Ping_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Status     : Status_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null;

   function Is_Ping_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(Ping_Request));

   function Is_Ping_Reply(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(Ping_Reply));

   procedure Ping_Request_Decode
     (Message : in Message_Record;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Ping_Request(Message),
       Depends => (Decode_Status => null, null => Message);
       -- Depends => (Decode_Status => Message);
       -- In this case, decoding can't fail because the message has no parameters.

   procedure Ping_Reply_Decode
     (Message : in Message_Record;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Ping_Reply(Message),
       Depends => (Status => null, Decode_Status => Message);
       -- Depends => ((Status, Decode_Status) => Message);
       -- In this case, Status is always "Success"; pinging the server never fails.

end Networking_Server.API;
