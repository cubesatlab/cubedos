--------------------------------------------------------------------------------
-- FILE   : echo_server-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;
with Name_Resolver;
with System;

package Echo_Server.API is

   -- Used to indicate the success or failure of the requested operation.
   type Status_Type is (Success, Failure);

   type Message_Type is (Ping_Request, Ping_Reply);

   function Ping_Request_Encode
     (Sender_Address : Message_Address;
      Request_ID    : Request_ID_Type;
      Priority      : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null;

   function Ping_Reply_Encode
     (Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Status     : Status_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null;

   function Is_Ping_Request(Message : Message_Record) return Boolean is
     (Message.Receiver_Address = Name_Resolver.Echo_Server and Message.Message_ID = Message_Type'Pos(Ping_Request));

   function Is_Ping_Reply(Message : Message_Record) return Boolean is
     (Message.Sender_Address = Name_Resolver.Echo_Server and Message.Message_ID = Message_Type'Pos(Ping_Reply));

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

end Echo_Server.API;
