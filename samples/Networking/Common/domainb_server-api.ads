--------------------------------------------------------------------------------
-- FILE   : domainb_server-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;
with System;
with Name_Resolver;

package DomainB_Server.API is

   -- Used to indicate the success or failure of the requested operation.
   type Status_Type is (Success, Failure);

   type Message_Type is (Ping_Request, Ping_Reply);

   function Ping_Request_Encode
     (Sender_Address : in Message_Address;
      Request_ID     : in Request_ID_Type;
      Priority       : in System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null;

   function Ping_Reply_Encode
     (Receiver_Address : in Message_Address;
      Request_ID       : in Request_ID_Type;
      Status           : in Status_Type;
      Priority         : in System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null;

   function Is_Ping_Request(Message : in Message_Record) return Boolean is
     (Message.Receiver_Address = Name_Resolver.DomainB_Server and Message.Message_ID = Message_Type'Pos(Ping_Request));

   function Is_Ping_Reply(Message : in Message_Record) return Boolean is
     (Message.Sender_Address = Name_Resolver.DomainB_Server and Message.Message_ID = Message_Type'Pos(Ping_Reply));

   procedure Ping_Request_Decode
     (Message : in Message_Record;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Ping_Request(Message),
       Depends => (Decode_Status => null, null => Message);
   -- In this case, decoding can't fail because the message has no parameters.

   procedure Ping_Reply_Decode
     (Message : in Message_Record;
      Status  : out Status_Type;
      Decode_Status : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Ping_Reply(Message),
       Depends => (Status => null, Decode_Status => Message);
   -- In this case, Status is always "Success"; pinging the server never fails.

end DomainB_Server.API;
