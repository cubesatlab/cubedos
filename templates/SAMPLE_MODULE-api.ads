--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;
with System;

package Sample_Module.API is
   
   -- Define data types here that are only needed in this API (such as status codes). Data
   -- types that might also be used in the implementation should be in the top level package.
   -- Note that status codes here are *different* from the status codes used by the message
   -- passing system itself (those are defined in the message manager package).
   type Status_Type is (Success, Failure);
   
   -- Types defined here are intended to be of interest to the users of the module. By
   -- convention we suggest creating an enumeration Message_Type that defines the different
   -- kinds of messages that can be received or sent by this module. Although users of the
   -- module don't need to know this, the position numbers of the enumerators here should be
   -- used as "message ID" values in the header of a message.
   --
   -- Note that not all requests necessarily need replies. In effect, the reply message
   -- is the return value of the API function defined by the request message; procedures
   -- that return nothing don't have corresponding reply messages.
   --
   type Message_Type is
     (A_Request,  -- Message requesting service.
      A_Reply,    -- Result of previous request (success/failure, or returned data).
      B_Request,  -- etc...
      B_Reply,
      C_Request,
      C_Reply);

   -- The encoding functions should be given names that reflect the operation but, by
   -- convention, should have a suffix of "Encode." Messages that are sent to the module,
   -- called "requests," take a sender domain and module ID, a request ID selected by the
   -- sender, a message priority, and whatever other parameters are appropriate for the message
   -- (not shown below). Messages sent from the module, called "replies," take a receiver
   -- module ID, a request ID that was provided by the sender (or zero), a message priority,
   -- and whatever other parameters are appropriate for the message (not shown below). Clients
   -- of a module do not need to be concerned with exactly how messages to the module or from
   -- the module are encoded.
   --
   -- Note that these functions do *not* send the messages, they only construct them. The caller
   -- must explicity send. This design reduces coupling and also gives the caller flexibilty to
   -- pre-construct messages and send them later or de-construct received messages at a latter
   -- time.
   --
   -- The request ID asssociated with a request is an arbitrary value assigned by the sender of
   -- a request. Modules are encouraged (although not compelled---this should be documented by
   -- the module author) to echo the request ID in the corresponding reply or replies. Sending
   -- modules can use the request ID in the replies to corrolate replies to requests in an
   -- environment where multiple outstanding requests are possible. The request ID of zero is
   -- reserved as a placeholder and can be used when this feature is not meaningful or useful.
   --
   function A_Request_Encode
     (Sender_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   with Global => null;
   
   function A_Reply_Encode
     (Receiver_Domain : Domain_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Status     : Status_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   with Global => null;
   
   
   -- The following functions return True of the given message is of the indicated type. They
   -- simplify the expression of preconditions on the decoding subprograms. They also hide the
   -- details of how messages are distinguished (the business about the position in the
   -- Message_Type enumeration).
   --
   function Is_A_Request(Message : Message_Record) return Boolean is
     (Message.Receiver = ID and Message.Message_ID = Message_Type'Pos(A_Request));
   
   function Is_A_Reply(Message : Message_Record) return Boolean is
     (Message.Sender = ID and Message.Message_ID = Message_Type'Pos(A_Reply));
   
   
   -- The decoding procedures take a message of the appropriate type and then decode its
   -- payload, writing the results into various out parameters (not shown below). The
   -- Decode_Status parameter indicates only if the message is malformed or not. It uses a
   -- Message_Status_Type defined in the message manager package. Errors in the handling of the
   -- message itself can be included in a reply message as a "normal" message parameter of an
   -- API-defined status type (or in some other way). Despite the precondition it is still
   -- possible for a message to be malformed; the precondition only checks the message header
   -- and not the details of the message format.
   --
   procedure A_Request_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_A_Request(Message),
     Depends => (Decode_Status => Message);

   procedure A_Reply_Decode
     (Message : in  Message_Record;
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_A_Reply(Message),
     Depends => (Decode_Status => Message);

end Sample_Module.API;
