--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-api.ads
-- SUBJECT: Specification of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;
-- All CubedOS domains should instantiate Generic_Message_Manager using the name
-- Message_Manager. A distributed application composed of multiple domains must put each domain
-- into a separate executable with its own Message_Manager package. The common name is required
-- because that name is (currently) hard-coded in the CubedOS core modules as such.
--   
-- The message manager package contains the Mailboxes array that is used by CubedOS for inter-
-- module communication. 
with Message_Manager;  use Message_Manager;     
     
-- The Name_Resolver is a package created by the application developer that declares constant
-- Message_Address values mapping module names to their (domain ID, module ID) pairs. The name
-- 'Name_Resolver' must be used.     
with Name_Resolver;

package Sample_Module.API is
   
   -- Define data types here that are only needed in this API (such as status codes). Data
   -- types that might also be used in the implementation should be in the top level package.
   -- Note that status codes here are *different* from the status codes used by the message
   -- passing system itself (those are defined in Generic_Message_Manager).
   type Status_Type is (Success, Failure);
   
   -- Types defined here are intended to be of interest to the users of the module. We suggest
   -- creating an enumeration Message_Type that defines the different kinds of messages that can
   -- be received ("requests") or sent ("replies") by this module. Although users of the module
   -- don't need to know this, the position numbers of the enumerators here are used as "message
   -- ID" values in the header of a message.
   --
   -- Note that not all requests necessarily need replies. In effect, the reply message is the
   -- return value of the API function defined by the request message; API procedures that
   -- return nothing don't have corresponding reply messages.
   -- 
   type Message_Type is
     (A_Request,  -- Message requesting service.
      A_Reply,    -- Result of previous request (success/failure, or returned data).
      B_Request,  -- etc...
      B_Reply,
      C_Request,
      C_Reply);

   -- The encoding functions should be given names that reflect the operation but, by
   -- convention, should have a suffix of "Encode." Requests that are sent to the module, take a
   -- sender address as a (domain ID, module ID) pair, a request ID selected by the sender, a
   -- message priority, and whatever other parameters are appropriate for the message (not shown
   -- below). Replies sent from the module take a receiver address as a (domain ID, module ID)
   -- pair, a request ID that was provided by the sender (or zero), a message priority, and
   -- whatever other parameters are appropriate for the message (not shown below). Clients of a
   -- module do not need to be concerned with exactly how messages to the module or from the
   -- module are encoded.
   --
   -- Note that these functions do *not* send the messages, they only construct them. The caller
   -- must explicity send. This design reduces coupling and also gives the caller flexibilty to
   -- pre-construct messages and send them later or de-construct received messages at an earlier
   -- time.
   --
   -- The request ID asssociated with a request is an arbitrary value assigned by the sender of
   -- a request. Modules are encouraged (although not compelled---this should be documented by
   -- the module author) to echo the request ID in the corresponding reply or replies. Sending
   -- modules can use the request ID in the replies to corrolate replies to requests in an
   -- environment where multiple outstanding requests are possible. The request ID of zero is
   -- reserved as a placeholder and can be used when this feature is not useful.
   --
   function A_Request_Encode
     (Sender_Address : Message_Address;
      Request_ID : Request_ID_Type;     
      -- This is where appropriate IN parameters are defined for additional items.
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   with Global => null;
   
   function A_Reply_Encode
     (Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Status     : Status_Type; 
      -- This is where appropriate IN parameters are defined for additional items.      
      -- Not all replies necessarily need to return a Status value. However, that is common.
      Priority   : System.Priority := System.Default_Priority) return Message_Record
   with Global => null;
   
   
   -- The following functions return True of the given message is of the indicated type. They
   -- simplify the expression of preconditions on the decoding subprograms. They also hide the
   -- details of how messages are distinguished (the business about the position in the
   -- Message_Type enumeration).
   --
   function Is_A_Request(Message : Message_Record) return Boolean is
     (Message.Receiver_Address = Name_Resolver.Sample_Module and Message.Message_ID = Message_Type'Pos(A_Request));
   
   function Is_A_Reply(Message : Message_Record) return Boolean is
     (Message.Sender_Address = Name_Resolver.Sample_Module and Message.Message_ID = Message_Type'Pos(A_Reply));
   
   
   -- The decoding procedures take a message of the appropriate type and then decodes its
   -- payload, writing the results into various out parameters (not shown below). The
   -- Decode_Status parameter indicates only if the message is malformed or not. It uses a
   -- Message_Status_Type defined in the Generic_Message_Manager package. Errors in the handling
   -- of the message itself can be included in a reply message as a "normal" message parameter
   -- of an API-defined status type (or in some other way). Despite the precondition it is still
   -- possible for a message to be malformed; the precondition only checks the message header
   -- and not the details of the message format.
   --
   procedure A_Request_Decode
     (Message : in  Message_Record;     
      -- This is were appropriate OUT parameters are defined for the decided items.
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_A_Request(Message),
     Depends => (Decode_Status => Message);

   procedure A_Reply_Decode
     (Message : in  Message_Record;     
      -- This is where appropriate OUT parameters are defined for the decoded items.
      Decode_Status : out Message_Status_Type)
   with
     Global => null,
     Pre => Is_A_Reply(Message),
     Depends => (Decode_Status => Message);

end Sample_Module.API;
