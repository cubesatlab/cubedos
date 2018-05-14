--------------------------------------------------------------------------------
-- FILE   : spi-api.ads
-- SUBJECT: Specification of a package that simplifies use of the SPI driver module.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;

package SPI.API is
   
   -- Types defined here are intended to be of interest to the users of the module. By
   -- convention we suggest creating an enumeration Message_Type that defines the message kinds
   -- that can be received by this module. A complementary enumeration type for reply messages
   -- sent by this module can also be defined here. Although users of the module don't need to
   -- know this, the position numbers of the enumerators here should be used as the first value
   -- in the message and used to distinguish one message kind from another.
   --
   type Message_Type is (A_Message, B_Message, C_Message);
   type Reply_Message_Type is (A_Reply, B_Reply, C_Reply);

   -- The operation functions should be given names that reflect the operation but, by
   -- convention, should have a suffix of "Message." They take a sender module ID and whatever
   -- other parameters are appropriate for the operation (not shown below) and they return a
   -- message encoded in some suitable way. Clients of a module do not need to be concerned with
   -- exactly how messages to that module are encoded. Note that these functions do *not* send
   -- the message, they only construct it. The caller must explicity send the message.
   --
   -- Although only one operation function is shown here, in a real module there would likely be
   -- many operations possible. These functions should all be pure.
   function Operation_Message(Sender : Module_ID) return Message_Record
     with Global => null;
   
   -- For operations that have replies this function takes a message and checks its format to
   -- see if it is a reply message from the specified operation. It is used by receivers of the
   -- reply to distinguish one received message from another.
   --
   function Is_Operation_Reply(Message : Message_Record) return Boolean
     with Global => null;
   
   -- For operations that have replies this procedure deconstructs the reply and returns, via
   -- appropriate out parameters (not shown below), whatever values are in the reply. The Status
   -- parameter indicates only if the message is malformed or not. Despite the precondition it
   -- is still possible for a message to be malformed as the Is_Operation_Reply function only
   -- checks that the message is a reply for a particular operation and not the details of the
   -- message format. Is_Operation_Reply is intended to be fast and fully analyzing a message
   -- will likely require decoding and checking the whole message which might be time consuming.
   --
   procedure Operation_Reply
     (Message : in  Message_Record;
      Status  : out Message_Status_Type)
     with
       Global => null,
       Pre => Is_Operation_Reply(Message),
       Depends => (Status => Message);

end SPI.API;
