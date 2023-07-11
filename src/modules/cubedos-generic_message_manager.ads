--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.ads
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
-- This package defines the core CubedOS mailbox system with associated types, etc. Each time
-- this package is instantiated a new "domain" is created. In a distributed CubedOS system,
-- every domain must have a unique ID as decided upon by administrative coordination.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Message_Types; use CubedOS.Message_Types;

generic
   This_Domain : Domain_Declaration;  -- The domain of this message manager.
   with procedure Send_Outgoing_Message(Msg : in out Msg_Owner);
package CubedOS.Generic_Message_Manager
  with
Abstract_State =>
  ((Mailboxes with External),
   (Lock with External),
   (Request_ID_Generator with External)),
  Initializes => (Mailboxes, Request_ID_Generator, Lock)
is

   Domain_ID : constant Domain_ID_Type := This_Domain.ID;
   Module_Count : constant Natural := This_Domain.Module_Count;

   Empty_Type_Array_Ptr : aliased constant Message_Type_Array := (0 => (1,1));

   -- True if the given receiving address can be sent the given message type.
   function Receives(Receiver : Module_ID_Type; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost;

   -- Returns an arbitrary, domain-unique request ID. Probably these IDs should also be unique
   -- across domains, but that is not yet implemented.
   procedure Get_Next_Request_ID(Request_ID : out Request_ID_Type)
     with Global => (In_Out => Request_ID_Generator);

   function Messaging_Ready return Boolean
     with Global => null;

   -- By default, Message Manager will not allow
   -- any messages to send until all modules declared
   -- in the domain declaration have been registered.
   -- This function may be used to skip the initialization
   -- process for any modules which haven't registered
   -- a mailbox.
   -- Doing this removes the gaurantee that all
   -- modules are prepared to receive messages and
   -- should be used only in development contexts.
   procedure Skip_Mailbox_Initialization
     with Global => (In_Out => Lock),
       Post => Messaging_Ready;

   -- Error codes.
   type Status_Type is (Accepted, Mailbox_Full);                          -- Mailbox access.
   type Message_Status_Type is (Success, Malformed, Insufficient_Space);  -- Message decoding.

   -- Retrieves a message from the indicated mailbox. May block indefinitely.
   procedure Fetch_Message(Module : in Module_ID_Type; Message : out Message_Record)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready,
     Post => Is_Valid(Message) and Payload(Message) /= null;

   -- A mailbox used by modules to send and receive messages.
   -- This is the only way to receive messages sent to a module
   -- and the only intended way to send messages from it.
   -- Modules should keep their instance of this object invisible
   -- to outside modules.
   type Module_Mailbox is private;

   function Spec(Box : Module_Mailbox) return Module_Metadata;
   function Valid(Box : Module_Mailbox) return Boolean;
   function Module_ID(Box : Module_Mailbox) return Module_ID_Type;

   function Receives(Receiver : access constant Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost,
     Pre => Receiver /= null,
     Depends => (Receives'Result => (Receiver, Msg_Type));

   function Receives(Receiver : Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost,
     Depends => (Receives'Result => (Receiver, Msg_Type));

   -- Sends the given message to the given address from this mailbox's address.
   -- Returns immediately. Moves the message's payload making it inaccessible
   -- after the call.
   procedure Send_Message(Box : Module_Mailbox;
                          Msg : in out Message_Record;
                          Target_Module : Module_Metadata;
                          Target_Domain : Domain_Declaration := This_Domain)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready
     and then Is_Valid(Msg)
     and then Receives(Target_Module, Message_Type(Msg))
     and then Has_Module(Target_Domain, Target_Module.Module_ID)
     and then Sender_Address(Msg) = (Domain_ID, Spec(Box).Module_ID)
     and then Receiver_Address(Msg) = (Target_Domain.ID, Target_Module.Module_ID),
     Post => Payload(Msg) = null;

   -- Sends the given message to the given address from this mailbox's address.
   -- Returns immediately. Moves the message's payload making it inaccessible
   -- after the call.
   procedure Send_Message(Box : Module_Mailbox; Msg : in out Message_Record)
     with Global => (In_Out => Mailboxes),
     Depends => (Mailboxes => +(Box, Msg),
                 Msg => Msg),
     Pre => Messaging_Ready
     and then Is_Valid(Msg)
     and then Sender_Address(Msg) = (Domain_ID, Spec(Box).Module_ID),
     Post => Payload(Msg) = null;

   -- Sends the given message to the given address from this mailbox's address.
   -- Returns immediately with the status of the operation's result.
   -- The destination domain must be this domain in order to get a status.
   -- If the destination domain is external, use a different Send_Message procedure.
   procedure Send_Message(Box : Module_Mailbox; Msg : in out Message_Record; Status : out Status_Type)
     with Global => (In_Out => Mailboxes),
     Depends => (Mailboxes => +(Box, Msg),
                 Status => (Box, Msg, Mailboxes),
                 Msg => Msg),
     Pre => Messaging_Ready
     and then Sender_Address(Msg) = (Domain_ID, Spec(Box).Module_ID)
     and then Receiver_Address(Msg).Domain_ID = Domain_ID;
     --and then Receives(Receiver_Address(Msg).Module_ID, Message_Type(Msg));

   -- Reads the next message, removing it from the message queue.
   -- Blocks until a message is available.
   procedure Read_Next(Box : Module_Mailbox; Msg : out Message_Record)
     with Pre => Messaging_Ready,
     Post => Is_Valid(Msg)
     and Payload(Msg) /= null
     and Receives(Spec(Box), Message_Type(Msg));

   -- Count the number of messages in the mailbox waiting
   -- to be read. This is a procedure and not a function because
   -- the mailboxes are volatile. Theoretically, it may be possible
   -- to make this a function, but the SPARK manual is
   -- beyond me on this subject.
   procedure Queue_Size(Box : Module_Mailbox; Size : out Natural)
     with Global => (In_Out => Mailboxes);

   function Make_Module_Mailbox(ID : in Module_ID_Type;
                                Spec : Module_Metadata) return Module_Mailbox
     with
       Post => Module_ID(Make_Module_Mailbox'Result) = ID
       and (for all T of Spec.Receive_Types.all => Receives(Make_Module_Mailbox'Result, T));

   -- Register a module with the mail system.
   -- Gets an observer for that module's mailbox.
   procedure Register_Module(Mailbox : in Module_Mailbox;
                             Msg_Queue_Size : in Positive)
     with Global => (In_Out => (Mailboxes, Lock)),
     Depends => (Mailboxes => +(Mailbox, Msg_Queue_Size),
                 Lock => +Mailbox),
     Pre => (for some M of This_Domain.Module_IDs => M = Module_ID(Mailbox));

   -- Gives a message received from a foreign domain to the message system.
   procedure Handle_Received(Msg : in out Msg_Owner);

   -- Definition of the array type used to hold messages in a mailbox. This needs to be here
   -- rather than in the body because Message_Count_Type is used below.
   subtype Message_Index_Type is Positive;
   subtype Message_Count_Type is Natural;
   type Message_Ptr_Array is array(Positive range <>) of Msg_Owner;

   type Message_Count_Array is array(Module_ID_Type) of Message_Count_Type;

   -- Returns a count of the number of messages in each mailbox. Note that the counts might
   -- not be a consistent snapshot since messages might be added/removed from mailboxes while
   -- the counts are being gathered. This procedure is intended to be used for software
   -- telemetry and debugging.
   procedure Get_Message_Counts(Count_Array : out Message_Count_Array)
     with Global => (In_Out => Mailboxes),
       Pre => Messaging_Ready;

   procedure Route_Message(Message : in Message_Record)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready
     --and then Receives(Receiver_Address(Message).Module_ID, Message_Type(Message))
     and then Is_Valid(Message);
   procedure Route_Message(Message : in Message_Record; Status : out Status_Type)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready
     --and then Receives(Receiver_Address(Message).Module_ID, Message_Type(Message))
     and then Is_Valid(Message);

   -- Blocks until all modules in the domain have intitialized
   -- their mailbox and are ready to receive messages.
   procedure Wait
     with Post => Messaging_Ready;

private

   type Module_Mailbox is
      record
         Module_ID : Module_ID_Type;
         Spec : Module_Metadata;
      end record;

   function Spec(Box : Module_Mailbox) return Module_Metadata
     is (Box.Spec);
   function Valid(Box : Module_Mailbox) return Boolean
   is (Box.Spec.Receive_Types /= null);


   procedure Route_Message(Message : in out Msg_Owner; Status : out Status_Type)
     with Global => (In_Out => (Mailboxes)),
     Pre => Message /= null
     and then Is_Valid(Message.all)
     and then Messaging_Ready,
     Post => Message = null;

   procedure Route_Message(Message : in out Msg_Owner)
     with Global => (In_Out => Mailboxes),
     Pre => Message /= null and then Is_Valid(Message.all)
     and then Messaging_Ready,
     Post => Message = null;

end CubedOS.Generic_Message_Manager;
