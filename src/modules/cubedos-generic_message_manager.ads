--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.ads
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- This package defines the core CubedOS messaging system.
-- This package is instantiated once for each domain in a distributed CubedOS system.
-- It handles message passing within the domain and routes messages into other domains.
-- Every domain must have a unique ID as decided upon by administrative coordination.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Message_Types; use CubedOS.Message_Types;
with CubedOS.Message_Debuggers; use CubedOS.Message_Debuggers;

generic
   -- The domain of this message manager.
   Domain : Domain_Metadata;
   -- A debugger may be injected to provide information
   -- about message passing.
   Debugger : Message_Debugger'Class := Null_Message_Debugger_Object;
package CubedOS.Generic_Message_Manager
  with
Abstract_State =>
  ((Mailboxes with External),
   (Lock with External),
   (Request_ID_Generator with External)),
  Initializes => (Mailboxes, Request_ID_Generator, Lock),
  Initial_Condition => (for all ID of This_Domain.Module_IDs => not Module_Registered(ID))
is
   This_Domain : constant Domain_Metadata := Domain;
   -- The ID of this domain.
   Domain_ID : constant Domain_ID_Type := This_Domain.ID;
   -- The number of modules in this domain.
   Module_Count : constant Positive := This_Domain.Module_Count;

   -------------------
   -- Initialization
   -------------------

   -- At runtime, no messages are allowed to be sent until
   -- every module in this domain has registered its mailbox.
   function Messaging_Ready return Boolean
     with Global => null,
     Depends => (Messaging_Ready'Result => null);


   -- This function blocks until all modules in the domain have
   -- intitialized their mailbox and messaging can be done safetly.
   procedure Wait
     with Post => Messaging_Ready;


   --------------------------
   -- Request ID Generation
   --------------------------

   -- Returns an arbitrary, domain-unique request ID. Probably these IDs should also be unique
   -- across domains, but that is not yet implemented.
   procedure Get_Next_Request_ID(Request_ID : out Request_ID_Type)
     with Global => (In_Out => Request_ID_Generator);


   -------------------
   -- Mailboxes
   -------------------

   -- An endpoint associated with exactly one module used
   -- to send and receive messages. It also stores metadata
   -- about the module used to prove aspects about message safety.
   -- Modules should keep their instance of this object hidden.
   type Module_Mailbox is private;

   function Make_Module_Mailbox(ID : in Module_ID_Type;
                                Spec : Module_Metadata) return Module_Mailbox
     with
       Pre => ID = Spec.Module_ID,
       Post => Module_ID(Make_Module_Mailbox'Result) = ID
       and (for all T of Spec.Receive_Types.all => Receives(Make_Module_Mailbox'Result, T));

   -- Checks if the given module id has already been registered.
   function Module_Registered(Module_ID : in Module_ID_Type) return Boolean
     with Ghost, Global => null;

   -- Registers a module with the messaging system.
   -- Modules must specify the maximum number of messages their
   -- mailbox can store. The mailbox is registered to the domain
   -- of this Message_Manager.
   procedure Register_Module(Mailbox : in Module_Mailbox;
                             Msg_Queue_Size : in Positive)
     with Global => (In_Out => (Mailboxes, Lock)),
     Depends => (Mailboxes => +(Mailbox, Msg_Queue_Size),
                 Lock => +Mailbox),
     Pre => Has_Module(This_Domain, Module_ID(Mailbox))
     and not Module_Registered(Module_ID(Mailbox))
     and Msg_Queue_Size < Natural'Last - 1,
     Post => Module_Registered(Module_ID(Mailbox));


   function Spec(Box : Module_Mailbox) return Module_Metadata
     with Post => Spec'Result.Receive_Types /= null;
   function Valid(Box : Module_Mailbox) return Boolean;
   function Module_ID(Box : Module_Mailbox) return Module_ID_Type;

   -- Checks if the given receiver may legally receive the given message type.
   function Receives(Receiver : Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost,
     Depends => (Receives'Result => (Receiver, Msg_Type));

   -- Get the number of messages in the mailbox waiting
   -- to be read.
   procedure Pending_Messages(Box : Module_Mailbox; Size : out Natural)
     with Global => (In_Out => Mailboxes),
       Pre => Has_Module(This_Domain, Module_ID(Box));

   -- A blocking function which will retreive messages sent to the given mailbox.
   -- The message will be of a type that the mailbox is allowed to receive.
   procedure Read_Next(Box : Module_Mailbox; Msg : out Message_Record)
     with Pre => Messaging_Ready and Has_Module(This_Domain, Module_ID(Box)),
     Post => Payload(Msg) /= null
     and Receives(Spec(Box), Message_Type(Msg));

   -- The result status of a message send operation.
   -- Accepted: The message was deposited in the destination mailbox successfully.
   -- Mailbox_Full: The message was discarded because the destination didn't have
   --               space to store it.
   -- Rejected_Type: The message was discarded because the destination couldn't
   --                receive messages of its type.
   -- Unavailable: The return status is unknown because the message sent to a
   --              foreign domain.
   type Status_Type is (Accepted, Mailbox_Full, Rejected_Type, Unavailable);

   -- Sends the given message to the specified module and domain. This
   -- procedure asserts that the destination module may receive the given
   -- message type and that the module does exist in the destination domain.
   -- It is the safest method of sending a message and should be prefered.
   -- The procedure returns immediately and moves the message's payload
   -- making it inaccessible after the call.
   --
   -- This procedure generally shouldn't be called directly and will be used
   -- in auto-generated API files.
   procedure Send_Message(Box : Module_Mailbox;
                          Msg : in out Message_Record;
                          Target_Module : Module_Metadata;
                          Target_Domain : Domain_Metadata := This_Domain;
                          Status : out Status_Type)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready
     and then Payload(Msg) /= null
     and then Receives(Target_Module, Message_Type(Msg))
     and then Has_Module(Target_Domain, Target_Module.Module_ID)
     and then Sender_Address(Msg) = (This_Domain.ID, Module_ID(Box))
     and then Receiver_Address(Msg) = (Target_Domain.ID, Target_Module.Module_ID),
     Post => Payload(Msg) = null;

   -- Sends the given message, guaranteeing only that the sending address is
   -- from the current domain and sending module.
   -- Returns immediately, moving the message's payload making it inaccessible
   -- after the call.
   --
   -- This procedure generally shouldn't be called directly and will be used
   -- in auto-generated API files.
   procedure Send_Message(Box : Module_Mailbox; Msg : in out Message_Record)
     with Global => (In_Out => Mailboxes),
     Depends => (Mailboxes => +(Msg),
                 Msg => Msg,
                 null => Box),
     Pre => Messaging_Ready
     and then Payload(Msg) /= null
     and then Sender_Address(Msg) = (This_Domain.ID, Module_ID(Box)),
     Post => Payload(Msg) = null;

   -- Sends the given message.
   -- Returns immediately with the status of the operation's result.
   --
   -- This procedure generally shouldn't be called directly and will be used
   -- in auto-generated API files.
   procedure Send_Message(Box : Module_Mailbox; Msg : in out Message_Record; Status : out Status_Type)
     with Global => (In_Out => Mailboxes),
     Depends => (Mailboxes => +(Msg),
                 Status => (Msg, Mailboxes),
                 Msg => Msg,
                 null => Box),
     Pre => Messaging_Ready
     and then Sender_Address(Msg) = (This_Domain.ID, Module_ID(Box))
     and then Receiver_Address(Msg).Domain_ID = Domain_ID
     and then Payload(Msg) /= null,
     Post => Payload(Msg) = null;


   --------------------------
   -- Interdomain Messaging
   --------------------------

   -- Submit a message received from a foreign domain to the message system.
   procedure Handle_Received(Msg : in out Msg_Owner)
     with Pre => Msg /= null
     and then Payload(Msg) /= null
     and then Messaging_Ready
     and then Has_Module(This_Domain, Receiver_Address(Msg).Module_ID),
     Post => Msg = null;


   -------------------
   -- Debug/Testing
   -------------------

   -- These are convenience functions for development
   -- and should not be used in production code.

   -- This procedure completes the initialization process
   -- immediately, unblocking the Wait function and
   -- allowing messages to be sent prematurely.
   --
   -- Doing this removes the guarantee that all
   -- modules are prepared to receive messages and can cause
   -- undefined behavior if a message is sent to one of these
   -- modules.
   --
   -- Useful for test configurations where not all modules
   -- are needed.
   procedure Skip_Mailbox_Initialization
     with Global => (In_Out => Lock),
     Post => Messaging_Ready;

   -- Sends a message without a mailbox. This is
   -- dangerous because it allows senders to lie about
   -- their address.
   procedure Route_Message(Message : in Message_Record)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready
     and then Payload(Message) /= null;

   procedure Route_Message(Message : in Message_Record; Status : out Status_Type)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready
     and then Payload(Message) /= null;

private
   type Module_Mailbox is
      record
         Module_ID : Module_ID_Type;
         Spec : Module_Metadata;
      end record;

   function Make_Module_Mailbox(ID : in Module_ID_Type;
                                Spec : Module_Metadata) return Module_Mailbox
     is (ID, Spec);

   function Spec(Box : Module_Mailbox) return Module_Metadata
     is (Box.Spec);
   function Valid(Box : Module_Mailbox) return Boolean
     is (Box.Spec.Receive_Types /= null);
   function Module_ID(Box : Module_Mailbox) return Module_ID_Type
     is (Box.Module_ID);

end CubedOS.Generic_Message_Manager;
