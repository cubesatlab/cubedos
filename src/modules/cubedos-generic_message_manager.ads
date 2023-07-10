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

with System;
with CubedOS.Lib.XDR;
use  CubedOS.Lib.XDR;
with CubedOS.Message_Types; use CubedOS.Message_Types;

generic
   This_Domain : Domain_Declaration;  -- The domain of this message manager.
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

   -- Definition of Request IDs. Normally requests are given unique ID values that are echoed in
   -- replies. This allows a module to associate a reply with a particular request. The request
   -- ID of zero i special; it is used in cases where no such request/reply matching is needed
   -- or sensible.
   type Request_ID_Type is mod 2**32;

   -- Note: A fully qualified message address is the triple (Domain_ID, Module_ID, Message_ID).

   -- Definition of a CubedOS message. Messages are stored in XDR_Arrays.

   -- Starting the index type at 0 is convenient when expressing "multiple of four" assertions.
   -- The 'extended' index type provides an extra value before the first allowed index. This is
   -- used by the XDR Octet_Array and String encoders so they can return a correct 'Last' when
   -- given zero length values to encode. Support for encoding zero length arrays and strings is
   -- useful.
   --
   subtype Data_Index_Type is XDR_Index_Type;
   subtype Data_Extended_Index_Type is XDR_Extended_Index_Type;
   subtype Data_Size_Type is XDR_Size_Type;
   subtype Data_Array is XDR_Array;
   type Data_Array_Owner is access Data_Array;

   -- TODO: All references to this value by API packages should be replaced
   -- with actual message sizes and this constant should be removed.
   Max_Message_Size : constant Positive := 1024;

   Empty_Type_Array_Ptr : aliased constant Message_Type_Array := (0 => (1,1));

   -- True if the given receiving address can be sent the given message type.
   function Receives(Receiver : Module_ID_Type; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost;

   -- Messages currently have a priority field that is not used. The intention is to allow high
   -- priority messages to be processed earlier and without interruption. SPARK does not support
   -- dynamic task priorities, however, so the usefulness of this idea is questionable. We could
   -- still sort mailboxes by message priority (not currently done) which might be a little
   -- useful.
   type Mutable_Message_Record is
      record
         Sender_Address : Message_Address;
         Receiver_Address : Message_Address;
         Request_ID : Request_ID_Type;
         Message_Type : Universal_Message_Type;
         Priority   : System.Priority;
         Payload    : Data_Array_Owner;
      end record;

   -- Frees any dynamically allocated memory the message references.
   procedure Delete(Msg : in out Mutable_Message_Record)
     with Pre => Msg.Payload /= null,
     Post => Msg.Payload = null;

   -- Immutible version of message record
   type Message_Record is private
     with Default_Initial_Condition => Payload(Message_Record) = null;
   type Msg_Owner is access Message_Record;

   function Is_Valid(Msg : Message_Record) return Boolean
     with Post => Is_Valid'Result = (Payload(Msg) /= null);

   -- Creates an immutible copy of the given message
   function Immutable(Msg : Mutable_Message_Record) return Message_Record
     with Pre => Msg.Payload /= null,
     Post => Is_Valid(Immutable'Result) and
     Message_Type(Immutable'Result) = Msg.Message_Type and
     Receiver_Address(Immutable'Result) = Msg.Receiver_Address and
     Sender_Address(Immutable'Result) = Msg.Sender_Address and
     Request_ID(Immutable'Result) = Msg.Request_ID and
     Priority(Immutable'Result) = Msg.Priority and
     Payload(Immutable'Result).all = Msg.Payload.all;

   function Sender_Address(Msg : Message_Record) return Message_Address;
   function Receiver_Address(Msg : Message_Record) return Message_Address;
   function Request_ID(Msg : Message_Record) return Request_ID_Type;
   function Message_Type(Msg : Message_Record) return Universal_Message_Type;
   function Priority(Msg : Message_Record) return System.Priority;
   function Payload(Msg : Message_Record) return access constant Data_Array;

   -- Adding duplicates for pointers cuts down on Ada's disgusting syntax
   function Sender_Address(Msg : not null access constant Message_Record) return Message_Address;
   function Receiver_Address(Msg : not null access constant Message_Record) return Message_Address;
   function Request_ID(Msg : not null access constant Message_Record) return Request_ID_Type;
   function Message_Type(Msg : not null access constant Message_Record) return Universal_Message_Type;
   function Priority(Msg : not null access constant Message_Record) return System.Priority;
   function Payload(Msg : not null access constant Message_Record) return access constant Data_Array;

   -- Frees any dynamically allocated memory the message references.
   procedure Delete(Msg : in out Message_Record)
     with Pre => Payload(Msg) /= null,
     Post => Payload(Msg) = null;

   -- Convenience constructor function for messages. This is used by encoding functions.
   function Make_Empty_Message
     (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Message_Type : Universal_Message_Type;
      Payload_Size : Natural;
      Priority   : System.Priority := System.Default_Priority) return Mutable_Message_Record
     with
       Global => null,
       Post=>
         Make_Empty_Message'Result.Sender_Address   = Sender_Address   and
         Make_Empty_Message'Result.Receiver_Address = Receiver_Address and
         Make_Empty_Message'Result.Request_ID = Request_ID and
         Make_Empty_Message'Result.Message_Type = Message_Type and
         Make_Empty_Message'Result.Payload /= null and
         Make_Empty_Message'Result.Payload'Length = Payload_Size and
         Make_Empty_Message'Result.Priority   = Priority;

   procedure Make_Empty_Message
     (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Message_Type : Universal_Message_Type;
      Payload : in out Data_Array_Owner;
      Priority   : System.Priority := System.Default_Priority;
      Result : out Mutable_Message_Record)
     with
       Global => null,
       Pre => Payload /= null,
       Post =>
         Result.Sender_Address   = Sender_Address   and
         Result.Receiver_Address = Receiver_Address and
         Result.Request_ID = Request_ID and
         Result.Message_Type = Message_Type and
         Result.Payload.all = Payload.all'Old and
         Result.Payload /= null and
         Payload = null and
         Result.Priority   = Priority;

   -- Convenience function to stringify messages
   function Stringify_Message (Message : in Message_Record) return String;

   -- Returns an arbitrary, domain-unique request ID. Probably these IDs should also be unique
   -- across domains, but that is not yet implemented.
   procedure Get_Next_Request_ID(Request_ID : out Request_ID_Type)
     with Global => (In_Out => Request_ID_Generator);

   -- True if the module is ready to receive mail.
   -- This refers to a module in the current domain.
   function Module_Ready(Module_ID : Module_ID_Type) return Boolean
     with Ghost,
     Depends => (Module_Ready'Result => null,
                 null => Module_ID);

   function Messaging_Ready return Boolean
     with Global => null;

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
   function Address(Box : Module_Mailbox) return Module_ID_Type;

   function Receives(Receiver : access constant Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost,
     Pre => Receiver /= null,
     Depends => (Receives'Result => (Receiver, Msg_Type));

   function Receives(Receiver : Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     with Ghost,
     Depends => (Receives'Result => (Receiver, Msg_Type));

   procedure Send_Message(Box : Module_Mailbox;
                          Msg : in out Message_Record;
                          Target_Module : Module_Metadata;
                          Target_Domain : Domain_Declaration := This_Domain)
     with Global => (In_Out => Mailboxes),
     Pre => Messaging_Ready
     and then Is_Valid(Msg)
     and then Receives(Target_Module, Message_Type(Msg))
     and then Has_Module(Target_Domain, Target_Module.Module_ID),
     Post => Payload(Msg) = null;

   -- Sends the given message to the given address from this mailbox's address.
   -- Returns immediately. Moves the message's payload making it inaccessible
   -- after the call.
   procedure Send_Message(Box : Module_Mailbox; Msg : in out Message_Record)
     with Global => (In_Out => Mailboxes),
     Depends => (Mailboxes => +(Box, Msg),
                 Msg => Msg),
     Pre => Messaging_Ready
     and then Is_Valid(Msg),
     Post => Payload(Msg) = null;

   -- Sends the given message to the given address from this mailbox's address.
   -- Returns immediately with the status of the operation's result.
   procedure Send_Message(Box : Module_Mailbox; Msg : in out Message_Record; Status : out Status_Type)
     with Global => (In_Out => Mailboxes),
     Depends => (Mailboxes => +(Box, Msg),
                 Status => (Box, Msg, Mailboxes),
                 Msg => Msg),
     Pre => Messaging_Ready;
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

   function Make_Module_Mailbox(Module_ID : in Module_ID_Type;
                                Spec : Module_Metadata) return Module_Mailbox
     with
       Post => Address(Make_Module_Mailbox'Result) = Module_ID
       and (for all T of Spec.Receive_Types.all => Receives(Make_Module_Mailbox'Result, T));

   -- Register a module with the mail system.
   -- Gets an observer for that module's mailbox.
   procedure Register_Module(Mailbox : in Module_Mailbox;
                             Msg_Queue_Size : in Positive)
     with Global => (In_Out => (Mailboxes, Lock)),
     Depends => (Mailboxes => +(Mailbox, Msg_Queue_Size),
                 Lock => +Mailbox),
     Pre => (for some M of This_Domain.Module_IDs => M = Address(Mailbox)),
     --Pre => not Module_Ready(Address(Mailbox).Module_ID),
     Post => Module_Ready(Address(Mailbox));

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


   -- Send the indicated message to the right mailbox. This might cross domains. This procedure
   -- returns at once with a status of Accepted if the message was definitely delivered. A status
   -- of Mailbox_Full indicates that delivery did not occur.
   --
   -- Depreciated: Use Mailboxes to send messages
   procedure Route_Message(Message : in out Msg_Owner; Status : out Status_Type)
     with Global => (In_Out => (Mailboxes)),
     Pre => Message /= null
     and then Is_Valid(Message.all)
     and then (if Receiver_Address(Message).Domain_ID = Domain_ID then Module_Ready(Receiver_Address(Message).Module_ID))
     and then Module_Ready(Receiver_Address(Message).Module_ID),
     --and then Receives(Receiver_Address(Message).Module_ID, Message_Type(Message))
     Post => Message = null;

   -- Send the indicated message to the right mailbox. This might cross domains. This procedure
   -- returns at once. If the message could not be delivered it is lost with no indication.
   --
   -- Depreciated: Use Mailboxes to send messages
   procedure Route_Message(Message : in out Msg_Owner)
     with Global => (In_Out => Mailboxes),
     Pre => Message /= null and then Is_Valid(Message.all)
     and then Messaging_Ready,
     --and then Receives(Receiver_Address(Message).Module_ID, Message_Type(Message))
     Post => Message = null;

   procedure Route_Message(Message : in Message_Record)
     with Global => (In_Out => Mailboxes),
     Pre => (if Receiver_Address(Message).Domain_ID = Domain_ID then Module_Ready(Receiver_Address(Message).Module_ID))
     and then Messaging_Ready
     --and then Receives(Receiver_Address(Message).Module_ID, Message_Type(Message))
     and then Is_Valid(Message);
   procedure Route_Message(Message : in Message_Record; Status : out Status_Type)
     with Global => (In_Out => Mailboxes),
     Pre => (if Receiver_Address(Message).Domain_ID = Domain_ID then Module_Ready(Receiver_Address(Message).Module_ID))
     and then Messaging_Ready
     --and then Receives(Receiver_Address(Message).Module_ID, Message_Type(Message))
     and then Is_Valid(Message);

   -- Blocks until all modules in the domain have intitialized
   -- their mailbox and are ready to receive messages.
   procedure Wait
     with Post => Messaging_Ready;

private
   type Message_Record is
      record
         Sender_Address : Message_Address := (1,1);
         Receiver_Address : Message_Address := (1,1);
         Request_ID : Request_ID_Type := 1;
         Message_Type : Universal_Message_Type := (1,1);
         Priority   : System.Priority := 1;
         Payload    : Data_Array_Owner := null;
      end record;

   function Sender_Address(Msg : Message_Record) return Message_Address is (Msg.Sender_Address);
   function Receiver_Address(Msg : Message_Record) return Message_Address is (Msg.Receiver_Address);
   function Request_ID(Msg : Message_Record) return Request_ID_Type is (Msg.Request_ID);
   function Message_Type(Msg : Message_Record) return Universal_Message_Type is (Msg.Message_Type);
   function Priority(Msg : Message_Record) return System.Priority is (Msg.Priority);
   function Payload(Msg : Message_Record) return access constant Data_Array is (Msg.Payload);

   function Sender_Address(Msg : not null access constant Message_Record) return Message_Address is (Msg.Sender_Address);
   function Receiver_Address(Msg : not null access constant Message_Record) return Message_Address is (Msg.Receiver_Address);
   function Request_ID(Msg : not null access constant Message_Record) return Request_ID_Type is (Msg.Request_ID);
   function Message_Type(Msg : not null access constant Message_Record) return Universal_Message_Type is (Msg.Message_Type);
   function Priority(Msg : not null access constant Message_Record) return System.Priority is (Msg.Priority);
   function Payload(Msg : not null access constant Message_Record) return access constant Data_Array is (Msg.Payload);

   type Module_Mailbox is
      record
         Module_ID : Module_ID_Type;
         Spec : Module_Metadata;
      end record;

   function Spec(Box : Module_Mailbox) return Module_Metadata
     is (Box.Spec);
   function Valid(Box : Module_Mailbox) return Boolean
   is (Box.Spec.Receive_Types /= null);

   -- Create a copy of the given message on the heap,
   -- also making a copy of the payload content.
   function Copy(Msg : Message_Record) return not null Msg_Owner
     with Pre => Is_Valid(Msg),
       Post => Is_Valid(Copy'Result.all);

   -- Create a copy of the given message on the heap,
   -- moving the payload content.
   procedure Move(Msg : in out Message_Record; Result : out not null Msg_Owner)
     with Pre => Is_Valid(Msg),
       Post => not Is_Valid(Msg) and Is_Valid(Result.all);

end CubedOS.Generic_Message_Manager;
