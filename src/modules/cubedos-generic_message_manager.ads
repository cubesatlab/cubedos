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

generic
   Domain_Number : Positive;  -- The domain ID of this message manager.
   Module_Count  : Positive;  -- Number of modules in the system.
   Maximum_Message_Size : Positive;  -- Maximum size of each message payload.
package CubedOS.Generic_Message_Manager
  with
    Abstract_State => ((Mailboxes with External), Mailbox_Init_Tracker, (Request_ID_Generator with External)),
    Initializes => (Mailboxes, Request_ID_Generator, Mailbox_Init_Tracker)
is
   -- Definition of domain ID numbers. Domain #0 is special; it means the "current" domain.
   -- There is a limit to the number of domains that can be used. Make this a generic parameter?
   Maximum_Domain_Count : constant := 256;
   subtype Domain_ID_Type is Natural range 0 .. Maximum_Domain_Count;
   Domain_ID : constant Domain_ID_Type := Domain_Number;

   -- Definition of module ID numbers. Full IDs are qualified by the domain ID.
   subtype Module_ID_Type is Positive range 1 .. Module_Count;

   -- Definition of message IDs. Full IDs are qualified by the module ID.
   -- There is a limit to how many messages a module can define. Make this a generic parameter?
   Maximum_Message_Count : constant := 256;
   type Message_ID_Type is range 0 .. Maximum_Message_Count - 1;

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
   subtype Data_Index_Type is XDR_Index_Type range 0 .. Maximum_Message_Size - 1;
   subtype Data_Extended_Index_Type is XDR_Extended_Index_Type range -1 .. Maximum_Message_Size - 1;
   subtype Data_Size_Type is XDR_Size_Type range 0 .. Maximum_Message_Size;
   subtype Data_Array is XDR_Array(Data_Index_Type);

   -- Message Addresses hold the Domain_ID and Module_ID for Modules in a CubedOS Application
   type Message_Address is
      record
         Domain_ID : Domain_ID_Type := 0;
         Module_ID : Module_ID_Type := 1;
       end record;
   -- Messages currently have a priority field that is not used. The intention is to allow high
   -- priority messages to be processed earlier and without interruption. SPARK does not support
   -- dynamic task priorities, however, so the usefulness of this idea is questionable. We could
   -- still sort mailboxes by message priority (not currently done) which might be a little
   -- useful.
   type Message_Record is
      record
         Sender_Address : Message_Address := (0, 1);
         Receiver_Address : Message_Address := (0 , 1);
         Request_ID : Request_ID_Type := 0;
         Message_ID : Message_ID_Type := 0;
         Priority   : System.Priority := System.Default_Priority;
         Size       : Data_Size_Type  := 0;
         Payload    : Data_Array      := (others => 0);
      end record;

   type Msg_Owner is access Message_Record;

   -- Convenience constructor function for messages. This is used by encoding functions.
   function Make_Empty_Message
     (Sender_Address : Message_Address;
      Receiver_Address : Message_Address;
      Request_ID : Request_ID_Type;
      Message_ID : Message_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Post=>
         Make_Empty_Message'Result.Sender_Address   = Sender_Address   and
         Make_Empty_Message'Result.Receiver_Address = Receiver_Address and
         Make_Empty_Message'Result.Request_ID = Request_ID and
         Make_Empty_Message'Result.Message_ID = Message_ID and
         Make_Empty_Message'Result.Priority   = Priority   and
         Make_Empty_Message'Result.Size       = 0;

   -- Convenience function to stringify messages
   function Stringify_Message (Message : in Message_Record) return String;

   -- Returns an arbitrary, domain-unique request ID. Probably these IDs should also be unique
   -- across domains, but that is not yet implemented.
   procedure Get_Next_Request_ID(Request_ID : out Request_ID_Type)
     with Global => (In_Out => Request_ID_Generator);

   -- Error codes.
   type Status_Type is (Accepted, Mailbox_Full);                          -- Mailbox access.
   type Message_Status_Type is (Success, Malformed, Insufficient_Space);  -- Message decoding.

   -- Retrieves a message from the indicated mailbox. May block indefinitely.
   procedure Fetch_Message(Module : in Module_ID_Type; Message : out Msg_Owner)
     with Global => (In_Out => Mailboxes);

   -- True if the module is ready to receive mail.
   function Module_Ready(Module_ID : Module_ID_Type) return Boolean
     with Global => (Input => Mailbox_Init_Tracker);


   -- A mailbox used by modules to send and receive messages.
   -- This is the only way to receive messages sent to a module
   -- and the only intended way to send messages from it.
   -- Modules should keep their instance of this object invisible
   -- to outside modules.
   type Module_Mailbox is limited private;

   -- Sends the given message to the given address from this mailbox's address.
   -- Returns immediately.
   procedure Send_Message(Box : Module_Mailbox; Msg : Message_Record)
     with Global => (In_Out => Mailboxes),
     Depends => (Mailboxes => +(Box, Msg));

   -- Sends the given message to the given address from this mailbox's address.
   -- Returns immediately with the status of the operation's result.
   procedure Send_Message(Box : Module_Mailbox; Msg : Message_Record; Status : out Status_Type)
     with Global => (In_Out => Mailboxes);

   -- Reads the next message, removing it from the message queue.
   -- Blocks until a message is available.
   procedure Read_Next(Box : Module_Mailbox; Msg : out Msg_Owner);

   -- Register a module with the mail system.
   -- Gets an observer for that module's mailbox.
   procedure Register_Module(Module_ID : in Module_ID_Type;
                             Msg_Queue_Size : in Positive;
                             Mailbox : out Module_Mailbox)
     with Global => (In_Out => (Mailboxes, Mailbox_Init_Tracker)),
       Depends => (Mailboxes => +(Msg_Queue_Size, Module_ID),
                   Mailbox_Init_Tracker => +(Module_ID),
                   Mailbox => Module_ID),
       Pre => Module_Ready(Module_ID) = False,
       Post => Module_Ready(Module_ID) = True;


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
     with Global => (In_Out => Mailboxes, Input => Mailbox_Init_Tracker),
       Pre => (for all I in Module_ID_Type => Module_Ready(I));


   -- Send the indicated message to the right mailbox. This might cross domains. This procedure
   -- returns at once with a status of Accepted if the message was definitely delivered. A status
   -- of Mailbox_Full indicates that delivery did not occur.
   --
   -- Depreciated: Use Mailboxes to send messages
   procedure Route_Message(Message : in out Msg_Owner; Status : out Status_Type)
     with Global => (In_Out => (Mailboxes, Mailbox_Init_Tracker)),
       Pre => (if Message.Receiver_Address.Domain_ID = Domain_ID then Module_Ready(Message.Receiver_Address.Module_ID));

   -- Send the indicated message to the right mailbox. This might cross domains. This procedure
   -- returns at once. If the message could not be delivered it is lost with no indication.
   --
   -- Depreciated: Use Mailboxes to send messages
   procedure Route_Message(Message : in out Msg_Owner)
     with Global => (In_Out => (Mailboxes, Mailbox_Init_Tracker)),
       Pre => (if Message.Receiver_Address.Domain_ID = Domain_ID then Module_Ready(Message.Receiver_Address.Module_ID));

   procedure Route_Message(Message : in Message_Record)
     with Global => (In_Out => (Mailboxes, Mailbox_Init_Tracker)),
     Pre => (if Message.Receiver_Address.Domain_ID = Domain_ID then Module_Ready(Message.Receiver_Address.Module_ID));
   procedure Route_Message(Message : in Message_Record; Status : out Status_Type)
     with Global => (In_Out => (Mailboxes, Mailbox_Init_Tracker)),
       Pre => (if Message.Receiver_Address.Domain_ID = Domain_ID then Module_Ready(Message.Receiver_Address.Module_ID));

private

      type Module_Mailbox is
      record
         Address : Message_Address;
      end record;

end CubedOS.Generic_Message_Manager;
