--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.ads
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

generic
   Domain_Number : Positive;  -- The domain ID of this message manager.
   Module_Count  : Positive;  -- Number of modules in the system.
   Mailbox_Size  : Positive;  -- Maximum number of pending messages in a mailbox.
   Maximum_Message_Size : Positive;  -- Maximum size of each message payload.
package CubedOS.Generic_Message_Manager
  with
    Abstract_State => ((Mailboxes with External), (Request_ID_Generator with External)),
    Initializes => (Mailboxes, Request_ID_Generator)
is

   -- Definition of domain ID numbers. Domain #0 is special; it means the "current" domain.
   -- There is a limit to the number of domains that can be used. Make this a generic parameter?
   Maximum_Domain_Count : constant := 16;
   subtype Domain_ID_Type is Natural range 0 .. Maximum_Domain_Count;
   Domain_ID : constant Domain_ID_Type := Domain_Number;

   -- Definition of module ID numbers.
   subtype Module_ID_Type is Positive range 1 .. Module_Count;

   -- Definition of message IDs. Full IDs are qualified by the module ID.
   -- There is a limit to how many messages a module can define.
   Maximum_Message_Count : constant := 256;
   subtype Message_ID_Type is Natural range 0 .. Maximum_Message_Count - 1;

   -- Definition of Request IDs. A request ID identifies a full request between two endpoints
   -- So for instance, if Module A wants information from Module C, and must go through Module
   -- B to get it, the entire A -> B -> C -> B -> A conversation has a unique Request ID.
   type Request_ID_Type is new Natural;

   -- A fully qualified message address is the triple (Domain_ID, Module_ID, Message_ID).

   -- The primitive unit of data stored in an XDR message. This type in the XDR types below are
   -- defined here rather than in CubedOS.Lib.XDR because these types are needed for the mail-
   -- box definition.
   --
   type XDR_Octet is mod 2**8;

   -- Definition of a CubedOS message. Messages are stored in XDR_Arrays.

   -- Starting the index type at 0 is convenient when expressing "multiple of four" assertions.
   -- The 'extended' index type provides an extra value before the first allowed index. This is
   -- used by the XDR Octet_Array and String encoders so they can return a correct 'Last' when
   -- given zero length values to encode. Support for encoding zero length arrays and strings is
   -- useful.
   --
   subtype XDR_Index_Type is Natural range 0 .. Maximum_Message_Size - 1;
   subtype XDR_Extended_Index_Type is Integer range -1 .. Maximum_Message_Size - 1;
   subtype XDR_Size_Type is Natural range 0 .. Maximum_Message_Size;
   type XDR_Array is array(XDR_Index_Type) of XDR_Octet;

   type Message_Record is
      record
         Sender_Domain   : Domain_ID_Type := 0;
         Receiver_Domain : Domain_ID_Type := 0;
         Sender     : Module_ID_Type  := 1;  -- Would another module ID be more appropriate?
         Receiver   : Module_ID_Type  := 1;  -- Would another module ID be more appropriate?
         Request_ID : Request_ID_Type := 0;
         Message_ID : Message_ID_Type := 0;
         Priority   : System.Priority := System.Default_Priority;
         Size       : XDR_Size_Type   := 0;
         Payload    : XDR_Array       := (others => 0);
      end record;

   -- Definition of the array used to hold messages in a mailbox.
   subtype Message_Index_Type is Positive range 1 .. Mailbox_Size;
   subtype Message_Count_Type is Natural range 0 .. Mailbox_Size;
   type Message_Array_Unconstrained is array(Positive range <>) of Message_Record;
   type Message_Array is array(Message_Index_Type) of Message_Record;


   -- Convenience constructor function for messages. This is used by encoding functions.
   function Make_Empty_Message
     (Sender_Domain   : Domain_ID_Type;
      Receiver_Domain : Domain_ID_Type;
      Sender     : Module_ID_Type;
      Receiver   : Module_ID_Type;
      Request_ID : Request_ID_Type;
      Message_ID : Message_ID_Type;
      Priority   : System.Priority := System.Default_Priority) return Message_Record
     with
       Global => null,
       Post=>
         Make_Empty_Message'Result.Sender_Domain = Sender_Domain and
         Make_Empty_Message'Result.Receiver_Domain = Receiver_Domain and
         Make_Empty_Message'Result.Sender = Sender and
         Make_Empty_Message'Result.Receiver = Receiver and
         Make_Empty_Message'Result.Request_ID = Request_ID and
         Make_Empty_Message'Result.Message_ID = Message_ID and
         Make_Empty_Message'Result.Priority = Priority and
         Make_Empty_Message'Result.Size = 0;

   procedure Get_Next_Request_ID(Request_ID : out Request_ID_Type)
     with Global => (In_Out => Request_ID_Generator);

   -- Error codes.
   type Status_Type is (Accepted, Mailbox_Full, Series_Full);
   type Message_Status_Type is (Success, Malformed);

   -- Send the indiciated message to the right mailbox. This might cross domains.
   procedure Route_Message(Message : in Message_Record)
     with Global => (In_Out => Mailboxes);

   -- Retrieves a message from the indicated mailbox. May block indefinitely.
   procedure Fetch_Message(Module : in Module_ID_Type; Message : out Message_Record)
     with Global => (In_Out => Mailboxes);

end CubedOS.Generic_Message_Manager;
