--------------------------------------------------------------------------------
-- FILE   : cubedos-message_types.ads
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- This package defines several types used in the message passing system.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with System;
with CubedOS.Lib.XDR; use CubedOS.Lib.XDR;

package CubedOS.Message_Types is

   ------------------
   -- Addresses
   ------------------

   -- Every domain in a system is uniquely identified by a Domain_ID.
   -- Domain #0 is special; it means the "current" domain.
   -- A CubedOS system supports a maximum of 256 domains.
   type Domain_ID_Type is new Natural range 0 .. 256;

   -- In a CubedOS system, every type of module has a
   -- statically assigned ID. A system may include a maximum
   -- of unique modules.
   type Module_ID_Type is new Positive range 1 .. 512;

   -- An address unambigiously identifies exactly one module by
   -- its Module_ID and Domain_ID.
   type Message_Address is
      record
         Domain_ID : Domain_ID_Type := 0;
         Module_ID : Module_ID_Type := 1;
      end record;


   -------------------
   -- Messages
   -------------------

   -- Definition of message IDs. A module may define several message types associated with
   -- it. Each type of message must have a statically declared message ID that is unique to
   -- that module.
   Maximum_Message_Count : constant := 256;
   type Message_ID_Type is range 0 .. Maximum_Message_Count - 1;

   -- The universal message type unambigiously associates a message type across
   -- modules and domains with exactly one definition.
   type Universal_Message_Type is
      record
         Module_ID : Module_ID_Type;
         Message_ID : Message_ID_Type;
      end record;

   -- Definition of Request IDs. Normally requests are given unique ID values that are echoed in
   -- replies. This allows a module to associate a reply with a particular request. The request
   -- ID of zero is special; it is used in cases where no such request/reply matching is needed
   -- or sensible.
   type Request_ID_Type is mod 2**32;

   -- The content of a message is stored in an XDR data array.
   -- Starting the index type at 0 is convenient when expressing "multiple of four" assertions.
   -- The 'extended' index type provides an extra value before the first allowed index. This is
   -- used by the XDR Octet_Array and String encoders so they can return a correct 'Last' when
   -- given zero length values to encode. Support for encoding zero length arrays and strings is
   -- useful.
   -- TODO: Remove these definitions. They aren't needed anymore because payload size is variable.
   subtype Data_Index_Type is XDR_Index_Type;
   subtype Data_Extended_Index_Type is XDR_Extended_Index_Type;
   subtype Data_Size_Type is XDR_Size_Type;
   subtype Data_Array is XDR_Array;
   type Data_Array_Owner is access Data_Array;

   -- Error type for message decoding
   type Message_Status_Type is (Success, Malformed, Insufficient_Space);  -- Message decoding.

   -- TODO: All references to this value by API packages should be replaced
   -- with actual message sizes and this constant should be removed.
   Max_Message_Size : constant Positive := 1024;

   -- Message records hold information passed between modules with some
   -- metadata about the transfer. It may reference a dynamically allocated
   -- payload which stores the message data.
   type Message_Record is private
     with Default_Initial_Condition => Payload(Message_Record) = null;
   type Msg_Owner is access Message_Record;

   function Sender_Address(Msg : Message_Record) return Message_Address;
   function Receiver_Address(Msg : Message_Record) return Message_Address;
   function Request_ID(Msg : Message_Record) return Request_ID_Type;
   function Message_Type(Msg : Message_Record) return Universal_Message_Type;
   function Priority(Msg : Message_Record) return System.Priority;
   function Payload(Msg : Message_Record) return access constant Data_Array;

   function Sender_Address(Msg : not null access constant Message_Record) return Message_Address;
   function Receiver_Address(Msg : not null access constant Message_Record) return Message_Address;
   function Request_ID(Msg : not null access constant Message_Record) return Request_ID_Type;
   function Message_Type(Msg : not null access constant Message_Record) return Universal_Message_Type;
   function Priority(Msg : not null access constant Message_Record) return System.Priority;
   function Payload(Msg : not null access constant Message_Record) return access constant Data_Array;


   --------------
   -- Modules
   --------------

   type Message_Type_Array is array (Natural range <>) of Universal_Message_Type;
   type Const_Msg_Type_Array_Ptr is access constant Message_Type_Array;

   -- TODO: Replace this with a zero width array instead of having an arbitrary value
   Empty_Type_Array : aliased constant Message_Type_Array := (0 => (1,1));

   -- Describes the messaging properties of a module, including the types
   -- of messages it may receive and its Module_ID.
   type Module_Metadata is
      record
         Module_ID : Module_ID_Type;
         Receive_Types : not null Const_Msg_Type_Array_Ptr := Empty_Type_Array'Access;
      end record;

   function Define_Module(This_Module : Module_ID_Type; This_Receives : Const_Msg_Type_Array_Ptr) return Module_Metadata
     with Pre => This_Receives /= null,
     Post => (for all T of This_Receives.all => Receives(Define_Module'Result, T))
     and Define_Module'Result.Module_ID = This_Module;

   -- Determines if the given module is capable of receiving the given message type.
   function Receives(Receiver : Module_Metadata; Msg_Type : Universal_Message_Type) return Boolean
     with Pre => Receiver.Receive_Types /= null;


   ---------------
   -- Domains
   ---------------

   type Module_ID_Set is array (Positive range <>) of Module_ID_Type;

   -- Describes a domain, assigning it an ID and a set of modules which
   -- it will include.
   type Domain_Metadata(Module_Count : Positive) is
      record
         ID : Domain_ID_Type;
         Module_IDs : Module_ID_Set(1 .. Module_Count);
      end record;

   -- Checks if the given domain contains a module with the given id.
   function Has_Module(Domain : Domain_Metadata; Module_ID : Module_ID_Type) return Boolean;


   --------------
   -- Debug
   --------------

   -- Convenience function to stringify messages
   function Stringify_Message (Message : in Message_Record) return String;

   --------------------
   -- Memory Managment
   --------------------

   -- Messages store their payload content on the heap. This means that several
   -- operations done on them require care to avoid leaking memory.

   -- Create a copy of the given message on the heap,
   -- also making a copy of the payload content and returning
   -- an access to it.
   function Copy(Msg : Message_Record) return not null Msg_Owner
     with Pre => Payload(Msg) /=  null,
     Post => Payload(Copy'Result) /= null;

   procedure Copy(Msg : Message_Record; Result : out Message_Record)
     with Pre => Payload(Msg) /= null,
       Post => Payload(Msg) /= null and Payload(Result) /= null;

   -- Create a copy of the given message on the heap,
   -- moving the payload content. Returns access to the new message.
   -- The given message looses access to its payload.
   procedure Move(Msg : in out Message_Record; Result : out not null Msg_Owner)
     with Pre => Payload(Msg) /= null,
     Post => Payload(Msg) = null and Payload(Result) /= null;

   -- Frees the payload of the message.
   procedure Delete(Msg : in out Message_Record)
     with Pre => Payload(Msg) /= null,
     Post => Payload(Msg) = null;

   -- Frees the message pointed to. If the message has
   -- an allocated payload, free that also.
   procedure Delete(Msg : in out Msg_Owner)
     with Pre => Msg /= null,
     Post => Msg = null;

private

   function Define_Module(This_Module : Module_ID_Type; This_Receives : Const_Msg_Type_Array_Ptr) return Module_Metadata
     is (This_Module, This_Receives);

   function Has_Module(Domain : Domain_Metadata; Module_ID : Module_ID_Type) return Boolean
     is (for some M of Domain.Module_IDs => M = Module_ID);

   function Receives(Receiver : Module_Metadata; Msg_Type : Universal_Message_Type) return Boolean
     is (for some T of Receiver.Receive_Types.all => T = Msg_Type);

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

end CubedOS.Message_Types;
