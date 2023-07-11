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

   -- Definition of domain ID numbers. Domain #0 is special; it means the "current" domain.
   -- There is a limit to the number of domains that can be used. Make this a generic parameter?
   Maximum_Domain_Count : constant := 256;
   type Domain_ID_Type is new Natural range 0 .. Maximum_Domain_Count;

   -- Definition of module ID numbers. Full IDs are qualified by the domain ID.
   type Module_ID_Type is new Positive range 1 .. 256;

   -- Definition of message IDs. Full IDs are qualified by the module ID.
   -- There is a limit to how many messages a module can define. Make this a generic parameter?
   Maximum_Message_Count : constant := 256;
   type Message_ID_Type is range 0 .. Maximum_Message_Count - 1;

   -- Message Addresses hold the Domain_ID and Module_ID for Modules in a CubedOS Application
   type Message_Address is
      record
         Domain_ID : Domain_ID_Type := 0;
         Module_ID : Module_ID_Type := 1;
      end record;

   -- Definition of Request IDs. Normally requests are given unique ID values that are echoed in
   -- replies. This allows a module to associate a reply with a particular request. The request
   -- ID of zero i special; it is used in cases where no such request/reply matching is needed
   -- or sensible.
   type Request_ID_Type is mod 2**32;

   type Module_ID_Set is array (Natural range <>) of Module_ID_Type;

   -- Describes a domain
   type Domain_Declaration(Module_Count : Positive) is
      record
         ID : Domain_ID_Type;
         Module_IDs : Module_ID_Set(1 .. Module_Count);
      end record;

   function Declare_Domain(Module_Count : Positive; Domain_ID : Domain_ID_Type; Module_IDs : Module_ID_Set) return Domain_Declaration
     with Pre => Module_Count = Module_IDs'Length,
       Post => (for all M of Module_IDs => Has_Module(Declare_Domain'Result, M));

   function Has_Module(Domain : Domain_Declaration; Module_ID : Module_ID_Type) return Boolean;


   -- The union of a module ID and message ID. Forms a
   -- unique identifier for this type of message across
   -- all domains in this project.
   type Universal_Message_Type is
      record
         Module_ID : Module_ID_Type;
         Message_ID : Message_ID_Type;
      end record;

   type Message_Type_Array is array (Natural range <>) of Universal_Message_Type;
   type Msg_Type_Array_Ptr is access Message_Type_Array;
   type Const_Msg_Type_Array_Ptr is access constant Message_Type_Array;

   -- TODO: Replace this with a zero width array instead of having an arbitrary value
   Empty_Type_Array : aliased constant Message_Type_Array := (0 => (1,1));

   type Module_Metadata is
      record
         Module_ID : Module_ID_Type;
         Receive_Types : not null Const_Msg_Type_Array_Ptr := Empty_Type_Array'Access;
      end record;

   function Receives(Receiver : Module_Metadata; Msg_Type : Universal_Message_Type) return Boolean
     with Pre => Receiver.Receive_Types /= null;

   function Declare_Receives(This_Module : Module_ID_Type; This_Receives : Const_Msg_Type_Array_Ptr) return Module_Metadata
     with Pre => This_Receives /= null,
     Post => (for all T of This_Receives.all => Receives(Declare_Receives'Result, T))
     and Declare_Receives'Result.Module_ID = This_Module;

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

   --  type Domain_ID_List is array (Natural range <>) of Domain_ID_Type;
   --  type Transport_Module_ID is new Positive range 1..32;
   --  type Transport_Identifier (Domain_Count : Natural) is
   --     record
   --        ID : Transport_Module_ID;
   --        Takes_Domains : Domain_ID_List(1 .. Domain_Count);
   --     end record;

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

   procedure Clear_Payload(Msg : in out Message_Record);

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

   -- Frees any dynamically allocated memory the message references.
   procedure Delete(Msg : in out Mutable_Message_Record)
     with Pre => Msg.Payload /= null,
     Post => Msg.Payload = null;

   -- Frees any dynamically allocated memory the message references.
   procedure Delete(Msg : in out Message_Record)
     with Pre => Payload(Msg) /= null,
     Post => Payload(Msg) = null;

   -- Frees the message pointed to. If the message has
   -- an allocated payload, free that also.
   procedure Delete(Msg : in out Msg_Owner)
     with Pre => Msg /= null,
     Post => Msg = null;

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

private
   function Declare_Domain(Module_Count : Positive; Domain_ID : Domain_ID_Type; Module_IDs : Module_ID_Set) return Domain_Declaration
     is (Module_Count, Domain_ID, Module_IDs);

   function Declare_Receives(This_Module : Module_ID_Type; This_Receives : Const_Msg_Type_Array_Ptr) return Module_Metadata
     is (This_Module, This_Receives);

   function Has_Module(Domain : Domain_Declaration; Module_ID : Module_ID_Type) return Boolean
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
