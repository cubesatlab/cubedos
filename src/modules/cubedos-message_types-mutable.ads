--------------------------------------------------------------------------------
-- FILE   : cubedos-message_types-mutable.ads
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- This package defines several types used in the message passing system in
-- auto-generated API files.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

package CubedOS.Message_Types.Mutable is

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

   -- Creates an immutible copy of the given message
   -- with a new copy of the given message's payload.
   function Immutable(Msg : Mutable_Message_Record) return Message_Record
     with Pre => Msg.Payload /= null,
     Post => Payload(Immutable'Result) /= null and
     Message_Type(Immutable'Result) = Msg.Message_Type and
     Receiver_Address(Immutable'Result) = Msg.Receiver_Address and
     Sender_Address(Immutable'Result) = Msg.Sender_Address and
     Request_ID(Immutable'Result) = Msg.Request_ID and
     Priority(Immutable'Result) = Msg.Priority and
     Payload(Immutable'Result).all = Msg.Payload.all;

   -- Frees any dynamically allocated memory the message references.
   procedure Delete(Msg : in out Mutable_Message_Record)
     with Pre => Msg.Payload /= null,
     Post => Msg.Payload = null;


end CubedOS.Message_Types.Mutable;
