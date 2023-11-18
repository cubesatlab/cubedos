--------------------------------------------------------------------------------
-- FILE   : cubedos-message_types-mutable.adb
-- SUBJECT: Specification of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- This package implements several functions used in the message passing system in
-- auto-generated API files.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Unchecked_Deallocation;

package body CubedOS.Message_Types.Mutable is

   function Make_Empty_Message
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID     : Request_ID_Type; Message_Type : Universal_Message_Type;
      Payload_Size : Natural;
      Priority       : System.Priority := System.Default_Priority)
      return Mutable_Message_Record
   is
      subtype Definite_Data_Array is Data_Array(0 .. Payload_Size - 1);
   begin
      return (
              Sender_Address => Sender_Address,
              Receiver_Address => Receiver_Address,
              Request_ID => Request_ID,
              Message_Type => Message_Type,
              Priority => Priority,
              Payload => new Definite_Data_Array'(others => 0)
             );
   end Make_Empty_Message;

   procedure Make_Empty_Message
     (Sender_Address : Message_Address; Receiver_Address : Message_Address;
      Request_ID     : Request_ID_Type; Message_Type : Universal_Message_Type;
      Payload : in out Data_Array_Owner;
      Priority       : System.Priority := System.Default_Priority;
     Result : out Mutable_Message_Record)
   is
   begin
      Result := (Sender_Address => Sender_Address,
                 Receiver_Address => Receiver_Address,
                 Request_ID => Request_ID,
                 Message_Type => Message_Type,
                 Priority => Priority,
                 Payload => Payload
                );
      Payload := null;
   end Make_Empty_Message;

   function Immutable(Msg : Mutable_Message_Record) return Message_Record is
      Payload_Copy : constant Data_Array_Owner := new Data_Array'(Msg.Payload.all);
   begin
      return (Msg.Sender_Address, Msg.Receiver_Address, Msg.Request_ID, Msg.Message_Type, Msg.Priority, Payload_Copy);
   end Immutable;

   procedure Private_Free is new Ada.Unchecked_Deallocation
     (Object => Data_Array, Name => Data_Array_Owner);

   procedure Delete(Msg : in out Mutable_Message_Record)
     with SPARK_Mode => Off
   is
   begin
      -- We lie to SPARK here to hide the fact
      -- that technically Free is a blocking function.
      Private_Free(Msg.Payload);
   end Delete;

end CubedOS.Message_Types.Mutable;
