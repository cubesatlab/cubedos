--------------------------------------------------------------------------------
-- FILE   : cubedos-message_types.adb
-- SUBJECT: Implementation of message passing types in CubedOS.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- This package implements several types used in the message passing system.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Ada.Unchecked_Deallocation;

package body CubedOS.Message_Types is

   procedure Private_Free is new Ada.Unchecked_Deallocation
     (Object => Message_Record, Name => Msg_Owner);
   procedure Private_Free is new Ada.Unchecked_Deallocation
     (Object => Data_Array, Name => Data_Array_Owner);

   function Is_Valid(Msg : Message_Record) return Boolean is
     (Msg.Payload /= null);

   function Immutable(Msg : Mutable_Message_Record) return Message_Record is
      Payload_Copy : constant Data_Array_Owner := new Data_Array'(Msg.Payload.all);
   begin
      return (Msg.Sender_Address, Msg.Receiver_Address, Msg.Request_ID, Msg.Message_Type, Msg.Priority, Payload_Copy);
   end Immutable;

   procedure Delete(Msg : in out Mutable_Message_Record)
     with SPARK_Mode => Off
   is
   begin
      -- We lie to SPARK here to hide the fact
      -- that technically Free is a blocking function.
      Private_Free(Msg.Payload);
   end Delete;

   procedure Delete(Msg : in out Message_Record)
     with SPARK_Mode => Off
   is
   begin
      -- We lie to spark about this because Free shouldn't be
      -- considered a blocking function.
      Private_Free(Msg.Payload);
   end Delete;

   procedure Delete(Msg : in out Msg_Owner) is
   begin
      if Msg.Payload /= null then
         Delete(Msg.all);
      end if;
      Private_Free(Msg);
   end Delete;

   procedure Free(Msg : in out Msg_Owner) is
   begin
      Private_Free(Msg);
   end Free;

   procedure Clear_Payload(Msg : in out Message_Record) is
   begin
      Msg.Payload := null;
   end Clear_Payload;

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

   function Stringify_Message (Message : in Message_Record) return String is
      Sender_Domain_String : constant String :=
        Domain_ID_Type'Image (Message.Sender_Address.Domain_ID);
      Sender_Module_String : constant String :=
        Module_ID_Type'Image (Message.Sender_Address.Module_ID);
      Receiver_Domain_String : constant String :=
        Domain_ID_Type'Image (Message.Receiver_Address.Domain_ID);
      Receiver_Module_String : constant String :=
        Module_ID_Type'Image (Message.Receiver_Address.Module_ID);
      Request_ID_String : constant String :=
        Request_ID_Type'Image (Message.Request_ID);
      Message_Type_String : constant String :=
        "(M:" & Module_ID_Type'Image(Message.Message_Type.Module_ID) & ", ID:"& Message_ID_Type'Image (Message.Message_Type.Message_ID) & ")";
      Message_Image : constant String :=
        "Src (D:" & Sender_Domain_String & ", M:" & Sender_Module_String & ") Dest (D:" &
        Receiver_Domain_String & ", M:" & Receiver_Module_String & ") Req_ID: " &
        Request_ID_String & " Msg_Type: " & Message_Type_String;
   begin
      return Message_Image;
   end Stringify_Message;

   function Copy(Msg : Message_Record) return not null Msg_Owner
   is
      subtype Definite_Data_Array is Data_Array(Msg.Payload'Range);
   begin
      return new Message_Record'(Sender_Address => Msg.Sender_Address,
                                Receiver_Address => Msg.Receiver_Address,
                                Request_ID => Msg.Request_ID,
                                Message_Type => Msg.Message_Type,
                                Priority => Msg.Priority,
                                Payload => new Definite_Data_Array'(Msg.Payload.all)
                               );
   end;

      procedure Move(Msg : in out Message_Record; Result : out not null Msg_Owner)
   is
   begin
      Result := new Message_Record'(Sender_Address => Msg.Sender_Address,
                                    Receiver_Address => Msg.Receiver_Address,
                                    Request_ID => Msg.Request_ID,
                                    Message_Type => Msg.Message_Type,
                                    Priority => Msg.Priority,
                                    Payload => Msg.Payload
                                   );
      Msg.Payload := null;
   end Move;

end CubedOS.Message_Types;
