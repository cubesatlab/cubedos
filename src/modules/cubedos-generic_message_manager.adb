--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.adb
-- SUBJECT: Body of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Ada.Unchecked_Deallocation;

-- with Name_Resolver;

package body CubedOS.Generic_Message_Manager with
   Refined_State => (Mailboxes => (Message_Storage),
    Mailbox_Metadata => (Inited, Mailbox_Types), Request_ID_Generator => Request_ID_Gen)
is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Message_Record, Name => Msg_Owner);
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Data_Array, Name => Data_Array_Owner);

   -- A protected object for generating request ID values.
   protected Request_ID_Gen is
      procedure Generate_Next_ID (Request_ID : out Request_ID_Type);
   private
      Next_Request_ID : Request_ID_Type := 1;
   end Request_ID_Gen;

   type Msg_Ptr_Array_Ptr is access Message_Ptr_Array;
   type Msg_Type_Array_Ptr is access Message_Type_Array;

   Mailbox_Types   : array (Module_ID_Type) of Msg_Type_Array_Ptr;
   Inited          : array (Module_ID_Type) of Boolean := (others => False);

   type Message_Queue is
      record
         Messages        : Msg_Ptr_Array_Ptr  := null;
         Count           : Message_Count_Type := 0;
         Next_In         : Message_Index_Type := 1;
         Next_Out        : Message_Index_Type := 1;
         Mailbox_Size    : Positive           := 1;
      end record
     with Dynamic_Predicate => (if Messages /= null then
                                  Count in 0..Messages'Length
                             and Next_In in Messages'Range
                             and Next_Out in Messages'Range
                             and Mailbox_Size = Messages'Length
                               );

   -- A protected type for holding messages.
   protected type Sync_Mailbox is

      -- Deposit the given message into THIS mailbox. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the returned status indicates this.
      procedure Send
        (Message : in out Msg_Owner; Status : out Status_Type)
        with Pre => Message /= null,
        Post => Message = null;

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the message is lost.
      procedure Unchecked_Send (Message : in out Msg_Owner)
        with Pre => Message /= null,
        Post => Message = null;

      -- Returns the number of messages in the mailbox.
      function Message_Count return Message_Count_Type;

      -- Receive a message. This entry waits indefinitely for a message to be available.
      entry Receive (Message : out Message_Record);

      -- Change the array used to store messages.
      procedure Set_Msg_Array (Arr : in out Msg_Ptr_Array_Ptr)
        with Pre => Arr /= null,
          Post => Arr = null;

   private
      Q : Message_Queue;
      Message_Waiting : Boolean := False;
   end Sync_Mailbox;

   -- One mailbox for each module.
   Message_Storage : array (Module_ID_Type) of Sync_Mailbox;

   function Module_Ready (Module_ID : Module_ID_Type) return Boolean is
     (Inited (Module_ID))
       with Refined_Global => (Input => Inited),
         Refined_Post => Module_Ready'Result = Inited(Module_ID);

   ------------------
   -- Implementations
   ------------------

   protected body Request_ID_Gen is

      procedure Generate_Next_ID (Request_ID : out Request_ID_Type) is
      begin
         Request_ID      := Next_Request_ID;
         Next_Request_ID := Next_Request_ID + 1;
      end Generate_Next_ID;

   end Request_ID_Gen;

   protected body Sync_Mailbox is

      procedure Send (Message : in out Msg_Owner; Status : out Status_Type) is
      begin
         if Q.Count = Q.Mailbox_Size then
            Status := Mailbox_Full;
            Message := null;
         else
            Q.Messages (Q.Next_In) := Message;
            Message := null;
            if Q.Next_In = Q.Mailbox_Size then
               Q.Next_In := 1;
            else
               Q.Next_In := Q.Next_In + 1;
            end if;
            Q.Count           := Q.Count + 1;
            Message_Waiting := True;
            Status          := Accepted;
         end if;

      end Send;

      procedure Unchecked_Send (Message : in out Msg_Owner) is
      begin
         if Q.Count /= Q.Mailbox_Size then
            Q.Messages (Q.Next_In) := Message;
            if Q.Next_In = Q.Mailbox_Size then
               Q.Next_In := 1;
            else
               Q.Next_In := Q.Next_In + 1;
            end if;
            Q.Count           := Q.Count + 1;
            Message_Waiting := True;
         end if;
         Message := null;
      end Unchecked_Send;

      function Message_Count return Message_Count_Type is
      begin
         return Q.Count;
      end Message_Count;

      entry Receive (Message : out Message_Record) when Message_Waiting is
         Ptr : Msg_Owner := Q.Messages (Q.Next_Out);
      begin
         Q.Messages (Q.Next_Out) := null;
         Message := Ptr.all;
         Free(Ptr);
         if Q.Next_Out = Q.Mailbox_Size then
            Q.Next_Out := 1;
         else
            Q.Next_Out := Q.Next_Out + 1;
         end if;
         Q.Count := Q.Count - 1;
         if Q.Count = 0 then
            Message_Waiting := False;
         end if;
      end Receive;

      procedure Set_Msg_Array (Arr : in out Msg_Ptr_Array_Ptr) is
      begin
         Q.Mailbox_Size := Arr'Length;
         Q.Messages     := Arr;
         Arr          := null;
      end Set_Msg_Array;

   end Sync_Mailbox;

   procedure Delete(Msg : in out Mutable_Message_Record) is
   begin
      Free(Msg.Payload);
   end Delete;

   ----------------------
   -- Immutable Messages
   ----------------------

   function Is_Valid(Msg : Message_Record) return Boolean is
     (Msg.Payload /= null);

   function Immutable(Msg : Mutable_Message_Record) return Message_Record is
      Payload_Copy : constant Data_Array_Owner := new Data_Array'(Msg.Payload.all);
   begin
      return (Msg.Sender_Address, Msg.Receiver_Address, Msg.Request_ID, Msg.Message_Type, Msg.Priority, Payload_Copy);
   end Immutable;

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

   procedure Delete(Msg : in out Message_Record) is
   begin
      Free(Msg.Payload);
   end Delete;

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
        Module_ID_Type'Image (Message.Receiver_Address.Domain_ID);
      Receiver_Module_String : constant String :=
        Module_ID_Type'Image (Message.Receiver_Address.Module_ID);
      Request_ID_String : constant String :=
        Request_ID_Type'Image (Message.Request_ID);
      Message_Type_String : constant String :=
        Module_ID_Type'Image(Message.Message_Type.Module_ID) & Message_ID_Type'Image (Message.Message_Type.Message_ID);
      Message_Image : constant String :=
        Sender_Domain_String & "!" & Sender_Module_String & "!" &
        Receiver_Domain_String & "!" & Receiver_Module_String & "!" &
        Request_ID_String & "!" & Message_Type_String & "!";
   begin
      return Message_Image;
   end Stringify_Message;

   procedure Get_Next_Request_ID (Request_ID : out Request_ID_Type) is
   begin
      Request_ID_Gen.Generate_Next_ID (Request_ID);
   end Get_Next_Request_ID;

   function Receives(Receiver : Message_Address; Msg_Type : Universal_Message_Type) return Boolean
     is (Mailbox_Types(Receiver.Module_ID) /= null and then (for some T of Mailbox_Types(Receiver.Module_ID).all => T = Msg_Type));

   procedure Route_Message
     (Message : in out Msg_Owner; Status : out Status_Type)
   is
   begin
      -- For now, let's ignore the domain and just use the receiver Module_ID only.
      Message_Storage (Message.Receiver_Address.Module_ID).Send
        (Message, Status);
   end Route_Message;

   procedure Route_Message (Message : in out Msg_Owner) is
   begin
      if Message.Receiver_Address.Domain_ID /= Domain_ID then
         -- Circular Dependency with Name_Resolver so resorting to hardcoding
         -- Message_Storage(Name_Resolver.Network_Server.Module_ID).Unchecked_Send(Message);
         Message_Storage (2).Unchecked_Send (Message);
      else
         Message_Storage (Message.Receiver_Address.Module_ID).Unchecked_Send
           (Message);
         pragma Unused(Message);
      end if;
   end Route_Message;

   procedure Route_Message (Message : in Message_Record) is
      Ptr : Msg_Owner := Copy(Message);
   begin
      Route_Message (Ptr);
      pragma Unused(Ptr);
   end Route_Message;

   procedure Route_Message
     (Message : in Message_Record; Status : out Status_Type)
   is
      Ptr : Msg_Owner := Copy(Message);
   begin
      Route_Message (Ptr, Status);
      pragma Unused(Ptr);
   end Route_Message;

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
      end;

   procedure Fetch_Message
     (Module : in Module_ID_Type; Message : out Message_Record)
   is
   begin
      Message_Storage (Module).Receive (Message);
   end Fetch_Message;

   -------------
   -- Mailbox
   -------------

   function Address(Box : Module_Mailbox) return Message_Address is
      (Box.Address);

   procedure Send_Message (Box : Module_Mailbox; Msg : in out Message_Record) is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Ptr.Sender_Address := Box.Address;
      Route_Message (Ptr);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Send_Message
     (Box : Module_Mailbox; Msg : in out Message_Record; Status : out Status_Type)
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Ptr.Sender_Address := Box.Address;
      Route_Message (Ptr, Status);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Read_Next (Box : Module_Mailbox; Msg : out Message_Record) is
   begin
      Fetch_Message (Box.Address.Module_ID, Msg);
   end Read_Next;

   procedure Queue_Size(Box : Module_Mailbox; Size : out Natural) is
   begin
      Size := Message_Storage (Box.Address.Module_ID).Message_Count;
   end Queue_Size;

   procedure Register_Module
     (Module_ID : in     Module_ID_Type; Msg_Queue_Size : in Positive;
      Mailbox   :    out Module_Mailbox; Receives : in Message_Type_Array)
   is
      Arr : Msg_Ptr_Array_Ptr := new Message_Ptr_Array (1 .. Msg_Queue_Size);
   begin
      -- Create a new mailbox for the ID
      Message_Storage (Module_ID).Set_Msg_Array (Arr);
      pragma Unused(Arr);
      -- Store what message types it may receive
      Mailbox_Types (Module_ID) := new Message_Type_Array'(Receives);

      Mailbox            := (Address => (Domain_ID, Module_ID));
      Inited (Module_ID) := True;
   end Register_Module;

   procedure Get_Message_Counts (Count_Array : out Message_Count_Array)
     with Refined_Global => (Proof_In => (Inited), In_Out => Message_Storage)
   is
   begin
      for I in Module_ID_Type loop
         Count_Array (I) := Message_Storage (I).Message_Count;
      end loop;
   end Get_Message_Counts;

end CubedOS.Generic_Message_Manager;
