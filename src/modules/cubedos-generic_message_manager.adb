--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.adb
-- SUBJECT: Body of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Ada.Unchecked_Deallocation;

-- with Name_Resolver;

with Ada.Text_IO;

with CubedOS.Lib.Bounded_Queues;
with Ada.Containers.Formal_Hashed_Maps;

package body CubedOS.Generic_Message_Manager with
Refined_State => (Mailboxes => Message_Storage,
                  Lock => Init_Lock,
                  Request_ID_Generator => Request_ID_Gen)
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

   function Equivalent_Key (Left, Right : Module_ID_Type) return Boolean is (Left = Right);
   function Hash_Func(Key : Module_ID_Type) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type(Key));
   package Boolean_Maps is new Ada.Containers.Formal_Hashed_Maps(Module_ID_Type, Boolean, Hash_Func, Equivalent_Key, Standard."=");
   type Boolean_Map is new Boolean_Maps.Map(Ada.Containers.Count_Type(Module_Count), Ada.Containers.Hash_Type(Module_ID_Type'Last));
   type Boolean_Map_Owner is access Boolean_Map;

   protected Init_Lock is
      entry Wait;
      function Is_Locked return Boolean;
      procedure Unlock (Module : Module_ID_Type)
        with Pre => (for some M of This_Domain.Module_IDs => M = Module);
   private
      Inited : Boolean_Map_Owner := new Boolean_Map;
      Locked : Boolean := True;
   end Init_Lock;

   package Message_Queues is new CubedOS.Lib.Bounded_Queues(Message_Record, Msg_Owner);
   type Message_Queue is new Message_Queues.Bounded_Queue;
   type Message_Queue_Owner is access Message_Queue;

   -- A protected type for holding messages.
   protected type Sync_Mailbox is

      -- Deposit the given message into THIS mailbox. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the returned status indicates this.
      procedure Send
        (Message : in out Msg_Owner; Status : out Status_Type)
        with Pre => Message /= null and then Payload(Message.all) /= null,
        Post => Message = null;

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the message is lost.
      procedure Unchecked_Send (Message : in out Msg_Owner)
        with Pre => Message /= null and then Payload(Message.all) /= null,
        Post => Message = null;

      -- Returns the number of messages in the mailbox.
      function Message_Count return Message_Count_Type;

      -- Receive a message. This entry waits indefinitely for a message to be available.
      entry Receive (Message : out Message_Record)
        with Post => Payload(Message) /= null;

      -- Change the array used to store messages.
      procedure Set_Queue_Size (Size : in Natural);

   private
      Q : Message_Queue_Owner := null;
      Message_Waiting : Boolean := False;
   end Sync_Mailbox;

   -- One mailbox for each module.
   Message_Storage : array (Module_ID_Type) of Sync_Mailbox;


   function Module_Ready (Module_ID : Module_ID_Type) return Boolean is
     (True);

   function Messaging_Ready return Boolean
     with SPARK_Mode => Off
     -- We hide this from SPARK because Init_Lock.Is_Locked doesn't have
     -- any meaningful interferences.
   is
   begin
      return not Init_Lock.Is_Locked;
   end Messaging_Ready;


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

   protected body Init_Lock is
      entry Wait when not Locked is
      begin null; end Wait;
      function Is_Locked return Boolean
        is (Locked);
      procedure Unlock (Module : Module_ID_Type) is
      begin
         -- Make sure all modules have a check
         for I of This_Domain.Module_IDs loop
            -- If an id is already in the map, this function
            -- won't override it.
            declare
               Position : Boolean_Maps.Cursor;
               Inserted : Boolean;
            begin
               Ada.Text_IO.Put_Line("Adding Line " & Module_ID_Type'Image(I));
               Insert(Inited.all, I, False, Position, Inserted);
               pragma Unused(Position, Inserted);
            end;
         end loop;

         Ada.Text_IO.Put_Line("Activating " & Module_ID_Type'Image(Module));
         Replace(Inited.all, Module, True);
         if (for all I of Inited.all => Element(Inited.all, I)) then
            Ada.Text_IO.Put_Line("Unlocking");
            Locked := False;
         end if;
      end Unlock;
   end Init_Lock;

   protected body Sync_Mailbox is

      procedure Send (Message : in out Msg_Owner; Status : out Status_Type) is
      begin
         if Q = null or else Is_Full(Q.all) then
            Status := Mailbox_Full;
            Delete(Message.all);
            Free(Message);
            Message := null;
         else
            pragma Assume(Valid(Q.all));
            Put(Q.all, Message);
            Message_Waiting := True;
            Status          := Accepted;
         end if;
         pragma Unused(Message);
      end Send;

      procedure Unchecked_Send (Message : in out Msg_Owner) is
      begin
         if Q = null or else Is_Full(Q.all) then
            Delete(Message.all);
            Free(Message);
            Message := null;
         else
            pragma Assume(Valid(Q.all));
            Put(Q.all, Message);
            Message_Waiting := True;
         end if;

         pragma Unused(Message);
      end Unchecked_Send;

      function Message_Count return Message_Count_Type is
      begin
         if Q = null then
            return 0;
         else
            pragma Assume(Valid(Q.all));
            return Count(Q.all);
         end if;
      end Message_Count;

      entry Receive (Message : out Message_Record) when Message_Waiting is
         Ptr : Msg_Owner;
      begin
         pragma Assume(if Message_Waiting then Q /= null);
         --pragma Assume(Payload(Peek(Q.all)) /= null);

         Next(Q.all, Ptr);
         Message := Ptr.all;
         Free(Ptr);

         if Is_Empty(Q.all) then
            Message_Waiting := False;
         end if;
      end Receive;

      procedure Set_Queue_Size (Size : in Natural) is
      begin
         if Q /= null then
            return;
         end if;
         Q := new Message_Queue(Size);
      end Set_Queue_Size;

   end Sync_Mailbox;

   procedure Delete(Msg : in out Mutable_Message_Record)
     with SPARK_Mode => Off
   is
   begin
      -- We lie to SPARK here to hide the fact
      -- that technically Free is a blocking function.
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

   procedure Delete(Msg : in out Message_Record)
     with SPARK_Mode => Off
   is
   begin
      -- We lie to spark about this because Free shouldn't be
      -- considered a blocking function.
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
        Domain_ID_Type'Image (Message.Receiver_Address.Domain_ID);
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

   function Receives(Receiver : access constant Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     is (Receives(Receiver.all, Msg_Type));

   function Receives(Receiver : Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     is (for some T of Receiver.Spec.Receive_Types.all => T = Msg_Type);

   function Receives(Receiver : Module_ID_Type; Msg_Type : Universal_Message_Type) return Boolean
   is (True);

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

   procedure Wait is
   begin
      Init_Lock.Wait;
   end Wait;

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

   function Address(Box : Module_Mailbox) return Module_ID_Type is
     (Box.Spec.Module_ID);

   procedure Send_Message(Box : Module_Mailbox;
                          Msg : in out Message_Record;
                          Target_Module : Module_Metadata;
                          Target_Domain : Domain_Declaration := This_Domain
                         )
     with Refined_Post => Msg.Payload = null
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Ptr.Sender_Address := (Domain_ID, Box.Spec.Module_ID);
      Ptr.Receiver_Address := (Target_Domain.ID, Target_Module.Module_ID);
      Route_Message (Ptr);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Send_Message (Box : Module_Mailbox; Msg : in out Message_Record)
     with Refined_Post => Msg.Payload = null
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Ptr.Sender_Address := (Domain_ID, Box.Spec.Module_ID);
      Route_Message (Ptr);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Send_Message
     (Box : Module_Mailbox; Msg : in out Message_Record; Status : out Status_Type)
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Ptr.Sender_Address := (Domain_ID, Box.Spec.Module_ID);
      Route_Message (Ptr, Status);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Read_Next (Box : Module_Mailbox; Msg : out Message_Record) is
   begin
      Fetch_Message (Box.Spec.Module_ID, Msg);
   end Read_Next;

   procedure Queue_Size(Box : Module_Mailbox; Size : out Natural) is
   begin
      Size := Message_Storage (Box.Spec.Module_ID).Message_Count;
   end Queue_Size;

   function Make_Module_Mailbox(Module_ID : in Module_ID_Type;
                                Spec : Module_Metadata) return Module_Mailbox
   is (Module_ID, Spec);

   procedure Register_Module(Mailbox : in Module_Mailbox;
                             Msg_Queue_Size : in Positive)
   is
   begin
      -- Create a new mailbox for the ID
      Message_Storage (Address(Mailbox)).Set_Queue_Size (Msg_Queue_Size);

      Init_Lock.Unlock(Address(Mailbox));
   end Register_Module;

   procedure Get_Message_Counts (Count_Array : out Message_Count_Array)
     with Refined_Global => (In_Out => Message_Storage)
   is
   begin
      for I in Module_ID_Type loop
         Count_Array (I) := Message_Storage (I).Message_Count;
      end loop;
   end Get_Message_Counts;
end CubedOS.Generic_Message_Manager;
