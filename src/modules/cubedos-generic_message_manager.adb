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

with CubedOS.Message_Types;
private with CubedOS.Message_Types; use CubedOS.Message_Types;

package body CubedOS.Generic_Message_Manager with
Refined_State => (Mailboxes => Message_Storage,
                  Lock => Init_Lock,
                  Request_ID_Generator => Request_ID_Gen)
is

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
      entry Receive (Message : out Message_Record);

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
            Delete(Message);
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
            Delete(Message);
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
         Delete(Ptr);

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
      Message_Storage (Receiver_Address(Message).Module_ID).Send
        (Message, Status);
   end Route_Message;

   procedure Route_Message (Message : in out Msg_Owner) is
   begin
      if Receiver_Address(Message).Domain_ID /= Domain_ID then
         Send_Outgoing_Message(Message);
      else
         Message_Storage (Receiver_Address(Message).Module_ID).Unchecked_Send
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
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Route_Message (Ptr);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Send_Message (Box : Module_Mailbox; Msg : in out Message_Record)
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Route_Message (Ptr);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Send_Message
     (Box : Module_Mailbox; Msg : in out Message_Record; Status : out Status_Type)
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
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

   -- Gives a message received from a foreign domain to the message system.
   procedure Handle_Received(Msg : in out Msg_Owner) is
   begin
      Route_Message(Msg);
   end Handle_Received;

   procedure Get_Message_Counts (Count_Array : out Message_Count_Array)
     with Refined_Global => (In_Out => Message_Storage)
   is
   begin
      for I in Module_ID_Type loop
         Count_Array (I) := Message_Storage (I).Message_Count;
      end loop;
   end Get_Message_Counts;
end CubedOS.Generic_Message_Manager;
