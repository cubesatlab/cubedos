--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.adb
-- SUBJECT: Body of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Message_Types.Message_Queues; use CubedOS.Message_Types.Message_Queues;
with Domain_Config;

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

   type Module_Index is new Positive range 1 .. Module_Count;
   type Module_Init_List is array (Module_Index) of Boolean
     with Default_Component_Value => False;
   type Module_Init_List_Owner is access Module_Init_List;

   protected Init_Lock is
      function Is_Locked return Boolean;
      entry Wait;
      procedure Unlock (Module : Module_ID_Type)
        with Pre => Has_Module(This_Domain, Module);
      procedure Unlock_Manual
        with Post => not Is_Locked;
   private
      Inited : Module_Init_List_Owner;
      Locked : Boolean := True;
   end Init_Lock;

   type Message_Queue_Owner is access Message_Queues.Bounded_queue;

   subtype Message_Count_Type is Natural;

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
        --with Post => Is_Valid(Message);

      -- Change the array used to store messages.
      procedure Set_Queue_Size (Size : in Natural);

   private
      Q : Message_Queue_Owner := null;
      Message_Waiting : Boolean := False;
   end Sync_Mailbox;

   -- One mailbox for each module.
   Message_Storage : array (Module_ID_Type) of Sync_Mailbox;

   function Messaging_Ready return Boolean
   is (not Init_Lock.Is_Locked)
     with SPARK_Mode => Off,
     -- We hide this from SPARK because Init_Lock.Is_Locked doesn't have
     -- any meaningful interferences.
     Refined_Post => Messaging_Ready'Result = not Init_Lock.Is_Locked;

   procedure Skip_Mailbox_Initialization is
   begin
      Init_Lock.Unlock_Manual;
      pragma Assert(Messaging_Ready);
   end Skip_Mailbox_Initialization;


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
      function Is_Locked return Boolean
        is (Locked);
      entry Wait when not Locked is
      begin
         null;
      end Wait;
      procedure Unlock (Module : Module_ID_Type) is
         Index : Module_Index with Relaxed_Initialization;
         Set : Boolean := False;
      begin
         if Inited = null then
            Inited := new Module_Init_List;
         end if;

         -- Get index of given module
         for I in 1 .. Module_Count loop
            if This_Domain.Module_IDs(I) = Module then
               Index := Module_Index(I);
               Set := True;
               exit;
            end if;
         end loop;

         if Set then
            Inited(Index) := True;
         end if;

         if (for all I of Inited.all => I) then
            Locked := False;
         end if;
      end Unlock;
      procedure Unlock_Manual is
      begin
         Locked := False;
      end Unlock_Manual;
   end Init_Lock;

   protected body Sync_Mailbox is

      procedure Send (Message : in out Msg_Owner; Status : out Status_Type) is
      begin
         if Q = null or else Is_Full(Q.all) then
            Status := Mailbox_Full;
            Delete(Message);
            Message := null;
         else
            Put(Q.all, Message);
            Message_Waiting := True;
            Status          := Accepted;
            Message := null;
         end if;
         pragma Unused(Message);
      end Send;

      procedure Unchecked_Send (Message : in out Msg_Owner) is
      begin
         if Q = null or else Is_Full(Q.all) then
            Delete(Message);
            Message := null;
         else
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
            return Count(Q.all);
         end if;
      end Message_Count;

      entry Receive (Message : out Message_Record) when Message_Waiting is
         Ptr : Msg_Owner;
      begin
         pragma Assume(if Message_Waiting then Q /= null);
         pragma Assume(if Message_Waiting then not Is_Empty(Q.all));

         Next(Q.all, Ptr);
         Copy(Ptr.all, Message);
         Delete(Ptr);
         pragma Unused(Ptr);

         if Is_Empty(Q.all) then
            Message_Waiting := False;
         end if;
      end Receive;

      procedure Set_Queue_Size (Size : in Natural) is
      begin
         if Q = null then
            Q := new Message_Queues.Bounded_Queue(Size);
         end if;
      end Set_Queue_Size;

   end Sync_Mailbox;

   procedure Get_Next_Request_ID (Request_ID : out Request_ID_Type) is
   begin
      Request_ID_Gen.Generate_Next_ID (Request_ID);
   end Get_Next_Request_ID;

   function Receives(Receiver : Module_Mailbox; Msg_Type : Universal_Message_Type) return Boolean
     is (Receives(Spec(Receiver), Msg_Type));

   procedure Route_Message
     (Message : in out Msg_Owner; Status : out Status_Type)
     with
       Pre => Message /= null
       and then Payload(Message) /= null
       and then Messaging_Ready,
       Post => Message = null
   is
   begin
      Domain_Config.On_Message_Sent_Debug(Message.all);

      -- For now, let's ignore the domain and just use the receiver Module_ID only.
      Message_Storage (Receiver_Address(Message).Module_ID).Send
        (Message, Status);
   end Route_Message;

   procedure Route_Message (Message : in out Msg_Owner)
     with
       Pre => Message /= null
       and then Payload(Message) /= null
       and then Messaging_Ready,
       Post => Message = null
   is
   begin
      Domain_Config.On_Message_Sent_Debug(Message.all);
      if Receiver_Address(Message).Domain_ID /= Domain_ID then
         Domain_Config.Send_Outgoing_Message(Message);
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
      pragma Assert(Messaging_Ready);
   end Wait;

   -------------
   -- Mailbox
   -------------

   procedure Send_Message(Box : Module_Mailbox;
                          Msg : in out Message_Record;
                          Target_Module : Module_Metadata;
                          Target_Domain : Domain_Metadata := This_Domain
                         )
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Route_Message (Ptr);
      pragma Unreferenced(Box, Target_Module, Target_Domain);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Send_Message (Box : Module_Mailbox; Msg : in out Message_Record)
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Route_Message (Msg);
      pragma Unreferenced(Box);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Send_Message
     (Box : Module_Mailbox; Msg : in out Message_Record; Status : out Status_Type)
   is
      Ptr : Msg_Owner;
   begin
      Move(Msg, Ptr);
      Route_Message (Ptr, Status);
      pragma Unreferenced(Box);
      pragma Unused(Ptr);
   end Send_Message;

   procedure Read_Next (Box : Module_Mailbox; Msg : out Message_Record) is
      Result : Message_Record;
   begin
      Message_Storage (Box.Spec.Module_ID).Receive (Result);

      -- Don't allow a mailbox to read a message it can't receive.
      while not Receives(Spec(Box), Message_Type(Result)) or Payload(Result) = null loop
         Delete(Result);
         Message_Storage (Box.Spec.Module_ID).Receive (Result);
         pragma Loop_Invariant(Payload(Result) /= null);
      end loop;
      pragma Assert(Receives(Spec(Box), Message_Type(Result)));
      Msg := Result;
      pragma Assert(Receives(Spec(Box), Message_Type(Msg)));
   end Read_Next;

   procedure Pending_Messages(Box : Module_Mailbox; Size : out Natural) is
   begin
      Size := Message_Storage (Box.Spec.Module_ID).Message_Count;
   end Pending_Messages;

   procedure Register_Module(Mailbox : in Module_Mailbox;
                             Msg_Queue_Size : in Positive)
   is
   begin
      -- Create a new mailbox for the ID
      Message_Storage (Module_ID(Mailbox)).Set_Queue_Size (Msg_Queue_Size);

      Init_Lock.Unlock(Module_ID(Mailbox));
   end Register_Module;

   -- Gives a message received from a foreign domain to the message system.
   procedure Handle_Received(Msg : in out Msg_Owner) is
   begin
      Route_Message(Msg);
   end Handle_Received;

end CubedOS.Generic_Message_Manager;
