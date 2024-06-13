--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.adb
-- SUBJECT: Body of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

-- with Name_Resolver;

package body CubedOS.Generic_Message_Manager
  with Refined_State => (Mailboxes => Message_Storage, Request_ID_Generator => Request_ID_Gen)
is

   -- A protected object for generating request ID values.
   protected Request_ID_Gen is
      procedure Generate_Next_ID(Request_ID : out Request_ID_Type);
   private
      Next_Request_ID : Request_ID_Type := 1;
   end Request_ID_Gen;

  -- A protected type for holding messages.
   protected type Mailbox is

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the returned status indicates this.
      procedure Send(Message : in Message_Record; Status : out Status_Type);

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the message is lost.
      procedure Unchecked_Send(Message : in Message_Record);

      -- Returns the number of messages in the mailbox.
      function Message_Count return Message_Count_Type;

      -- Receive a message. This entry waits indefinitely for a message to be available.
      entry Receive(Message : out Message_Record);

   private
      Messages : Message_Array;
      Count    : Message_Count_Type := 0;
      Next_In  : Message_Index_Type := 1;
      Next_Out : Message_Index_Type := 1;
      Message_Waiting : Boolean := False;
   end Mailbox;

   -- One mailbox for each module.
   Message_Storage : array(Module_ID_Type) of Mailbox;

   ------------------
   -- Implementations
   ------------------

   protected body Request_ID_Gen is

      procedure Generate_Next_ID(Request_ID : out Request_ID_Type) is
      begin
         Request_ID := Next_Request_ID;
         Next_Request_ID := Next_Request_ID + 1;
      end Generate_Next_ID;

   end Request_ID_Gen;


   protected body Mailbox is

      procedure Send(Message : in Message_Record; Status : out Status_Type) is
      begin
         if Count = Mailbox_Size then
            Status := Mailbox_Full;
         else
            Messages(Next_In) := Message;
            if Next_In = Mailbox_Size then
               Next_In := 1;
            else
               Next_In := Next_In + 1;
            end if;
            Count := Count + 1;
            Message_Waiting := True;
            Status := Accepted;
         end if;
      end Send;


      procedure Unchecked_Send(Message : in Message_Record) is
      begin
         if Count /= Mailbox_Size then
            Messages(Next_In) := Message;
            if Next_In = Mailbox_Size then
               Next_In := 1;
            else
               Next_In := Next_In + 1;
            end if;
            Count := Count + 1;
            Message_Waiting := True;
         end if;
      end Unchecked_Send;


      function Message_Count return Message_Count_Type is
      begin
         return Count;
      end Message_Count;


      entry Receive(Message : out Message_Record) when Message_Waiting is
      begin
         Message := Messages(Next_Out);
         if Next_Out = Mailbox_Size then
            Next_Out := 1;
         else
            Next_Out := Next_Out + 1;
         end if;
         Count := Count - 1;
         if Count = 0 then
            Message_Waiting := False;
         end if;
      end Receive;

   end Mailbox;


   function Make_Empty_Message
     (Sender_Address   : in Message_Address;
      Receiver_Address : in Message_Address;
      Request_ID : in Request_ID_Type;
      Message_ID : in Message_ID_Type;
      Priority   : in System.Priority := System.Default_Priority) return Message_Record
   is
      Message : Message_Record;
   begin
      Message.Sender_Address := Sender_Address;
      Message.Receiver_Address   := Receiver_Address;
      Message.Request_ID := Request_ID;
      Message.Message_ID := Message_ID;
      Message.Priority   := Priority;
      Message.Size       := 0;
      Message.Payload := (others => 0);
      return Message;
   end Make_Empty_Message;


   function Stringify_Message (Message : in Message_Record) return String is
      Sender_Domain_String : constant String := Domain_ID_Type'Image(Message.Sender_Address.Domain_ID);
      Sender_Module_String : constant String := Module_ID_Type'Image(Message.Sender_Address.Module_ID);
      Receiver_Domain_String : constant String := Module_ID_Type'Image(Message.Receiver_Address.Domain_ID);
      Receiver_Module_String : constant String := Module_ID_Type'Image(Message.Receiver_Address.Module_ID);
      Request_ID_String : constant String := Request_ID_Type'Image(Message.Request_ID);
      Message_ID_String : constant String := Message_ID_Type'Image(Message.Message_ID);
      Message_Image     : constant String := Sender_Domain_String & "!" & Sender_Module_String & "!" &
                                             Receiver_Domain_String & "!" &  Receiver_Module_String & "!" &
                                             Request_ID_String & "!" & Message_ID_String & "!";
   begin
      return Message_Image;
   end Stringify_Message;


   procedure Get_Next_Request_ID(Request_ID : out Request_ID_Type) is
   begin
      Request_ID_Gen.Generate_Next_ID(Request_ID);
   end Get_Next_Request_ID;


   procedure Route_Message(Message : in Message_Record; Status : out Status_Type) is
   begin
      -- For now, let's ignore the domain and just use the receiver Module_ID only.
      Message_Storage(Message.Receiver_Address.Module_ID).Send(Message, Status);
   end Route_Message;


   procedure Route_Message(Message : in Message_Record) is
   begin
      if Message.Receiver_Address.Domain_ID /= Domain_ID then
        -- Circular Dependency with Name_Resolver so resorting to hardcoding
        -- Message_Storage(Name_Resolver.Network_Server.Module_ID).Unchecked_Send(Message);
         Message_Storage(2).Unchecked_Send(Message);
      else
         Message_Storage(Message.Receiver_Address.Module_ID).Unchecked_Send(Message);
      end if;
   end Route_Message;

   procedure Fetch_Message(Module : in Module_ID_Type; Message : out Message_Record) is
   begin
      Message_Storage(Module).Receive(Message);
   end Fetch_Message;


   procedure Get_Message_Counts(Count_Array : out Message_Count_Array) is
   begin
      for I in Module_ID_Type loop
         Count_Array(I) := Message_Storage(I).Message_Count;
      end loop;
   end Get_Message_Counts;

end CubedOS.Generic_Message_Manager;
