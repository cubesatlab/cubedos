--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with CubedOS.Publish_Subscribe_Server.API;
use  CubedOS.Publish_Subscribe_Server.API;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package body CubedOS.Publish_Subscribe_Server.Messages
  with Refined_State => (Database => Subscription_Map)
is

   Mailbox : constant Module_Mailbox := Make_Module_Mailbox(This_Module, Mail_Target);

   -- If this is really packed, the memory cost is minimal.
   type Subscription_Map_Type is array (Module_ID_Type, Channel_ID_Type) of Boolean
     with Pack;

   Subscription_Map : Subscription_Map_Type := (others => (others => False));

   procedure Init is
   begin
      Message_Manager.Register_Module(Mailbox, 8);
   end Init;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Subscribe_Request(Message : in Message_Record)
     with Pre => Is_Subscribe_Request(Message) and Is_Valid(Message)
   is
      Channel : Channel_ID_Type;
      Status  : Message_Status_Type;
   begin
      Subscribe_Request_Decode(Message, Channel, Status);
      if Status = Success then
         -- Should we have the Subscription_Map handle the entire Message Address?
         Subscription_Map(Sender_Address(Message).Module_ID, Channel) := True;
      end if;
        API.Send_Subscribe_Reply
           (Mailbox, Sender_Address(Message), Request_ID(Message), Channel, (if Status=Success then Success else Failure));
   end Handle_Subscribe_Request;


   procedure Handle_Unsubscribe_Request(Message : in Message_Record)
     with Pre => Is_Unsubscribe_Request(Message) and Is_Valid(Message)
   is
      Channel : Channel_ID_Type;
      Status  : Message_Status_Type;
   begin
      Unsubscribe_Request_Decode(Message, Channel, Status);
      if Status = Success then
         -- Notice that unsubscribing from a channel you are not subscribed to is not an error.
         -- The operation returns Success without comment.
         -- TODO: Is this appropriate?
         Subscription_Map(Sender_Address(Message).Module_ID, Channel) := False;
      end if;

      API.Send_Unsubscribe_Reply
        (Mailbox, Sender_Address(Message), Request_ID(Message), Channel,
        (if Status = Success then Success else Failure));
   end Handle_Unsubscribe_Request;


   procedure Handle_Publish_Request(Message : in Message_Record)
     with Pre => Is_Publish_Request(Message) and Is_Valid(Message)
   is
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array(1 .. Payload(Message)'Length - 8);
      Size    : CubedOS.Lib.Octet_Array_Count;
      Status  : Message_Status_Type;
   begin
      Publish_Request_Decode(Message, Channel, Message_Data, Size, Status);
      if Status = Malformed then
         API.Send_Publish_Reply
              (Mailbox, Sender_Address(Message), Request_ID(Message), Channel, Failure);
      else
         API.Send_Publish_Reply
              (Mailbox, Sender_Address(Message), Request_ID(Message), Channel, Success);

         -- Do the actual publishing.
         for I in Module_ID_Type loop
            if Subscription_Map(I, Channel) then
               API.Send_Publish_Result
                 (Mailbox,
                  --TODO: Record the domain of subscribers
                  (0, I),
                  0,
                  Channel,
                  Message_Data(1 .. Size));
            end if;
         end loop;
      end if;
   end Handle_Publish_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   procedure Process(Message : in Message_Record)
     with Pre => Is_Valid(Message)
   is
   begin
      if Is_Subscribe_Request(Message) then
         Handle_Subscribe_Request(Message);
      elsif Is_Unsubscribe_Request(Message) then
         Handle_Unsubscribe_Request(Message);
      elsif Is_Publish_Request(Message) then
         Handle_Publish_Request(Message);
      else
         -- TODO: An unexpected message type has been received. What should be done about that?
         null;
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Record;
   begin
      Message_Manager.Wait;

      loop
         Read_Next(Mailbox, Incoming_Message);
         Process(Incoming_Message);
         Delete(Incoming_Message);
         pragma Loop_Invariant(Payload(Incoming_Message) = null);
      end loop;
   end Message_Loop;

end CubedOS.Publish_Subscribe_Server.Messages;
