--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with CubedOS.Publish_Subscribe_Server.API;
with Message_Manager;
with Name_Resolver;
with CubedOS.Log_Server.API;

use  CubedOS.Publish_Subscribe_Server.API;

package body CubedOS.Publish_Subscribe_Server.Messages
  with Refined_State => (Database => Subscription_Map)
is
   use Message_Manager;

   -- If this is really packed, the memory cost is minimal.
   type Subscription_Map_Type is array (Module_ID_Type, Channel_ID_Type) of Boolean
     with Pack;

   Subscription_Map : Subscription_Map_Type := [others => [others => False]];

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Subscribe_Request(Message : in Message_Record)
     with Pre => Is_Subscribe_Request(Message)
   is
      Channel : Channel_ID_Type;
      Status  : Message_Status_Type;
   begin
      Subscribe_Request_Decode(Message, Channel, Status);
      if Status = Malformed then
         Route_Message
           (API.Subscribe_Reply_Encode
              (Message.Sender_Address, Message.Request_ID, Channel, Failure));
      else
         -- Should we have the Subscription_Map handle the entire Message Address?
         Subscription_Map(Message.Sender_Address.Module_ID, Channel) := True;
         Route_Message
           (API.Subscribe_Reply_Encode
              (Message.Sender_Address, Message.Request_ID, Channel, Success));
      end if;
   end Handle_Subscribe_Request;


   procedure Handle_Unsubscribe_Request(Message : in Message_Record)
     with Pre => Is_Unsubscribe_Request(Message)
   is
      Channel : Channel_ID_Type;
      Status  : Message_Status_Type;
   begin
      Unsubscribe_Request_Decode(Message, Channel, Status);
      if Status = Malformed then
         Route_Message
           (API.Unsubscribe_Reply_Encode
              (Message.Sender_Address, Message.Request_ID, Channel, Failure));
      else
         -- Notice that unsubscribing from a channel you are not subscribed to is not an error.
         -- The operation returns Success without comment.
         -- TODO: Is this appropriate?
         Subscription_Map(Message.Sender_Address.Module_ID, Channel) := False;
         Route_Message
           (API.Unsubscribe_Reply_Encode
              (Message.Sender_Address, Message.Request_ID, Channel, Success));
      end if;
   end Handle_Unsubscribe_Request;


   procedure Handle_Publish_Request(Message : in Message_Record)
     with Pre => Is_Publish_Request(Message)
   is
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array(1 .. Data_Size_Type'Last - 8);
      Size    : CubedOS.Lib.Octet_Array_Count;
      Status  : Message_Status_Type;
   begin
      Publish_Request_Decode(Message, Channel, Message_Data, Size, Status);
      if Status = Malformed then
         Route_Message
           (API.Publish_Reply_Encode
              (Message.Sender_Address, Message.Request_ID, Channel, Failure));
      else
         Route_Message
           (API.Publish_Reply_Encode
              (Message.Sender_Address, Message.Request_ID, Channel, Success));

         -- Do the actual publishing.
         for I in Module_ID_Type loop
            if Subscription_Map(I, Channel) then
               Route_Message
                 (API.Publish_Result_Encode
                    ((Name_Resolver.Publish_Subscribe_Server.Domain_ID, I),
                     0,
                     Channel,
                     Message_Data(1 .. Size)));
            end if;
         end loop;
      end if;
   end Handle_Publish_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   procedure Process(Message : in Message_Record) is
   begin
      if Is_Subscribe_Request(Message) then
         Handle_Subscribe_Request(Message);
      elsif Is_Unsubscribe_Request(Message) then
         Handle_Unsubscribe_Request(Message);
      elsif Is_Publish_Request(Message) then
         Handle_Publish_Request(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Publish_Subscribe_Server,
                                            CubedOS.Log_Server.API.Error,
                                            "An unexpected message type has been received!");
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Publish_Subscribe_Server.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end CubedOS.Publish_Subscribe_Server.Messages;
