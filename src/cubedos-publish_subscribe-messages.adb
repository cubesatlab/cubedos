--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib;
with CubedOS.Publish_Subscribe.API;
use  CubedOS.Publish_Subscribe.API;

package body CubedOS.Publish_Subscribe.Messages
  with Refined_State => (Database => Subscription_Map)
is
   use Message_Manager;

   type Subscription_Map_Type is array (Module_ID_Type, Channel_ID_Type) of Boolean
     with Pack;

   Subscription_Map : Subscription_Map_Type := (others => (others => False));

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
              (Message.Sender_Domain, Message.Sender, Message.Request_ID, Channel, Failure));
      else
         Subscription_Map(Message.Sender, Channel) := True;
         Route_Message
           (API.Subscribe_Reply_Encode
              (Message.Sender_Domain, Message.Sender, Message.Request_ID, Channel, Success));
      end if;
   end Handle_Subscribe_Request;


   procedure Handle_Publish_Request(Message : in Message_Record)
     with Pre => Is_Publish_Request(Message)
   is
      Channel : Channel_ID_Type;
      Message_Data : CubedOS.Lib.Octet_Array(1 .. 2048);  -- TODO: What size would be best?
      Size    : CubedOS.Lib.Octet_Array_Count;
      Status  : Message_Status_Type;
   begin
      Publish_Request_Decode(Message, Channel, Message_Data, Size, Status);
      if Status = Malformed then
         Route_Message
           (API.Publish_Reply_Encode
              (Message.Sender_Domain, Message.Sender, Message.Request_ID, Channel, Failure));
      else
         for I in Module_ID_Type loop
            if Subscription_Map(I, Channel) then
               Route_Message
                 (API.Publish_Result_Encode
                    (Domain_ID, I, 0, Channel, Message_Data(1 .. Size)));
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
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end CubedOS.Publish_Subscribe.Messages;
