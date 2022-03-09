--------------------------------------------------------------------------------
-- FILE   : networking_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--------------------------------------------------------------------------------
with Networking_Server.API;

package body Networking_Server.Messages is
   use Message_Manager;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ping_Request(Message : in Message_Record)
     with
       Pre => Networking_Server.API.Is_Ping_Request(Message)
   is
      Outgoing_Message : Message_Record;
      Decode_Status    : Message_Status_Type;
   begin
      Networking_Server.API.Ping_Request_Decode(Message, Decode_Status);

      -- Just ignore messages that don't decode properly (decoding Ping_Requests can't fail anyway).
      if Decode_Status = Message_Manager.Success then
         Outgoing_Message :=
           Networking_Server.API.Ping_Reply_Encode
             (Receiver_Domain => Message.Sender_Domain,
              Receiver        => Message.Sender,
              Request_ID      => Message.Request_ID,
              Status          => Networking_Server.API.Success);  -- Ping is always successful.
         Message_Manager.Route_Message(Outgoing_Message);
      end if;
   end Handle_Ping_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if Networking_Server.API.Is_Ping_Request(Message) then
         Handle_Ping_Request(Message);
      else
         -- An unknown message type has been received. What should be done about that?
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

end Networking_Server.Messages;
