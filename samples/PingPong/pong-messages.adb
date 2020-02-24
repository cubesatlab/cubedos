--------------------------------------------------------------------------------
-- FILE   : pong-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ping.API;
with Ping;
with Pong.API;
with Ada.Integer_Text_IO;

package body Pong.Messages is
   use Message_Manager;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ponged(I : in Integer; Message : in Message_Record)
     with Pre => Pong.API.Is_Ponged(Message)
   is
      Outgoing_Message : Message_Record;
   begin
      Pong.API.Ponged_Decode(Message);
      Ada.Text_IO.Put("+++ Pong ");
      Ada.Integer_Text_IO.Put(Item  => I,
                              Width => 0,
                              Base  => 10);

      Put(" Request ID: " & Request_ID_Type'Image (Message.Request_ID));

      Ada.Text_IO.Put_Line(" : Received PONGED");

      -- Wait a bit.
      -- delay(2.5);

      -- Send a Grabbed message to Ping.
      Outgoing_Message := Ping.API.Pinged_Encode(Sender_Domain => Domain_ID, Sender => ID, Priority => System.Default_Priority, Request_ID => R_ID);
      Message_Manager.Route_Message(Outgoing_Message);
   end Handle_Ponged;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(I : in Integer; Message : in Message_Record) is
   begin
      if Pong.API.Is_Ponged(Message) then
         Handle_Ponged(I, Message);
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
      I : Integer := 0; -- Do we want to keep track only when entering a dependent function
   begin                -- such as Handle_Ponged, or from the main task loop??
      loop
         I := I + 1;
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(I, Incoming_Message);
      end loop;
   end Message_Loop;

end Pong.Messages;
