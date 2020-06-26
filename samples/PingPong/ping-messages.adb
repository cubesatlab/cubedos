-- FILE   : ping-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ping.API;
with Pong.API;
with Pong;
with Ada.Integer_Text_IO;
with Ada.Real_Time;



package body Ping.Messages is
   use Message_Manager;

   -- The initialization of the Ping module, starts the ball bouncing!
   procedure Initialize is
      Outgoing_Message : Message_Record;

   begin
      Outgoing_Message := Pong.API.Ponged_Encode
        (Sender_Domain => Domain_ID,
         Sender => ID,
         Priority => System.Default_Priority,
         Request_ID => R_ID);
      Message_Manager.Route_Message(Outgoing_Message);
   end Initialize;



   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Pinged(I : in Integer; Message : in Message_Record)
    with Pre => Ping.API.Is_Pinged(Message)

   is

      Outgoing_Message : Message_Record;

     -- m: Message.Request_ID;
   begin
      Ping.API.Pinged_Decode(Message);
      Ada.Text_IO.Put("+++ Ping ");
      Ada.Integer_Text_IO.Put(Item  => I,
                              Width => 0,
                              Base  => 10);

      Put(" | Request ID: " & Request_ID_Type'Image (Message.Request_ID));

      Ada.Text_IO.Put_Line(" | Received PINGED");

      -- Wait a bit.
      -- delay(2.5);

      -- Send a Grabbed message to Pong.
      Outgoing_Message := Pong.API.Ponged_Encode
        (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID);
      Message_Manager.Route_Message(Outgoing_Message);
   end Handle_Pinged;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(I : in Integer; Message : in Message_Record) is
   begin
      if Ping.API.Is_Pinged(Message) then
         Handle_Pinged(I, Message);
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
      I : Integer := 0;   -- Do we want to keep track only when entering a dependent function
      
      Start_Time : Ada.Real_Time.Time;
      Relative_Time : Ada.Real_Time.Time_Span;
      Relative_Duration : Duration;
      
      use type Ada.Real_Time.Time;
      
      package Duration_IO is new Fixed_IO(Duration);
      use Duration_IO;
      

   begin                  -- such as Handle_Pinged, or from the main task loop??
      Initialize;
      loop
         I := I + 1;
         New_Line(2);
         Start_Time := Ada.Real_Time.Clock;
         
         -- Fetch and Route
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(I, Incoming_Message);
         
         Relative_Time := Ada.Real_Time.Clock - Start_Time;
         Relative_Duration := Ada.Real_Time.To_Duration(Relative_Time);
         Put("Ping"); Put(I'Image); Put(" Time Duration:   ");Put(Relative_Duration); New_Line;
                  
      end loop;
   end Message_Loop;

end Ping.Messages;

