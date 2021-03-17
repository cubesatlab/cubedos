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
with Ada.Real_Time;

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

      Put(" | Request ID: " & Request_ID_Type'Image (Message.Request_ID));

      Ada.Text_IO.Put_Line(" | Received PONGED");

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
      
      Start_Time : Ada.Real_Time.Time;
      Relative_Time : Ada.Real_Time.Time_Span;
      Relative_Duration : Duration;
      Total_Time : Duration := 0.000000000;

      
      use type Ada.Real_Time.Time;
      package Duration_IO is new Fixed_IO(Duration);
      use Duration_IO;
      
   begin                -- such as Handle_Ponged, or from the main task loop??
      I := 1;
      while I /= 10 loop
         Start_Time := Ada.Real_Time.Clock;
         
         -- Fetch and Route
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(I, Incoming_Message);
         
         Relative_Time := Ada.Real_Time.Clock - Start_Time;
         Relative_Duration := Ada.Real_Time.To_Duration(Relative_Time);
         Total_Time := Total_Time + Ada.Real_Time.To_Duration(Relative_Time);
         Put("Pong"); Put(I'Image); Put(" Time Duration:   "); Put(Relative_Duration); New_Line;
         
         I := I + 1;
      end loop;
      delay 1.0;
      New_Line(2);
      Put("---");New_Line;
      Put("Total Pong Time : "); Put(Total_Time); New_Line;
      Put("---");
      New_Line(2);
   end Message_Loop;

end Pong.Messages;
