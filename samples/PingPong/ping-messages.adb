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
   Start_Time        : Ada.Real_Time.Time;
   Relative_Time     : Ada.Real_Time.Time_Span;
   Relative_Duration : Duration;
   Total_Time        : Duration := 0.000_000_000;
   i                 : Integer  := 0;

   -- The initialization of the Ping module, starts the ball bouncing!
   procedure Initialize is
      Outgoing_Message : Message_Record;

   begin
      Start_Time := Ada.Real_Time.Clock;
      -- send an empty message to pong to begin the ball rolling. This message requests a
      -- return
      Outgoing_Message :=
        Pong.API.Init_Encode
          (Sender_Domain => Domain_ID, Sender => ID,
           Priority      => System.Default_Priority, Request_ID => R_ID);
      Message_Manager.Route_Message (Outgoing_Message);
      Outgoing_Message :=
        Pong.API.Ponged_Encode
          (Sender_Domain => Domain_ID, Sender => ID,
           Priority      => System.Default_Priority, Request_ID => R_ID,
           Send_Return   => True);
      Message_Manager.Route_Message (Outgoing_Message);

   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Pinged (Message : in Message_Record) with
      Pre => Ping.API.Is_Pinged (Message)
   is
      use type Ada.Real_Time.Time;
      package Duration_IO is new Fixed_IO (Duration);
      use Duration_IO;
      Outgoing_Message : Message_Record;
      MAX              : constant Integer := 10;
      Message_Val      : Boolean;
   begin
      -- Begin by decoding the message to see if there is any information in it
      Ping.API.Pinged_Decode (Message, Message_Val);
      -- Do math on time spent on message (possibly faster by saving till end?)
      Relative_Time     := Ada.Real_Time.Clock - Start_Time;
      Relative_Duration := Ada.Real_Time.To_Duration (Relative_Time);
      Total_Time := Total_Time + Ada.Real_Time.To_Duration (Relative_Time);

      -- Print information about the ping
      Ada.Text_IO.Put ("+++ Ping ");
      Ada.Integer_Text_IO.Put (Item => i, Width => 0, Base => 10);

      Put (" | Request ID: " & Request_ID_Type'Image (Message.Request_ID));

      Ada.Text_IO.Put_Line (" | Received PINGED");
      Put ("Ping");
      Put (i'Image);
      Put (" Time Duration:   ");
      Put (Relative_Duration);
      New_Line;
      -- Wait a bit.
      -- delay(2.5);

      -- if max pings has not been reached, tell pong we want a response
      if i < MAX then
         Outgoing_Message :=
           Pong.API.Ponged_Encode
             (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
              Priority      => System.Default_Priority, Send_Return => True);
      else
         -- otherwise tell pong we don't want a response, thus we react to no more messages.
         Outgoing_Message :=
           Pong.API.Ponged_Encode
             (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
              Priority      => System.Default_Priority, Send_Return => False);
         Put ("Ending Comms");
         New_Line;
      end if;
      Message_Manager.Route_Message (Outgoing_Message);
      Start_Time := Ada.Real_Time.Clock;
      i          := i + 1;
   end Handle_Pinged;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process (Message : in Message_Record) is
   begin
      if Ping.API.Is_Pinged (Message) then
         Handle_Pinged (Message);
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
   begin                  -- such as Handle_Pinged, or from the main task loop??
      Initialize;
      loop
-- Halt until message available, then route the message to where it needs to go
         Message_Manager.Fetch_Message (ID, Incoming_Message);

         Process (Incoming_Message);
      end loop;
   end Message_Loop;

end Ping.Messages;
