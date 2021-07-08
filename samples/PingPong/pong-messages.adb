--------------------------------------------------------------------------------
-- FILE   : pong-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ping.API;
with Ping;
with Pong.API;
with Ada.Integer_Text_IO;
with Ada.Real_Time;

package body Pong.Messages is
   use Message_Manager;
   Start_Time        : Ada.Real_Time.Time;
   Relative_Time     : Ada.Real_Time.Time_Span;
   Relative_Duration : Duration;
   Total_Time        : Duration := 0.000_000_000;
   i                 : Integer  := 0;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ponged (Message : in Message_Record) with
      Pre => Pong.API.Is_Ponged (Message)
   is
      use type Ada.Real_Time.Time;
      package Duration_IO is new Fixed_IO (Duration);
      use Duration_IO;
      Outgoing_Message : Message_Record;
      Message_Val      : Boolean;
   begin
      -- Begin by decoding the message to see if there is any information in it
      Pong.API.Ponged_Decode (Message, Message_Val);
      -- find time spend between sending and recieving message
      Relative_Time     := Ada.Real_Time.Clock - Start_Time;
      Relative_Duration := Ada.Real_Time.To_Duration (Relative_Time);
      Total_Time := Total_Time + Ada.Real_Time.To_Duration (Relative_Time);

      -- Print information about the ping (printing is very slow, remove if we want speed tests)
      Ada.Text_IO.Put ("+++ Pong ");
      Ada.Integer_Text_IO.Put (Item => i, Width => 0, Base => 10);

      Put (" | Request ID: " & Request_ID_Type'Image (Message.Request_ID));

      Ada.Text_IO.Put_Line (" | Received PONGED");
      Put ("Pong");
      Put (i'Image);
      Put (" Time Duration:   ");
      Put (Relative_Duration);
      New_Line;

      -- Send a Grabbed message to Ping if ping asked for one
      if Message_Val then
         Outgoing_Message :=
           Ping.API.Pinged_Encode
             (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
              Priority      => System.Default_Priority, Send_Return => True);
         Message_Manager.Route_Message (Outgoing_Message);
      end if;
      Start_Time := Ada.Real_Time.Clock;
      i          := i + 1;
   end Handle_Ponged;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process (Message : in Message_Record) is
   begin
      if Pong.API.Is_Ponged (Message) then
         Handle_Ponged (Message);
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
   begin                  -- such as Handle_Ponged, or from the main task loop??

      loop
-- Halt until message available, then route the message to where it needs to go
         Message_Manager.Fetch_Message (ID, Incoming_Message);
         Process (Incoming_Message);
      end loop;
   end Message_Loop;

end Pong.Messages;
