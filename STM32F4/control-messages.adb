--------------------------------------------------------------------------------
-- FILE   : control-messages.adb
-- SUBJECT: Body of a package that implements the controller message loop.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;
with CubedOS.Tick_Generator.API;
use  CubedOS;

with LED_Driver.API;

package body Control.Messages is
   use Message_Manager;

   -------------------
   -- Message Handling
   -------------------

   type Pattern_Index_Type is mod 4;
   Pattern : constant array(Pattern_Index_Type) of LED_Driver.LED_Type :=
     (LED_Driver.Green, LED_Driver.Orange, LED_Driver.Red, LED_Driver.Blue);
   Current_LED : Pattern_Index_Type := Pattern_Index_Type'Last;  -- Incremented before used.

   procedure Handle_Tick_Reply(Message : in Message_Record)
     with Pre => Tick_Generator.API.Is_Tick_Reply(Message)
   is
      Outgoing_Message : Message_Record;
   begin
      Outgoing_Message := LED_Driver.API.Off_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         LED           => Pattern(Current_LED));
      Route_Message(Outgoing_Message);

      Current_LED := Current_LED + 1;

      Outgoing_Message := LED_Driver.API.On_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         LED           => Pattern(Current_LED));
      Route_Message(Outgoing_Message);
   end Handle_Tick_Reply;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if Tick_Generator.API.Is_Tick_Reply(Message) then
         Handle_Tick_Reply(Message);
      else
         -- Drop unexpected messages on the floor.
         null;
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Record;
      Outgoing_Message : Message_Record;
   begin
      -- Initialization...

      -- Make sure all the LEDs are off.
      Outgoing_Message := LED_Driver.API.All_Off_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0);
      Route_Message(Outgoing_Message);

      -- Request a periodic tick every 1.0 seconds.
      Outgoing_Message := Tick_Generator.API.Relative_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Tick_Interval => Ada.Real_Time.To_Time_Span(1.0),
         Request_Type  => Tick_Generator.API.Periodic,
         Series_ID     => 1);
      Route_Message(Outgoing_Message);

      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Control.Messages;
