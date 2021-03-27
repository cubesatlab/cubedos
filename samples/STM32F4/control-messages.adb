 -------------------------------------------------------------------------------
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
with LED_Driver; use LED_Driver;
--with LEDs; -- Used for testing
with CubedOS.Publish_Subscribe_Server.API; use CubedOS.Publish_Subscribe_Server.API;

package body Control.Messages is
   use Message_Manager;

   -------------------
   -- Message Handling
   -------------------

   type Pattern_Index_Type is mod 4;
   Pattern : constant array(Pattern_Index_Type) of LED_Driver.LED_Type :=
     (LED_Driver.Green, LED_Driver.Orange, LED_Driver.Red, LED_Driver.Blue);
   Current_LED : Pattern_Index_Type := Pattern_Index_Type'Last;  -- Incremented before used.

   -- Counter for tick timer
   Counter : Integer := 1;



   procedure Handle_Subscribe_Result(Message : in Message_Record)
     with Pre => Publish_Subscribe_Server.API.Is_Publish_Result(Message)
   is
      Outgoing_Message: Message_Record;
   begin

     -- Cancel the tick request, set tick timer to 10.0 seconds
     if (Counter mod 3 = 0) then
      Outgoing_Message := Tick_Generator.API.Cancel_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Series_ID     => 1,
         Priority      => System.Default_Priority);
      Route_Message(Outgoing_Message);

      Outgoing_Message := Tick_Generator.API.Relative_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Tick_Interval => Ada.Real_Time.To_Time_Span(10.0),
         Request_Type  => Tick_Generator.API.Periodic,
         Series_ID     => 1);
      Route_Message(Outgoing_Message);
         Counter := Counter + 1;

   -- Cancel the tick request, set tick timer to 3.0 seconds
     elsif (Counter mod 3 = 1) then
      Outgoing_Message := Tick_Generator.API.Cancel_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Series_ID     => 1,
         Priority      => System.Default_Priority);
      Route_Message(Outgoing_Message);

      Outgoing_Message := Tick_Generator.API.Relative_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Tick_Interval => Ada.Real_Time.To_Time_Span(3.0),
         Request_Type  => Tick_Generator.API.Periodic,
         Series_ID     => 1);
      Route_Message(Outgoing_Message);
         Counter := Counter + 1;

     -- Cancel the tick request, set tick timer to 1.0 seconds
     elsif (Counter mod 3 = 2) then
      Outgoing_Message := Tick_Generator.API.Cancel_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Series_ID     => 1,
         Priority      => System.Default_Priority);
      Route_Message(Outgoing_Message);

      Outgoing_Message := Tick_Generator.API.Relative_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Tick_Interval => Ada.Real_Time.To_Time_Span(1.0),
         Request_Type  => Tick_Generator.API.Periodic,
         Series_ID     => 1);
      Route_Message(Outgoing_Message);
         Counter := Counter + 1;
   end if;

   end Handle_Subscribe_Result;



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
     -- LEDs.All_On;
   end Handle_Tick_Reply;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if Tick_Generator.API.Is_Tick_Reply(Message) then
         Handle_Tick_Reply(Message);

      elsif Publish_Subscribe_Server.API.Is_Publish_Result(Message) then
         Handle_Subscribe_Result(Message);

      elsif Publish_Subscribe_Server.API.Is_Subscribe_Reply(Message) then
         -- LEDs.All_On; -- Flash all the LEDs to indicate subscription to channel 1 (test)
         null;
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

      -- Turn blue LED on for init state
      Outgoing_Message := LED_Driver.API.On_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         LED           => LED_Driver.Blue);
      Route_Message(Outgoing_Message);

      -- Request a periodic tick every 10.0 seconds.
      Outgoing_Message := Tick_Generator.API.Relative_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Tick_Interval => Ada.Real_Time.To_Time_Span(10.0),
         Request_Type  => Tick_Generator.API.Periodic,
         Series_ID     => 1);
         Route_Message(Outgoing_Message);

         -- Subscribe to channel 1.
      Outgoing_Message := Subscribe_Request_Encode
        (Sender_Domain => 1,
         Sender        => Control.ID,
         Request_ID    => 0,
         Channel       => 1,
         Priority      => System.Default_Priority);
      Route_Message(Outgoing_Message);


      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Control.Messages;

