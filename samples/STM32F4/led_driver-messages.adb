--------------------------------------------------------------------------------
-- FILE   : led_driver-messages.adb
-- SUBJECT: Body of a package that implements the LED driver.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with LEDs;
with LED_Driver.API;
with Name_Resolver;

package body LED_Driver.Messages is
   use Message_Manager;

   -- A look up table that maps my abstract representation of an LED into that required by LEDs.
   LED_Lookup : constant array(LED_Type) of LEDs.User_LED :=
     [Green => LEDs.Green, Orange => LEDs.Orange, Red => LEDs.Red, Blue => LEDs.Blue];

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_On_Request(Message : in Message_Record)
     with Pre => LED_Driver.API.Is_On_Request(Message)
   is
      Status : Message_Status_Type;
      LED    : LED_Type;
   begin
      LED_Driver.API.On_Request_Decode(Message, Status, LED);
      if Status = Success then
         LEDs.On(LED_Lookup(LED));
      end if;
   end Handle_On_Request;


   procedure Handle_Off_Request(Message : in Message_Record)
     with Pre => LED_Driver.API.Is_Off_Request(Message)
   is
      Status : Message_Status_Type;
      LED    : LED_Type;
   begin
      LED_Driver.API.Off_Request_Decode(Message, Status, LED);
      if Status = Success then
         LEDs.Off(LED_Lookup(LED));
      end if;
   end Handle_Off_Request;


   procedure Handle_All_On_Request(Message : in Message_Record)
     with Pre => LED_Driver.API.Is_All_On_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      LED_Driver.API.All_On_Request_Decode(Message, Status);
      if Status = Success then
         LEDs.All_On;
      end if;
   end Handle_All_On_Request;


   procedure Handle_All_Off_Request(Message : in Message_Record)
     with Pre => LED_Driver.API.Is_All_Off_Request(Message)
   is
      Status : Message_Status_Type;
   begin
      LED_Driver.API.All_Off_Request_Decode(Message, Status);
      if Status = Success then
         LEDs.All_Off;
      end if;
   end Handle_All_Off_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
      if LED_Driver.API.Is_On_Request(Message) then
         Handle_On_Request(Message);
      elsif LED_Driver.API.Is_Off_Request(Message) then
         Handle_Off_Request(Message);
      elsif LED_Driver.API.Is_All_On_Request(Message) then
         Handle_All_On_Request(Message);
      elsif LED_Driver.API.Is_All_Off_Request(Message) then
         Handle_All_Off_Request(Message);
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
         Message_Manager.Fetch_Message(Name_Resolver.LED_Driver.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end LED_Driver.Messages;
