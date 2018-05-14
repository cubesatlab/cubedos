--------------------------------------------------------------------------------
-- FILE   : led_driver-messages.adb
-- SUBJECT: Body of the LED driver message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with LED_Driver.Internals;
with Message_Manager;

package body LED_Driver.Messages is

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      -- Initialize the LEDs before trying to process any messages for them.
      Internals.Initialize;
      loop
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         Internals.Process_Message(Incoming_Message);
      end loop;
   end Message_Loop;

end LED_Driver.Messages;
