--------------------------------------------------------------------------------
-- FILE   : sensor_driver-messages.adb
-- SUBJECT: Body of the sensor driver message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Tick_Generator;
with CubedOS.Tick_Generator.API;
with Message_Manager;
with Sensor_Driver.Internals;

package body Sensor_Driver.Messages is

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      -- Initialize the sensors before trying to process any messages for them.
      Internals.Initialize;

      -- Request a tick message every 100 ms.
      Message_Manager.Mailboxes(CubedOS.Tick_Generator.ID).Unchecked_Send
        (CubedOS.Tick_Generator.API.Periodic_Ticks_Message(ID, 100));

      -- Process the tick messages as they arrive.
      loop
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         Internals.Process_Message(Incoming_Message);
      end loop;
   end Message_Loop;

end Sensor_Driver.Messages;
