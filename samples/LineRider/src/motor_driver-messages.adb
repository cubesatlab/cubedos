--------------------------------------------------------------------------------
-- FILE   : motor_driver-messages.adb
-- SUBJECT: Body of the motor driver message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;
with Motor_Driver.Internals;

package body Motor_Driver.Messages is

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      -- Initialize the motors before trying to process any messages for them.
      Internals.Initialize;

      -- Process messages as they arrive.
      loop
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         Internals.Process_Message(Incoming_Message);
      end loop;
   end Message_Loop;

end Motor_Driver.Messages;
