

-- The "last chance handler" is the user-defined routine that is called when an exception is
-- propagated. We need it in the executable, therefore it must be somewhere in the closure of
-- the context clauses.
--
with Last_Chance_Handler;
pragma Unreferenced(Last_Chance_Handler);

with Sender.Messages;
pragma Unreferenced(Sender.Messages);

with Receiver.Messages;
pragma Unreferenced(Receiver.Messages);

with LEDs;
with Message_Manager;
with System;

procedure Main is
   pragma Priority (System.Priority'First);

   Null_Message : Message_Manager.Message_Record;
begin
   -- Create an empty message.
   Null_Message.Sender := 1;  -- Never used so value doesn't matter.
   Null_Message.Data := (others => 0);
   Null_Message.Size := 0;

   -- Send a triggering message to the sender to start the process.
   -- The content of this message is irrelevant (it is ignored by the sender).
   LEDs.On(LEDs.Blue);
   Message_Manager.Mailboxes(Sender.ID).Unchecked_Send(Null_Message);

   -- After sending the message there is nothing useful to do.
   loop
      null;
   end loop;
end Main;
