--------------------------------------------------------------------------------
-- FILE   : sender-messages.adb
-- SUBJECT: Body of the sender message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
-- pragma SPARK_Mode(On);

with Message_Manager;
with Sender.Internals;

package body Sender.Messages is

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      loop
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         Internals.Process_Message(Incoming_Message);
      end loop;
   end Message_Loop;

end Sender.Messages;
