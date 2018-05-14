--------------------------------------------------------------------------------
-- FILE   : sender-internals.adb
-- SUBJECT: Body of a package that implements the main part of the sender.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with LEDs;
with Receiver;
with Receiver.API;

package body Sender.Internals is
   use type Message_Manager.Status_Type;

   procedure Process_Message(Incoming_Message : in Message_Manager.Message_Record) is
      pragma Unreferenced(Incoming_Message);

      Status : Message_Manager.Status_Type;
   begin
      for I in 1 .. 1_000_000 loop
         Message_Manager.Mailboxes(Receiver.ID).Send(Receiver.API.Make_Message(ID), Status);
         if Status = Message_Manager.Mailbox_Full then
            LEDs.On(LEDs.Red);
         end if;
      end loop;
   end Process_Message;

end Sender.Internals;

