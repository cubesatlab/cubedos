---------------------------------------------------------------------------
-- FILE    : check_messaging_proof.adb
-- SUBJECT : Tests formal verification of messages.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--
-- This file should fail/pass spark proof inspection in the specified
-- places. It verifies that SPARK is correctly preventing modules
-- from sending messages to modules that can't accept them.
--
---------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Message_Manager;
use Message_Manager;

procedure Check_Messaging_Proof is

   Module_ID_1 : constant Module_ID_Type := 1;
   Mailbox_1 : Module_Mailbox;
   Message : Message_Record;
begin

   -- Reading from a mailbox before it is registered is illegal
   Message_Manager.Fetch_Message(Module_ID_1, Message);

   -- Register mailbox 1
   Register_Module(Module_ID_1, 1, Mailbox_1, Empty_Type_Array);

   -- Now reading is ok
   Message_Manager.Fetch_Message(Module_ID_1, Message);

   -- But we should really be reading with the mailbox
   Read_Next(Mailbox_1, Message);

end Check_Messaging_Proof;
