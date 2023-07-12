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
with CubedOS.Message_Types; use CubedOS.Message_Types;

procedure Check_Messaging_Proof is

   Module_ID_1 : constant Module_ID_Type := 1;
   Metadata : constant Module_Metadata := Define_Module(Module_ID_1, Empty_Type_Array'Access);
   Mailbox_1 : constant Module_Mailbox := Make_Module_Mailbox(Module_ID_1, Metadata);
   Message : Message_Record;
begin

   -- Reading from a mailbox before it is registered is illegal
   Message_Manager.Read_Next(Mailbox_1, Message);

   -- Register mailbox 1
   Register_Module(Mailbox_1, 1);

   -- Now reading is ok
   Message_Manager.Read_Next(Mailbox_1, Message);

   -- But we should really be reading with the mailbox
   Read_Next(Mailbox_1, Message);

end Check_Messaging_Proof;
