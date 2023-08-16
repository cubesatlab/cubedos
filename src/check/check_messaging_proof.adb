---------------------------------------------------------------------------
-- FILE    : check_messaging_proof.adb
-- SUBJECT : Tests formal verification of messages.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--
-- This file should fail/pass spark proof inspection in the specified
-- places. It verifies that SPARK is correctly preventing modules
-- from sending messages to modules that can't accept them.
--
-- IT WILL NOT EXECUTE SUCCESFULLY, IT'S NOT SUPPOSED TO
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
   pragma Assert(Payload(Message) = null);

   -- Reading from a mailbox before the messaging system is ready is illegal
   Read_Next(Mailbox_1, Message);
   Delete(Message);

   pragma Assume(Has_Module(Message_Manager.This_Domain, Module_ID(Mailbox_1)));
   -- Register mailbox 1
   Register_Module(Mailbox_1, 1);

   Message_Manager.Skip_Mailbox_Initialization;

   -- Now reading is ok
   Read_Next(Mailbox_1, Message);
   Delete(Message);
   pragma Unused(Message);

end Check_Messaging_Proof;
