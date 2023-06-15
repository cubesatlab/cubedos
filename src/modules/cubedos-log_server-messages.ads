--------------------------------------------------------------------------------
-- FILE   : cubedos.log_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with Ada.Text_IO;
with System;

package CubedOS.Log_Server.Messages is

private
   Mailbox : Message_Manager.Module_Mailbox;

   task Message_Loop
     with Global => (In_Out => (Ada.Text_IO.File_System, Message_Manager.Mailboxes, Message_Manager.Mailbox_Metadata, Mailbox))
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;
end CubedOS.Log_Server.Messages;
