--------------------------------------------------------------------------------
-- FILE   : cubedos.log_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Jorvik);
pragma Partition_Elaboration_Policy(Sequential);

with Ada.Text_IO;
with System;
with CubedOS.Log_Server.API; use CubedOS.Log_Server.API;

package CubedOS.Log_Server.Messages is
   pragma Elaborate_Body;

   use Message_Manager;

   Public : Public_Mailbox_Owner with Constant_After_Elaboration;

   This_Receives: aliased constant Message_Type_Array := (0 => Log_Text_Msg);

   procedure Initialize;

   task Message_Loop
     with Global => (In_Out => (Ada.Text_IO.File_System, Message_Manager.Mailboxes, Message_Manager.Lock))
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;
end CubedOS.Log_Server.Messages;
