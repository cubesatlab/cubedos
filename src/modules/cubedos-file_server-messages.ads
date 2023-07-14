--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the file server.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Jorvik);
pragma Partition_Elaboration_Policy(Sequential);

with System;
with Message_Manager;
with CubedOS.File_Server.API;

package CubedOS.File_Server.Messages is
   use Message_Manager;
   use CubedOS.File_Server.API;

   -- Prepare to receive messages
   procedure Init
     with Global => (In_Out => (Mailboxes, Lock)),
     Pre => not Module_Registered(This_Module),
     Post => Module_Registered(This_Module);

   task Message_Loop is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.File_Server.Messages;
