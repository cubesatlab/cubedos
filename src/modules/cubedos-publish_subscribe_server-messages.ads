--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Jorvik);
pragma Partition_Elaboration_Policy(Sequential);

-- Why are these here?
pragma Task_Dispatching_Policy(FIFO_Within_Priorities);
pragma Queuing_Policy(FIFO_Queuing);

with Message_Manager;
with CubedOS.Publish_Subscribe_Server.API;

with System;

package CubedOS.Publish_Subscribe_Server.Messages
  with
    Abstract_State => (Database),
    Initializes => (Database)
is
   use Message_Manager;
   use CubedOS.Publish_Subscribe_Server.API;

   procedure Init
     with Global => (In_Out => (Mailboxes, Lock)),
     Pre => not Module_Registered(This_Module),
     Post => Module_Registered(This_Module);

   task type Message_Loop
     with Global => (In_Out => (Database)),
        Priority => System.Default_Priority
   is
      -- pragma Storage_Size(4 * 1024);
      -- pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Publish_Subscribe_Server.Messages;
