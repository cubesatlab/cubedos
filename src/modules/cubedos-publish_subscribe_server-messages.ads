--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;

package CubedOS.Publish_Subscribe_Server.Messages
  with
    Abstract_State => Database,
    Initializes => Database
is

   task type Message_Loop
     with Global => (In_Out => (Database, Message_Manager.Mailboxes)),
        Priority => System.Default_Priority
   is
      -- pragma Storage_Size(4 * 1024);
      -- pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Publish_Subscribe_Server.Messages;
