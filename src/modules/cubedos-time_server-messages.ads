--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-messages.ads
-- SUBJECT: Specification of a package for a time server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with Ada.Real_Time;
with System;
with Message_Manager;

package CubedOS.Time_Server.Messages
  with
    Abstract_State => ((Tick_Database with External), Own_Mailbox),
    Initializes => (Message_Loop, Tick_Database)
is

   task Message_Loop
     with
       Global => (Input => Ada.Real_Time.Clock_Time, In_Out => (Tick_Database, Own_Mailbox))
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Time_Server.Messages;
