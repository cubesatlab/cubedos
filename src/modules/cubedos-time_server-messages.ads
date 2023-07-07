--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-messages.ads
-- SUBJECT: Specification of a package for a time server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Jorvik);
pragma Partition_Elaboration_Policy(Sequential);

with Ada.Real_Time;
with System;
with Message_Manager; use Message_Manager;
with CubedOS.Time_Server.API; use CubedOS.Time_Server.API;

package CubedOS.Time_Server.Messages
  with
    Abstract_State => ((Tick_Database with External)),
  Initializes => (Tick_Database, Message_Loop)
is

   procedure Init
     with Global => (In_Out => (Mailboxes, Lock)),
     Pre => not Module_Ready(This_Module),
     Post => Module_Ready(This_Module);

   task Message_Loop
     with
       Global => (Input => (Ada.Real_Time.Clock_Time), In_Out => (Tick_Database, Lock, Mailboxes))
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Time_Server.Messages;
