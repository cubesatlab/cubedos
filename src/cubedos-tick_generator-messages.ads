--------------------------------------------------------------------------------
-- FILE   : cubedos-tick_generator.ads
-- SUBJECT: Specification of a package for a tick generator module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with Ada.Real_Time;
with System;
with Message_Manager;

package CubedOS.Tick_Generator.Messages
  with
    Abstract_State => (Tick_Database with External),
    Initializes => (Message_Loop, Tick_Database)
is

   task Message_Loop
     with
       Global => (Input => Ada.Real_Time.Clock_Time, In_Out => (Tick_Database, Message_Manager.Mailboxes))
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Tick_Generator.Messages;
