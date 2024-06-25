--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server-messages.ads
-- SUBJECT: Specification of a package for a time server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Real_Time;
with System;
with Message_Manager;

package CubedOS.Time_Server.Messages
  with
    Abstract_State =>
      (Tick_Database
        with External),
    Initializes =>
      (Message_Loop, Tick_Database)
is

   task Message_Loop
     with
       Global => (Input => Ada.Real_Time.Clock_Time, In_Out => (Tick_Database, Message_Manager.Mailboxes))
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Time_Server.Messages;
