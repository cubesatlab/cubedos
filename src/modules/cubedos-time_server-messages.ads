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
    Abstract_State => ((Tick_Database with External), (Own_Mailbox with External)),
  Initializes => (Tick_Database, Message_Loop)
is

   This_Receives: constant Message_Type_Array := (Relative_Request_Msg,
                                             Absolute_Request_Msg,
                                             Cancel_Request_Msg);

   procedure Init
     with Global => (In_Out => (Mailboxes, Mailbox_Metadata, Own_Mailbox)),
     Pre => not Module_Ready(This_Module),
     Post => Module_Ready(This_Module)
     and then (for all T of This_Receives => Receives(This_Module, T));

   task Message_Loop
     with
       Global => (Input => (Ada.Real_Time.Clock_Time), In_Out => (Tick_Database, Own_Mailbox, Mailboxes), Proof_In => Mailbox_Metadata)
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Time_Server.Messages;
