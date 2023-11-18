--------------------------------------------------------------------------------
-- FILE    : message_manager.adb
-- SUBJECT : Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR  : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Jorvik);
pragma Partition_Elaboration_Policy(Sequential);

with CubedOS.Generic_Message_Manager;
with CubedOS.Message_Debuggers;

with Name_Resolver;

package Message_Manager is
  new CubedOS.Generic_Message_Manager
    (Domain => Name_Resolver.Domain,
     Debugger => CubedOS.Message_Debuggers.Null_Message_Debugger_Object);
