--------------------------------------------------------------------------------
-- FILE    : message_manager.adb
-- SUBJECT : Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

pragma Profile(Jorvik);
pragma Partition_Elaboration_Policy(Sequential);

with CubedOS.Generic_Message_Manager;
with Name_Resolver;
with CubedOS.Message_Debuggers;

package Message_Manager is
  new CubedOS.Generic_Message_Manager
	(Name_Resolver.Domain_A, CubedOS.Message_Debuggers.Console_Message_Debugger_Object);
