--------------------------------------------------------------------------------
-- FILE    : message_manager.adb
-- SUBJECT : Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR  : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with CubedOS.Generic_Message_Manager;
pragma Elaborate_All(CubedOS.Generic_Message_Manager);

package Message_Manager is
  new CubedOS.Generic_Message_Manager
    (Domain_Number =>  1,
     Module_Count  => 16,
     Mailbox_Size  =>  8,
     Maximum_Message_Size => 1024);
