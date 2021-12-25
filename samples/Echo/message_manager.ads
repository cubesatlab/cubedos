--------------------------------------------------------------------------------
-- FILE    : message_manager.adb
-- SUBJECT : Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Generic_Message_Manager;
pragma Elaborate_All(CubedOS.Generic_Message_Manager);

package Message_Manager is
  new CubedOS.Generic_Message_Manager
    (Domain_Number =>  1,
     Module_Count  =>  6,
     Mailbox_Size  =>  8,
     Maximum_Message_Size => 128);
