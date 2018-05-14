--------------------------------------------------------------------------------
-- FILE    : message_manager.adb
-- SUBJECT : Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR  : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------

with CubedOS.Generic_Message_Manager;

package Message_Manager is
  new CubedOS.Generic_Message_Manager
    (Module_Count => 2,
     Mailbox_Size => 1,
     Maximum_Message_Size => 256);
