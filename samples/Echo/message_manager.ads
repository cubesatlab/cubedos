--------------------------------------------------------------------------------
-- FILE    : message_manager.adb
-- SUBJECT : Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
--pragma Elaborate_All(CubedOS.Generic_Message_Manager);
       
with CubedOS.Generic_Message_Manager;
with CubedOS.Message_Debuggers;
with Name_Resolver;

package Message_Manager is
  new CubedOS.Generic_Message_Manager
    (Domain => Name_Resolver.Domain,
     Debugger => CubedOS.Message_Debuggers.Null_Message_Debugger_Object);
