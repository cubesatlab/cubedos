--------------------------------------------------------------------------------
-- FILE   : networking_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

package DomainB_Server.Messages is

   task Message_Loop is
	  pragma Storage_Size(4 * 1024);
	  pragma Priority(System.Default_Priority);
   end Message_Loop;

end DomainB_Server.Messages;
