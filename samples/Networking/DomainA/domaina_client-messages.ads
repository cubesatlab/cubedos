--------------------------------------------------------------------------------
-- FILE   : echo_client-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

package DomainA_Client.Messages is

   task Message_Loop is
	  pragma Storage_Size(4 * 1024);
	  pragma Priority(System.Default_Priority);
   end Message_Loop;

end DomainA_Client.Messages;
