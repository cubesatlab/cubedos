--------------------------------------------------------------------------------
-- FILE   : cubedos.transport_udp-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

package CubedOS.Transport_UDP.Messages is
	
	task Network_Loop is
		pragma Priority(System.Default_Priority);
	end Network_Loop;

   task Message_Loop
	 with Global => (In_Out => Message_Manager.Mailboxes)
   is
	  -- pragma Storage_Size(4 * 1024);
	  pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Transport_UDP.Messages;
