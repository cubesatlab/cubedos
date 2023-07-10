--------------------------------------------------------------------------------
-- FILE   : cubedos.transport_udp-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

with Message_Manager;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package CubedOS.Transport_UDP.Messages is

   procedure Init;

   -- Sends the given message
   procedure Send(Msg : in out Msg_Owner);

	task Incoming_Loop is
		pragma Priority(System.Default_Priority);
	end Incoming_Loop;

   task Outgoing_Loop
	 with Global => (In_Out => Message_Manager.Mailboxes)
   is
	  -- pragma Storage_Size(4 * 1024);
	  pragma Priority(System.Default_Priority);
   end Outgoing_Loop;

end CubedOS.Transport_UDP.Messages;
