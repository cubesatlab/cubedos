--------------------------------------------------------------------------------
-- FILE   : cubedos.transport_udp-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
with System;

with Message_Manager;
with CubedOS.Message_Types; use CubedOS.Message_Types;


package CubedOS.Transport_UDP.Messages is

   procedure Init;

   procedure Send(Msg : in out Msg_Owner)
     with Pre => Msg /= null
     and then Payload(Msg) /= null,
     Post => Msg = null;

	task Outgoing_Loop is
		pragma Priority(System.Default_Priority);
	end Outgoing_Loop;

   task Incoming_Loop
	 with Global => (In_Out => (Message_Manager.Mailboxes, Message_Manager.Lock))
   is
	  -- pragma Storage_Size(4 * 1024);
	  pragma Priority(System.Default_Priority);
   end Incoming_Loop;

end CubedOS.Transport_UDP.Messages;
