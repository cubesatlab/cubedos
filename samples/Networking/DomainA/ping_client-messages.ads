--------------------------------------------------------------------------------
-- FILE   : ping_client-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with System;

with Ping_Client.API;
with Message_Manager;

package Ping_Client.Messages is
   use Ping_Client.API;
   use Message_Manager;

   procedure Init
     with Global => (In_Out => (Mailboxes, Lock)),
     Pre => not Module_Registered(This_Module),
     Post => Module_Registered(This_Module);

   task Message_Loop is
	  pragma Storage_Size(4 * 1024);
	  pragma Priority(System.Default_Priority);
   end Message_Loop;

end Ping_Client.Messages;
