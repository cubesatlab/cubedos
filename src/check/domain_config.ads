--------------------------------------------------------------------------------
-- FILE   : domain_config.ads
-- SUBJECT: Specifies information regarding a domain.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
-- This file is included by instantiations of message manager.
-- It has the procedure used by the message manager to route outgoing
-- messages to the appropriate transport modules, along with other
-- domain specific configurations.
--
-- It also contains several debug procedures which may be implemented
-- by the user to get information about message passing during development.
-- After development, these functions should have their body set to "null"
-- to avoid pointless computation.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Message_Types; use CubedOS.Message_Types;

package Domain_Config is

   -- This procedure routes messages destined to foreign
   -- domains to the appropriate transport module.
   procedure Send_Outgoing_Message (Msg : in out Msg_Owner)
     with Pre => Msg /= null,
     Post => Msg = null;


   ---------------
   -- Debugging
   ---------------

   -- Called immediately before messaging is allowed to begin.
   procedure On_Message_System_Initialization_Complete
     with Global => null;

   -- This procedure is called by the message manager
   -- for every message that is sent.
   procedure On_Message_Sent_Debug (Msg : in Message_Record)
     with Global => null;

   -- Called when a message is successfully deposited in its destination
   -- mailbox. Only called for messages sent to this domain.
   procedure On_Message_Receive_Succeed(Msg : in Message_Record)
     with Global => null;

   -- Called when a message fails to be deposited in its destination
   -- mailbox. The message is lost after this call.
   -- Only called for messages sent to this domain.
   procedure On_Message_Receive_Failed(Msg : in Message_Record)
     with Global => null;

   -- Called when a message is read from the mailbox of the receiver.
   procedure On_Message_Read(Receiver : in Module_Metadata; Msg : in Message_Record)
     with Global => null;

   -- Called when a message from the given mailbox is discarded because the
   -- receiver hasn't declared that it may receive the message type.
   procedure On_Message_Discarded(Receiver : in Module_Metadata; Msg : in Message_Record)
     with Global => null;



end Domain_Config;
