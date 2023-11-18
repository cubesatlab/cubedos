--------------------------------------------------------------------------------
-- FILE   : domain_config.ads
-- SUBJECT: Specifies information regarding a domain.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- This file is included by instantiations of message manager.
-- It has the procedure used by the message manager to route outgoing
-- messages to the appropriate transport modules, along with other
-- domain specific configurations.
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
pragma Profile (Jorvik);

with CubedOS.Message_Types; use CubedOS.Message_Types;

package Domain_Config is

   -- This procedure routes messages destined to foreign
   -- domains to the appropriate transport module.
   procedure Send_Outgoing_Message (Msg : in out Msg_Owner)
     with Pre => Msg /= null and Payload(Msg) /= null,
     Post => Msg = null;

end Domain_Config;
