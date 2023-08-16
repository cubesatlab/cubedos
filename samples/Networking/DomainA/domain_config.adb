--------------------------------------------------------------------------------
-- FILE   : domain_config.adb
-- SUBJECT: Configuration details for a domain.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Transport_UDP.Messages;

package body Domain_Config is

   procedure Send_Outgoing_Message (Msg : in out Msg_Owner) is
   begin
      CubedOS.Transport_UDP.Messages.Send(Msg);
   end Send_Outgoing_Message;

end Domain_Config;
