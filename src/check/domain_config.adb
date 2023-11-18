--------------------------------------------------------------------------------
-- FILE   : domain_config.adb
-- SUBJECT: Specifies information regarding a domain.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Transport_UDP.Messages;

package body Domain_Config is

   procedure Send_Outgoing_Message (Msg : in out Msg_Owner)
   is
   begin
      CubedOS.Transport_UDP.Messages.Send(Msg);
   end Send_Outgoing_Message;

end Domain_Config;
