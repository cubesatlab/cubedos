
pragma SPARK_Mode (On);

with CubedOS.Message_Types; use CubedOS.Message_Types;

package Domain_Config is

   -- This procedure routes messages destined to foreign
   -- domains to the appropriate transport module.
   procedure Send_Outgoing_Message (Msg : in out Msg_Owner)
     with Pre => Msg /= null,
     Post => Msg = null;

   -- This procedure is called by the message manager
   -- for every message that is sent.
   procedure On_Message_Sent_Debug (Msg : in Message_Record)
     with Global => null;

end Domain_Config;
