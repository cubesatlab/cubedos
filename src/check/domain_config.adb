pragma SPARK_Mode (On);

--with CubedOS.Transport_UDP.Messages;

package body Domain_Config is

   procedure Send (Msg : in out Msg_Owner) is
   begin
      null;
      --CubedOS.Transport_UDP.Messages.Send(Msg);
   end Send;

end Domain_Config;
