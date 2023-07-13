pragma SPARK_Mode (On);

--with CubedOS.Transport_UDP.Messages;
with Ada.Text_IO;

package body Domain_Config is

   procedure Send_Outgoing_Message (Msg : in out Msg_Owner) is
   begin
      null;
      --CubedOS.Transport_UDP.Messages.Send(Msg);
   end Send_Outgoing_Message;

   procedure On_Message_Sent_Debug (Msg : in Message_Record)
     with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put_Line("[Sent] " & Stringify_Message(Msg));
   end On_Message_Sent_Debug;

end Domain_Config;
