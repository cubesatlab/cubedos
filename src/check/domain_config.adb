--------------------------------------------------------------------------------
-- FILE   : domain_config.adb
-- SUBJECT: Specifies information regarding a domain.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
--------------------------------------------------------------------------------

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

   procedure On_Message_Receive_Succeed(Msg : in Message_Record)
     with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put_Line("[Received OK] " & Stringify_Message(Msg));
   end On_Message_Receive_Succeed;

   procedure On_Message_Receive_Failed(Msg : in Message_Record)
     with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put_Line("[Received Fail] " & Stringify_Message(Msg));
   end On_Message_Receive_Failed;

   procedure On_Message_Read(Receiver : in Module_Metadata; Msg : in Message_Record)
     with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put_Line("[Message Read] " & Module_ID_Type'Image(Receiver.Module_ID));
   end On_Message_Read;

   procedure On_Message_Discarded(Receiver : in Module_Metadata; Msg : in Message_Record)
     with SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put_Line("[Message Discarded] " & Module_ID_Type'Image(Receiver.Module_ID));
   end On_Message_Discarded;

end Domain_Config;
