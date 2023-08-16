--------------------------------------------------------------------------------
-- FILE   : cubedos-message_debuggers.adb
-- SUBJECT: Defines the implementation of included message debuggers.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;

package body CubedOS.Message_Debuggers is

   ------------------
   -- Null Debugger
   ------------------

   procedure On_Message_System_Initialization_Complete(D : in Null_Message_Debugger)
   is null;
   procedure On_Message_Sent_Debug(D : in Null_Message_Debugger; Msg : in Message_Record)
   is null;
   procedure On_Message_Receive_Succeed(D : in Null_Message_Debugger; Msg : in Message_Record)
   is null;
   procedure On_Message_Receive_Failed(D : in Null_Message_Debugger; Msg : in Message_Record)
   is null;
   procedure On_Message_Read(D : in Null_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record)
   is null;
   procedure On_Message_Discarded(D : in Null_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record)
   is null;

   --------------------
   -- Console Debugger
   --------------------

   procedure On_Message_System_Initialization_Complete(D : in Console_Message_Debugger)
   is
   begin
      Put_Line("[All Modules Initialized]");
   end;
   procedure On_Message_Sent_Debug(D : in Console_Message_Debugger; Msg : in Message_Record)
   is
   begin
      Put_Line("[Sent] " & Stringify_Message(Msg));
   end;
   procedure On_Message_Receive_Succeed(D : in Console_Message_Debugger; Msg : in Message_Record)
   is
   begin
      Put_Line("[Received OK] " & Stringify_Message(Msg));
   end;
   procedure On_Message_Receive_Failed(D : in Console_Message_Debugger; Msg : in Message_Record)
   is
   begin
      Put_Line("[Received Fail] " & Stringify_Message(Msg));
   end;
   procedure On_Message_Read(D : in Console_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record)
   is
   begin
      Put_Line("[Message Read] " & Module_ID_Type'Image(Receiver.Module_ID));
   end;
   procedure On_Message_Discarded(D : in Console_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record)
   is
   begin
      Put_Line("[Message Discarded] " & Module_ID_Type'Image(Receiver.Module_ID));
   end;

end CubedOS.Message_Debuggers;
