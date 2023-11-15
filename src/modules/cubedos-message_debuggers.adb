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

   procedure On_Message_System_Initialization_Complete(D : in Null_Message_Debugger) is null;
   procedure On_Message_Sent_Debug(D : in Null_Message_Debugger; Msg : in Message_Record) is null;
   procedure On_Message_Receive_Succeed(D : in Null_Message_Debugger; Msg : in Message_Record) is null;
   procedure On_Message_Receive_Failed(D : in Null_Message_Debugger; Msg : in Message_Record; Reason : Fail_Reason) is null;
   procedure On_Message_Read(D : in Null_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record) is null;
   procedure On_Message_Discarded(D : in Null_Message_Debugger; Msg : in Message_Record; Reason : Message_Discard_Reason) is null;
   procedure On_Foreign_Message_Received(D : in Null_Message_Debugger; Msg : in Message_Record) is null;

   --------------------
   -- Console Debugger
   --------------------

   procedure On_Message_System_Initialization_Complete(D : in Console_Message_Debugger) is
   begin
      Put_Line("[All Modules Initialized]");
   end;

   procedure On_Message_Sent_Debug(D : in Console_Message_Debugger; Msg : in Message_Record) is
   begin
      Put_Line("[Sent] " & Stringify_Message(Msg));
   end;

   procedure On_Message_Receive_Succeed(D : in Console_Message_Debugger; Msg : in Message_Record) is
   begin
      Put_Line("[Received OK] " & Stringify_Message(Msg));
   end;

   procedure On_Message_Receive_Failed(D : in Console_Message_Debugger; Msg : in Message_Record; Reason : Fail_Reason) is
   begin
      Put_Line("[Received Fail] " & Stringify_Message(Msg) & " Reason: " & Fail_Reason'Image(Reason));
   end;

   procedure On_Message_Read(D : in Console_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record) is
   begin
      Put_Line("[Message Read] " & Module_ID_Type'Image(Receiver.Module_ID));
   end;

   procedure On_Message_Discarded(D : in Console_Message_Debugger; Msg : in Message_Record; Reason : Message_Discard_Reason) is
   begin
      Put_Line("[Message Discarded] " & Stringify_Message(Msg) & " Reason: " & Message_Discard_Reason'Image(Reason));
   end;

   procedure On_Foreign_Message_Received(D : in Console_Message_Debugger; Msg : in Message_Record) is
   begin
      Put_Line("[Received Foreign Message] " & Stringify_Message(Msg));
   end;

end CubedOS.Message_Debuggers;
