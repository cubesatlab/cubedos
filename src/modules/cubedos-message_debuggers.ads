--------------------------------------------------------------------------------
-- FILE   : cubedos-message_debuggers.ads
-- SUBJECT: Specification of a message system debugger object.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Message_Types; use CubedOS.Message_Types;

package CubedOS.Message_Debuggers is
   pragma Elaborate_Body;

   type Message_Debugger is interface;
   -- Message debuggers can be injected into the message manager
   -- to provide debug information about message passing.

   procedure On_Message_System_Initialization_Complete(D : in Message_Debugger) is abstract;
   -- Called immediately before messaging is allowed to begin.

   procedure On_Message_Sent_Debug(D : in Message_Debugger; Msg : in Message_Record) is abstract;
   -- This procedure is called by the message manager
   -- for any message that any module in the domain sends.

   procedure On_Message_Receive_Succeed(D : in Message_Debugger; Msg : in Message_Record) is abstract;
   -- Called when a message is successfully deposited in its destination
   -- mailbox. Only called for messages sent to this domain.

   type Fail_Reason is (Mailbox_Full_Or_Unitialized, Rejected_Type);
   procedure On_Message_Receive_Failed(D : in Message_Debugger; Msg : in Message_Record; Reason : Fail_Reason) is abstract;
   -- Called when a message fails to be deposited in its destination
   -- mailbox. The message is lost after this call.
   -- Only called for messages sent to this domain.

   procedure On_Message_Read(D : in Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record) is abstract;
   -- Called when a message is read from the mailbox of the receiver.

   procedure On_Message_Discarded(D : in Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record) is abstract;
   -- Called when a message from the given mailbox is discarded because the
   -- receiver hasn't declared that it may receive the message type.

   procedure On_Foreign_Message_Received(D : in Message_Debugger; Msg : in Message_Record) is abstract;
   -- Called when a message is received from a foreign domain.

   ---------------------------
   -- Default Implementations
   ---------------------------

   type Null_Message_Debugger is new Message_Debugger with null record;
   -- This message debugger does nothing and should be used for production.

   procedure On_Message_System_Initialization_Complete(D : in Null_Message_Debugger);
   procedure On_Message_Sent_Debug(D : in Null_Message_Debugger; Msg : in Message_Record);
   procedure On_Message_Receive_Succeed(D : in Null_Message_Debugger; Msg : in Message_Record);
   procedure On_Message_Receive_Failed(D : in Null_Message_Debugger; Msg : in Message_Record; Reason : Fail_Reason);
   procedure On_Message_Read(D : in Null_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record);
   procedure On_Message_Discarded(D : in Null_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record);
   procedure On_Foreign_Message_Received(D : in Null_Message_Debugger; Msg : in Message_Record);

   Null_Message_Debugger_Object : aliased constant Null_Message_Debugger := Null_Message_Debugger'(null record);

   type Console_Message_Debugger is new Message_Debugger with null record;
   -- This message debugger prints info to the console.

   procedure On_Message_System_Initialization_Complete(D : in Console_Message_Debugger);
   procedure On_Message_Sent_Debug(D : in Console_Message_Debugger; Msg : in Message_Record);
   procedure On_Message_Receive_Succeed(D : in Console_Message_Debugger; Msg : in Message_Record);
   procedure On_Message_Receive_Failed(D : in Console_Message_Debugger; Msg : in Message_Record; Reason : Fail_Reason);
   procedure On_Message_Read(D : in Console_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record);
   procedure On_Message_Discarded(D : in Console_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record);
   procedure On_Foreign_Message_Received(D : in Console_Message_Debugger; Msg : in Message_Record);

   Console_Message_Debugger_Object : aliased constant Console_Message_Debugger := Console_Message_Debugger'(null record);

end CubedOS.Message_Debuggers;
