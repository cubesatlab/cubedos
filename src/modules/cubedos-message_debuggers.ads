--------------------------------------------------------------------------------
-- FILE   : cubedos-message_debuggers.ads
-- SUBJECT: Specification of a message system debugger object.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

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

   type Message_Discard_Reason is (Destination_Doesnt_Exist, Destination_Doesnt_Accept_Message_Type);
   procedure On_Message_Discarded(D : in Message_Debugger; Msg : in Message_Record; Reason : Message_Discard_Reason) is abstract;
   -- Called when a message is discarded by the message system.

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
   procedure On_Message_Discarded(D : in Null_Message_Debugger; Msg : in Message_Record; Reason : Message_Discard_Reason);
   procedure On_Foreign_Message_Received(D : in Null_Message_Debugger; Msg : in Message_Record);

   type Console_Message_Debugger is new Message_Debugger with null record;
   -- This message debugger prints info to the console.

   procedure On_Message_System_Initialization_Complete(D : in Console_Message_Debugger);
   procedure On_Message_Sent_Debug(D : in Console_Message_Debugger; Msg : in Message_Record);
   procedure On_Message_Receive_Succeed(D : in Console_Message_Debugger; Msg : in Message_Record);
   procedure On_Message_Receive_Failed(D : in Console_Message_Debugger; Msg : in Message_Record; Reason : Fail_Reason);
   procedure On_Message_Read(D : in Console_Message_Debugger; Receiver : in Module_Metadata; Msg : in Message_Record);
   procedure On_Message_Discarded(D : in Console_Message_Debugger; Msg : in Message_Record; Reason : Message_Discard_Reason);
   procedure On_Foreign_Message_Received(D : in Console_Message_Debugger; Msg : in Message_Record);

   -- With both objects declared here, the following error is produced:
   --
   -- cubedos-message_debuggers.ads:49:04: error: first freezing point of type "Null_Message_Debugger" must appear within early call region of primitive body [E0003]
   -- cubedos-message_debuggers.ads:49:04: error: launch "gnatprove --explain=E0003" for more information
   -- cubedos-message_debuggers.ads:49:04: error: region starts at line 72
   -- cubedos-message_debuggers.ads:49:04: error: region ends at cubedos-message_debuggers.adb:17
   -- cubedos-message_debuggers.ads:49:04: error: first freezing point at line 71
   --
   -- The problem appears to be due to the way the declaration of the first object interacts with the second.
   -- Switching the order of the declarations just causes the other object to generate the same error.
   -- To fix this, it might be necessary to declare Null_Message_Debuger and Console_Message_Debugger in
   -- separate packages (each with their own pragma Elaborate_Body). In the meantime, I'm commenting out one
   -- of the declarations since Console_Message_Debugger_Object isn't needed right now.
   --
   Null_Message_Debugger_Object : aliased constant Null_Message_Debugger := Null_Message_Debugger'(null record);
   -- Console_Message_Debugger_Object : aliased constant Console_Message_Debugger := Console_Message_Debugger'(null record);

end CubedOS.Message_Debuggers;
