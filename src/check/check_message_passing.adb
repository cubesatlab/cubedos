---------------------------------------------------------------------------
-- FILE    : check_message_passing.adb
-- SUBJECT : Package containing tests of the message system.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--
---------------------------------------------------------------------------

with Message_Manager; use Message_Manager;
with CubedOS.Message_Types; use CubedOS.Message_Types;
with CubedOS.Message_Types.Mutable; use CubedOS.Message_Types.Mutable;
with AUnit.Assertions; use AUnit.Assertions;

package body Check_Message_Passing is

   Sender_Addr : constant Message_Address := (1, 3);
   Receiver_Addr : constant Message_Address := (1, 4);

   Unnacceptable_Type : constant Universal_Message_Type := (1, 0);
   Acceptable_Type : constant Universal_Message_Type := (1, 1);

   Receiver_Receive_Types : aliased constant Message_Type_Array := (0 => Acceptable_Type);
   Receiver_Metadata : constant Module_Metadata := Define_Module(Receiver_Addr.Module_ID, Receiver_Receive_Types'Access);

   Unacceptable_Msg : Message_Record := Immutable(Make_Empty_Message
                                                     (Sender_Addr, Receiver_Addr, 0, Unnacceptable_Type, 0));
   Unacceptable_Msg_2 : Message_Record := Immutable(Make_Empty_Message
                                                    (Sender_Addr, Receiver_Addr, 0, Unnacceptable_Type, 0));
   Acceptable_Msg : Message_Record := Immutable(Make_Empty_Message
                                                (Sender_Addr, Receiver_Addr, 0, Acceptable_Type, 0));

   Sender : constant Module_Mailbox := Make_Module_Mailbox(Sender_Addr.Module_ID, (Sender_Addr.Module_ID, Empty_Type_Array'Access));
   Receiver : constant Module_Mailbox := Make_Module_Mailbox(Receiver_Addr.Module_ID, Receiver_Metadata);

   function Pending_Messages(Box : Module_Mailbox) return Natural is
      Number : Natural;
   begin
      Message_Manager.Pending_Messages(Box, Number);
      return Number;
   end Pending_Messages;

   procedure Discard_Next(Box : Module_Mailbox) is
     Msg : Message_Record;
   begin
      Read_Next(Box, Msg);
      Delete(Msg);
   end;

   procedure Send_Unacceptable_Message_With_Safe_Procedure is
   begin
      Send_Message(Sender, Unacceptable_Msg, Receiver_Metadata, This_Domain);
   end;

   -- Messages of an unreceivable type aren't read by mailboxes.
   procedure Test_Msg_Type_Checking(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      pragma Unused(T);

      -- Register mailboxes
      Register_Module(Sender, 1);
      Register_Module(Receiver, 1);

      Message_Manager.Skip_Mailbox_Initialization;

      -- Check that acceptable message reaches receiver
      Send_Message(Sender, Acceptable_Msg);
      Assert(Pending_Messages(Receiver) = 1, "Acceptable message wasn't received.");

      -- Read that message out
      Discard_Next(Receiver);

      -- Confirm that the message was removed from the mailbox
      Assert(Pending_Messages(Receiver) = 0, "Read message wasn't wasn't removed from mailbox.");

      -- Check that unacceptable message can't be sent by the safe send procedure
      Assert_Exception(Send_Unacceptable_Message_With_Safe_Procedure'Access, "Safe send procedure didn't prevent sending unsafe message");

      -- Send the unreceivable message using the unsafe procedure
      Send_Message(Sender, Unacceptable_Msg_2);

      -- Verify that the message wasn't deposited in the destination mailbox
      Assert(Pending_Messages(Receiver) = 0, "Unacceptable message was desposited in the destination mailbox.");
   end Test_Msg_Type_Checking;


   procedure Register_Tests(T : in out Message_Passing_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Msg_Type_Checking'Access, "Message Type Safety");
   end Register_Tests;


   function Name(T : in Message_Passing_Test) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("Msg Passing");
   end Name;

end Check_Message_Passing;
