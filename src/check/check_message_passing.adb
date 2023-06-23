---------------------------------------------------------------------------
-- FILE    : check_message_passing.adb
-- SUBJECT : Package containing tests of the message system.
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--
---------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package body Check_Message_Passing is

   -- Modules declare what message types they are allowed to receive.
   -- Test that sending a message to a module that doesn't support it
   -- throws an exception before the message reaches the module,
   -- but an acceptable message reaches its destination.
   procedure Test_Msg_Type_Checking is
      Unnacceptable_Type : constant Universal_Message_Type := (1, 0);
      Acceptable_Type : constant Universal_Message_Type := (1, 1);

      Sender_Addr : constant Message_Address := (0, 1);
      Receiver_Addr : constant Message_Address := (0, 2);

      Unacceptable_Msg : Message_Record := Immutable(Make_Empty_Message
        (Sender_Addr, Receiver_Addr, 0, Unnacceptable_Type, 0));
      Acceptable_Msg : Message_Record := Immutable(Make_Empty_Message
        (Sender_Addr, Receiver_Addr, 0, Acceptable_Type, 0));

      Sender : Module_Mailbox;
      Receiver : Module_Mailbox;
   begin
      -- Declare what mailboxes accept what message types
      -- This is normally done in API files
      Declare_Accepts(Receiver_Addr.Module_ID, Acceptable_Type);

      -- Register mailboxes
      Register_Module(Sender_Addr.Module_ID, 1, Sender, Empty_Type_Array);
      Register_Module(Receiver_Addr.Module_ID, 1, Receiver, (0 => Acceptable_Type));


      -- Check that acceptable message reaches receiver
      Send_Message(Sender, Acceptable_Msg);
      declare
         Size : Natural;
      begin
         Queue_Size(Receiver, Size);
         pragma Assert(Size = 1, "Acceptable message wasn't received.");
      end;

      -- Check that unacceptable message throws an exception
      declare
      begin
         Send_Message(Sender, Unacceptable_Msg);
         pragma Assert(False, "Unacceptable message was sent without objection.");
      exception
         when others => pragma Assert(True, "Unnacceptable message raised error on send.");
      end;
   end Test_Msg_Type_Checking;

   procedure Run_Tests is
   begin
      Test_Msg_Type_Checking;
   end Run_Tests;

end Check_Message_Passing;
