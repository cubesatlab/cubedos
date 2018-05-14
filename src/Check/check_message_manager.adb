--------------------------------------------------------------------------------
-- FILE   : check_message_manager.adb
-- SUBJECT: Package containing unit tests of the message manager.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
-- THIS TEST IS NOT CURRENTLY USED! (2017-11-13) It violates the Ravenscar restrictions by
-- trying to create local protected objects. The test should be reworked to be a proper
-- Ravenscar program.
--------------------------------------------------------------------------------
with Ada.Exceptions;
with AUnit.Assertions;
with Message_Manager;

use Ada.Exceptions;
use AUnit.Assertions;
use Message_Manager;

package body Check_Message_Manager is

   procedure Test_Message_Order(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      Test_Mailbox : Mailbox;
      Test_Message : Message_Record := Make_Empty_Message(Sender     => 1,
                                                          Receiver   => 1,
                                                          Message_ID => 1,
                                                          Priority   => 1);
      Status : Status_Type;
   begin
      -- Fill the mailbox with messages having sequential data.
       for I in Message_Index_Type loop
         Test_Message.Payload(0) := XDR_Octet(I);
         Test_Message.Size := 1;
         Test_Mailbox.Send(Test_Message, Status);
         Assert(Status = Accepted, "Message not accepted");
      end loop;

      -- Check to make sure the messages are being recieved in the right order.
      for I in  Message_Index_Type loop
         Test_Mailbox.Receive(Test_Message);
         Assert(Test_Message.Payload(0) = XDR_Octet(I), "Incorrect message received");
      end loop;
   end Test_Message_Order;


   procedure Test_Message_Order_Unchecked(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      Test_Mailbox : Mailbox;
      Test_Message : Message_Record := Make_Empty_Message(Sender     => 1,
                                                          Receiver   => 1,
                                                          Message_ID => 1,
                                                          Priority   => 1);
   begin
      -- Fill the mailbox with messages having sequential data.
       for I in Message_Index_Type loop
         Test_Message.Payload(0) := XDR_Octet(I);
         Test_Message.Size := 1;
         Test_Mailbox.Unchecked_Send(Test_Message);
      end loop;

      -- Check to make sure the messages are being recieved in the right order.
      for I in  Message_Index_Type loop
         Test_Mailbox.Receive(Test_Message);
         Assert(Test_Message.Payload(0) = XDR_Octet(I), "Incorrect message received");
      end loop;
   end Test_Message_Order_Unchecked;


   procedure Test_Mailbox_Full(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      Test_Mailbox : Mailbox;
      Test_Message : Message_Record := Make_Empty_Message(Sender     => 1,
                                                          Receiver   => 1,
                                                          Message_ID => 1,
                                                          Priority   => 1);
      Status : Status_Type;
   begin
      -- Fill the mailbox and then try adding an additional message.
      for I in Message_Index_Type'First .. Message_Index_Type'Last + 1 loop
         Test_Message.Payload(0) := XDR_Octet(I);
         Test_Mailbox.Send(Test_Message, Status);
         if I = Message_Index_Type'Last + 1 then
            Assert(Status = Mailbox_Full, "Mailbox should be full");
         else
            Assert(Status = Accepted, "Mailbox should not be full");
         end if;
      end loop;
   end Test_Mailbox_Full;


   procedure Test_Multiple_Tasks(T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      type Counter_Type is mod 2**16;

      Kill_Sender : Boolean := False with Atomic, Volatile;
      Failure : Exception_Occurrence;

      procedure Do_Test is
         Test_Mailbox : Mailbox;

         task Send;
         task Receive;

         task body Send is
            Send_Message : Message_Record := Make_Empty_Message(Sender     => 1,
                                                                Receiver   => 1,
                                                                Message_ID => 1,
                                                                Priority   => 1);
            Status : Status_Type;
         begin
            for I in Counter_Type'Range loop
               Send_Message.Payload(0) := XDR_Octet(I / 256);
               Send_Message.Payload(1) := XDR_Octet(I rem 256);
               loop
                  Test_Mailbox.Send(Send_Message, Status);
                  exit when Status = Accepted or Kill_Sender;
               end loop;
               exit when Kill_Sender;
            end loop;
         end Send;


         task body Receive is
            Receive_Message : Message_Record;
            Received_I : Counter_Type;
         begin
            for I in Counter_Type'Range loop
               Test_Mailbox.Receive(Receive_Message);
               Received_I :=
                 256*Counter_Type(Receive_Message.Payload(0)) +
                     Counter_Type(Receive_Message.Payload(1));

               Assert(Received_I = I,
                      "Incorrect message. Expected: " & Counter_Type'Image(I) &
                        ", received: "                & Counter_Type'Image(Received_I));
            end loop;
         exception
            when Ex : others =>
               Save_Occurrence(Target => Failure, Source => Ex);
               Kill_Sender := True;
         end Receive;

      begin  -- Do_Test
         null;
      end Do_Test;

   begin -- Test_Multiple_Tasks
      Do_Test;
      if Kill_Sender then
         Reraise_Occurrence(Failure);
      end if;
   end Test_Multiple_Tasks;


   procedure Register_Tests(T : in out Message_Manager_Test) is
   begin
      Registration.Register_Routine(T, Test_Message_Order'Access, "Message Order");
      Registration.Register_Routine(T, Test_Message_Order_Unchecked'Access, "Message Order Unchecked");
      Registration.Register_Routine(T, Test_Mailbox_Full'Access, "Mailbox Full");
      Registration.Register_Routine(T, Test_Multiple_Tasks'Access, "Multiple Tasks");
   end Register_Tests;


   function Name(T : Message_Manager_Test) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("Message Manager");
   end Name;

end Check_Message_Manager;
