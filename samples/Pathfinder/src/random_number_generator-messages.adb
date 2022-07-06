--------------------------------------------------------------------------------
--
--  FILE   : random_number_generator-messages.adb
--  SUBJECT: Body of a package that implements the main part of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Exceptions;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Fibonacci;

package body Random_Number_Generator.Messages is
   ML : Message_Loop;

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
      Fib_Number       : Natural;
      Call_Sign        : constant String := "Low[" & Pri'Image & " ]: ";
   begin
      Initialize;
      loop
         Ada.Text_IO.Put_Line
           (Call_Sign & "Wasting time generating a Fibonacci number...");
         Fib_Number := Fibonacci.Gen_Slowest (FSeed);

         Ada.Text_IO.Put (Call_Sign & "Fibonacci(" & FSeed'Image & " ) is");
         Ada.Text_IO.Put_Line (Fib_Number'Image);

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           (Call_Sign & "Getting next message to process...");

         Message_Manager.Fetch_Message (ID, Incoming_Message);
         Process (Incoming_Message);

         Ada.Text_IO.Put_Line (Call_Sign & "Processed message.");
         Ada.Text_IO.New_Line;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Error: " & Ada.Exceptions.Exception_Message (E));
         GNAT.OS_Lib.OS_Exit (Status => 1);
   end Message_Loop;

   procedure Initialize is
   begin
      Message_Manager.Initialize_Priority (ID, Pri);
   end Initialize;

   procedure Process (Message : in Message_Record) is
   begin
      if Random_Number_Generator.API.Is_Generate_Number_Request (Message) then
         Handle_Generate_Number_Request (Message);
      end if;
   end Process;

   procedure Handle_Generate_Number_Request (Message : in Message_Record) is
      Outgoing_Message : Message_Record;
      Outgoing_Status  : Status_Type;
      Call_Sign        : constant String :=
        "Low[" & Pri'Image & " ] -- Handle Request: ";
   begin
      Outgoing_Message :=
        Random_Number_Generator.API.Generate_Number_Reply_Encode
          (Receiver_Domain => Domain_ID, Receiver => Message.Sender,
           Request_ID      => Message.Request_ID, Priority => Pri);

      Outgoing_Message.Is_Clobberable := False;

      Ada.Text_IO.Put_Line (Call_Sign & "Sending Reply...");
      Ada.Text_IO.New_Line;

      Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);

      while Outgoing_Status /= Accepted loop
         Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);
      end loop;

   end Handle_Generate_Number_Request;
end Random_Number_Generator.Messages;
