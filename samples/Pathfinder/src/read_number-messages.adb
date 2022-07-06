--------------------------------------------------------------------------------
--
--  FILE   : Read_Number-messages.adb
--  SUBJECT: Body of a package that implements the main part of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Exceptions;
with GNAT.OS_Lib;
with Ada.Text_IO;

package body Read_Number.Messages is
   ML : Message_Loop;

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
      Fib_Number       : Natural;
      Call_Sign        : constant String := "High[" & Pri'Image & " ]: ";
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
      Outgoing_Message : Message_Record;
      Outgoing_Status  : Status_Type;
   begin
      Message_Manager.Initialize_Priority(ID, Pri);
      Outgoing_Message :=
        System_Bus.API.Random_Number_Request_Encode
          (Sender_Domain => Domain_ID, Sender => ID, Priority => Pri,
           Request_ID    => R_ID);

      Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);

      while Outgoing_Status /= Accepted loop
         Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);
      end loop;
   end Initialize;

   procedure Process (Message : in Message_Record) is
   begin
      if System_Bus.API.Is_Random_Number_Reply (Message) then
         Handle_Read_Number_Reply (Message);
      end if;
   end Process;

   procedure Handle_Read_Number_Reply (Message : in Message_Record) is
      Outgoing_Message : Message_Record;
      Outgoing_Status  : Status_Type;
      Status           : Message_Status_Type;
      Value            : Positive;
      Call_Sign        : constant String :=
        "High[" & Pri'Image & " ] -- Handle Reply: ";
   begin
      System_Bus.API.Random_Number_Reply_Decode (Message, Status, Value);
      --  Act on the request message.

      if Status = Malformed then
         Ada.Text_IO.Put_Line
           (Call_Sign & "Error: Message status is Malformed!");
         Ada.Text_IO.New_Line;
         return;
      end if;

      Ada.Text_IO.Put (Call_Sign & "The random number recieved is");
      Ada.Text_IO.Put_Line (Value'Image);
      Ada.Text_IO.Put_Line (Call_Sign & "Thank you! I would like another...");
      Ada.Text_IO.New_Line;

      Outgoing_Message :=
        System_Bus.API.Random_Number_Request_Encode
          (Sender_Domain => Domain_ID, Sender => ID, Priority => Pri,
           Request_ID    => R_ID);

      Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);

      while Outgoing_Status /= Accepted loop
         Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);
      end loop;

   end Handle_Read_Number_Reply;
end Read_Number.Messages;
