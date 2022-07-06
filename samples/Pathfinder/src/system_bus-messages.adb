--------------------------------------------------------------------------------
--
-- FILE   : system_bus-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Ada.Exceptions;
with GNAT.OS_Lib;
with Ada.Text_IO;
with Telemetry.API;
with Read_Number;

package body System_Bus.Messages is

   ML    : Message_Loop;
   Count : Positive := 1;

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
      Fib_Number       : Natural;
      Call_Sign        : constant String := "SysBus[" & Pri'Image & " ]: ";
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

         Ada.Text_IO.Put_Line (Call_Sign & "Loop #" & Count'Image);
         Count := Count + 1;
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
      Message_Manager.Initialize_Priority (ID, Pri);

      Outgoing_Message :=
        System_Bus.API.Telemetry_Encode
          (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
           Priority      => Telemetry.Pri);

      for I in 1 .. 4 loop
         Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);
      end loop;
   end Initialize;

   procedure Process (Message : in Message_Record) is
   begin
      if System_Bus.API.Is_Random_Number_Request (Message) then
         Handle_Random_Number_Request (Message);
      elsif System_Bus.API.Is_Telemetry (Message) then
         Handle_Telemetry (Message);
      elsif Random_Number_Generator.API.Is_Generate_Number_Reply (Message) then
         Handle_Random_Number_Reply (Message);
      end if;
   end Process;

   -----------------
   -- Random Number
   -----------------
   procedure Handle_Random_Number_Request (Message : in Message_Record) is
      Outgoing_Message : Message_Record;
      Outgoing_Status  : Status_Type;
   begin
      Outgoing_Message :=
        Random_Number_Generator.API.Generate_Number_Request_Encode
          (Sender_Domain => Domain_ID, Sender => ID,
           Priority      => Message.Priority, Request_ID => R_ID);

      Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);

      while Outgoing_Status /= Accepted loop
         Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);
      end loop;
   end Handle_Random_Number_Request;

   procedure Handle_Random_Number_Reply (Message : in Message_Record) is
      Outgoing_Message : Message_Record;
      Outgoing_Status  : Status_Type;
   begin
      -- Reply to read
      Outgoing_Message :=
        System_Bus.API.Random_Number_Reply_Encode
          (Receiver_Domain => Domain_ID,
           Receiver        => Read_Number.ID,
           Request_ID      => Read_Number.R_ID,
           Priority        => Message.Priority);

      Outgoing_Message.Payload := Message.Payload;

      Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);

      while Outgoing_Status /= Accepted loop
         Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);
      end loop;
   end Handle_Random_Number_Reply;

   -------------
   -- Telemetry
   -------------
   procedure Handle_Telemetry (Message : in Message_Record) is
      Outgoing_Message : Message_Record;
      Outgoing_Status  : Status_Type;
      Call_Sign        : constant String := "SysBus[" & Pri'Image & " ]: ";
   begin

      Ada.Text_IO.Put_Line
        (Call_Sign & "Received Telemetry, Priority:" & Message.Priority'Image);
      Ada.Text_IO.New_Line;

      Outgoing_Message :=
        Telemetry.API.Telemetry_Encode
          (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
           Priority      => Message.Priority);

      Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);

      while Outgoing_Status /= Accepted loop
         Message_Manager.Route_Message (Outgoing_Message, Outgoing_Status);
      end loop;
   end Handle_Telemetry;

end System_Bus.Messages;
