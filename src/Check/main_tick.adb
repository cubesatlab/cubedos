--------------------------------------------------------------------------------
-- FILE   : main_tick.adb
-- SUBJECT: The main file to test the tick generator package.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
-- In order to test the tick generator module, you must run the produced executable
-- in the command prompt, and ^C out of it when a suitable time has passed. Then,
-- observe the behavior to ensure the desired effects are happening.
--------------------------------------------------------------------------------
with Ada.Integer_Text_IO;
with Ada.Real_Time;
with Ada.Text_IO;
with CubedOS.Tick_Generator;
with CubedOS.Tick_Generator.API;
with CubedOS.Tick_Generator.Messages;
pragma Unreferenced(CubedOS.Tick_Generator.Messages);
with Message_Manager;

use Ada.Integer_Text_IO;
use Ada.Text_IO;
--use CubedOS.Lib;
use CubedOS.Tick_Generator;
use CubedOS.Tick_Generator.API;
use Message_Manager;

procedure Main_Tick is

   task Test_Tick_Generator_1; -- Normal periodic tick test
   task Test_Tick_Generator_2; -- Normal periodic tick test
   task Test_Tick_Generator_3; -- One shot tick test
   task Test_Tick_Generator_4; -- Cancel tick request test


   -- Basic test.
   task body Test_Tick_Generator_1 is
      Tick_Request     : Message_Record;
      Incoming_Message : Message_Record;
      Series_ID        : Series_ID_Type;
      Count            : Natural;
      Status           : Message_Status_Type;
      ID   : constant Message_Manager.Module_ID_Type := 16;
   begin
      -- Make and then request a tick message from the generator module.
      Tick_Request := Relative_Request_Encode(ID, Ada.Real_Time.Milliseconds(3000), Periodic, 1);
      Message_Manager.Mailboxes(1).Unchecked_Send(Tick_Request);
      loop
         -- Receives each tick message, and prints the count number.
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         if Is_Tick_Reply(Incoming_Message) then
            Tick_Reply_Decode(Incoming_Message, Series_ID, Count, Status);
            if Status = Success then
               Put("Task 1 --"); Put(Count); New_Line;
            end if;
         end if;
      end loop;
   end Test_Tick_Generator_1;


   -- The interval in this test is the same as above in case that causes a problem.
   task body Test_Tick_Generator_2 is
      Tick_Request     : Message_Record;
      Incoming_Message : Message_Record;
      Series_ID        : Series_ID_Type;
      Count            : Natural;
      Status           : Message_Status_Type;
      ID   : constant Message_Manager.Module_ID_Type := 15;
   begin
      -- Make and then request a tick message from the generator module.
      Tick_Request := Relative_Request_Encode(ID, Ada.Real_Time.Milliseconds(3000), Periodic, 2);
      Message_Manager.Mailboxes(1).Unchecked_Send(Tick_Request);
      loop
         -- Receives each tick message, and prints the count number.
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         if Is_Tick_Reply(Incoming_Message) then
            Tick_Reply_Decode(Incoming_Message, Series_ID, Count, Status);
            if Status = Success then
               Put("Task 2 --"); Put(Count); New_Line;
            end if;
         end if;
      end loop;
   end Test_Tick_Generator_2;


   -- One shot test.
   task body Test_Tick_Generator_3 is
      Tick_Request     : Message_Record;
      Incoming_Message : Message_Record;
      Series_ID        : Series_ID_Type;
      Count            : Natural;
      Status           : Message_Status_Type;
      ID   : constant Message_Manager.Module_ID_Type := 14;
   begin
      -- Make a request for ONE tick message from the generator module.
      Tick_Request := Relative_Request_Encode(ID, Ada.Real_Time.Milliseconds(200), One_Shot, 3);
      Message_Manager.Mailboxes(1).Unchecked_Send(Tick_Request);
      loop
         -- Receives the tick message, and prints out the count number.
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         if Is_Tick_Reply(Incoming_Message) then
            Tick_Reply_Decode(Incoming_Message, Series_ID, Count, Status);
            if Status = Success then
               Put("Task 3 --"); Put(Count); New_Line;
            end if;
         end if;
      end loop;
   end Test_Tick_Generator_3;


   -- Add and remove a series, then create a different series.
   task body Test_Tick_Generator_4 is
      Tick_Request        : Message_Record;
      Incoming_Message    : Message_Record;
      Cancel_Tick_Request : Message_Record;
      Series_ID           : Series_ID_Type;
      Count               : Natural;
      Status              : Message_Status_Type;
      ID   : constant Message_Manager.Module_ID_Type := 13;
   begin
      -- Make and request a tick message from the generator module.
      Tick_Request := Relative_Request_Encode(ID, Ada.Real_Time.Milliseconds(2000), Periodic, 44);
      Message_Manager.Mailboxes(1).Unchecked_Send(Tick_Request);

      -- Remove series and ask for a new periodic tick.
      Cancel_Tick_Request := Cancel_Request_Encode(ID, 44);
      Message_Manager.Mailboxes(1).Unchecked_Send(Cancel_Tick_Request);
      Tick_Request := Relative_Request_Encode(ID, Ada.Real_Time.Milliseconds(1000), Periodic, 4);
      Message_Manager.Mailboxes(1).Unchecked_Send(Tick_Request);
      loop
         -- Receives each tick message, and prints the count number.
         Message_Manager.Mailboxes(ID).Receive(Incoming_Message);
         if Is_Tick_Reply(Incoming_Message) then
            Tick_Reply_Decode(Incoming_Message, Series_ID, Count, Status);
            if Status = Success then
               Put("Task 4 --"); Put(Count); New_Line;
            end if;
         end if;
      end loop;
   end Test_Tick_Generator_4;


begin
   null;
end Main_Tick;
