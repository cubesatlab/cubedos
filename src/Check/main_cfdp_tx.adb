--------------------------------------------------------------------------------
-- FILE   : main_cfdp_tx.adb
-- SUBJECT: The main file for testing CFDP (transmitter).
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------

with Ada.Text_IO;
with CubedOS.CFDP.API;
with CubedOS.CFDP.Messages;
with CubedOS.File_Server.Messages;
pragma Unreferenced(CubedOS.CFDP.Messages);
pragma Unreferenced(CubedOS.File_Server.Messages);
with Message_Manager;

use Ada.Text_IO;
use CubedOS;
use Message_Manager;

procedure Main_CFDP_TX is
   My_Module_ID : constant Message_Manager.Module_ID_Type := 10;  -- Be sure this doesn't conflict.
   Incoming_Message : Message_Manager.Message_Record;
begin

   -- Try to send a file.
   Message_Manager.Route_Message
     (CFDP.API.Put_Request_Message(Domain_ID, My_Module_ID, 1, "File_1.txt", "File_1.txt"));
   Put_Line("TX : Put Request message sending File_1 to CFDP entity #1");
   loop
      Message_Manager.Fetch_Message(My_Module_ID, Incoming_Message);

      -- Process reply messages.
      null;
   end loop;
end Main_CFDP_TX;
