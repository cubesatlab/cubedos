--------------------------------------------------------------------------------
-- FILE   : main_cfdp_rx.adb
-- SUBJECT: The main file for testing CFDP (receiver).
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------

--with Ada.Text_IO;
--with CubedOS.CFDP.API;
with CubedOS.CFDP;
with CubedOS.File_Server.Messages;
pragma Unreferenced(CubedOS.CFDP);
pragma Unreferenced(CubedOS.File_Server.Messages);
with Message_Manager;

procedure Main_CFDP_RX is
   My_Module_ID : constant Message_Manager.Module_ID_Type := 5;  -- Be sure this doesn't conflict.
   Incoming_Message : Message_Manager.Message_Record;
begin

   loop
      Message_Manager.Fetch_Message(My_Module_ID, Incoming_Message);

      -- Process messages.
      null;
   end loop;
end Main_CFDP_RX;
