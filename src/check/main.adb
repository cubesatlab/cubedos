--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program to test full build of all core modules.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
-- This program doesn't actually do anything. However, it pulls in all of the core
-- modules in a single executable. Compiling this program thus will ensure that all
-- of the modules are compiled. Also analysis of this program will cover all core
-- modules.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS;

-- Bring in the necessary modules, both from CubedOS and from this application.

with CubedOS.File_Server.Messages;
with CubedOS.Interpreter.Messages;
with CubedOS.Log_Server.Messages;
with CubedOS.Publish_Subscribe_Server.Messages;
with CubedOS.Time_Server.Messages;
--with CubedOS.Transport_UDP.Messages;
with Message_Manager;

with Ada.Text_IO;

procedure Main is
begin
   CubedOS.Interpreter.Messages.Init;
   CubedOS.Log_Server.Messages.Init;
   CubedOS.Publish_Subscribe_Server.Messages.Init;
   CubedOS.File_Server.Messages.Init;
   CubedOS.Time_Server.Messages.Init;
   --CubedOS.Transport_UDP.Messages.Init;

   Message_Manager.Wait;

   Ada.Text_IO.Put_Line("Initialized Successfully");
end Main;
