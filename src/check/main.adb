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
with Ada.Real_Time;
with System;
with CubedOS;

-- Bring in the necessary modules, both from CubedOS and from this application.

with CubedOS.File_Server.Messages;
with CubedOS.Interpreter.Messages;
with CubedOS.Log_Server.Messages;
with CubedOS.Publish_Subscribe_Server.Messages;
with CubedOS.Time_Server.Messages;
with CubedOS.Transport_UDP.Messages;
with CubedOS.Message_System;
with CubedOS.Message_System.Modules;
with Test_Module;
with Test_Module.API;

pragma Unreferenced(CubedOS.File_Server.Messages);
pragma Unreferenced(CubedOS.Interpreter.Messages);
pragma Unreferenced(CubedOS.Log_Server.Messages);
pragma Unreferenced(CubedOS.Publish_Subscribe_Server.Messages);
pragma Unreferenced(CubedOS.Time_Server.Messages);
pragma Unreferenced(CubedOS.Transport_UDP.Messages);
pragma Unreferenced(CubedOS.Message_System);
pragma Unreferenced(CubedOS.Message_System.Modules);
pragma Unreferenced(Test_Module);
pragma Unreferenced(Test_Module.API);

procedure Main is

   pragma Priority(System.Priority'First);
   use type Ada.Real_Time.Time;
   Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(1000);
begin
   -- This loop does nothing at the lowest priority. It spends most of its time sleeping.
   loop
      delay until Next_Release;
      Next_Release := Next_Release + Ada.Real_Time.Milliseconds(1000);
      return;
   end loop;
end Main;
