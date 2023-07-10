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
--with CubedOS.Transport_UDP.Messages;

--pragma Unreferenced(CubedOS.Transport_UDP.Messages);

procedure Main is

   pragma Priority(System.Priority'First);
   use type Ada.Real_Time.Time;
   Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(1000);
begin
   CubedOS.Interpreter.Messages.Init;
   CubedOS.Log_Server.Messages.Init;
   CubedOS.Publish_Subscribe_Server.Messages.Init;
   CubedOS.File_Server.Messages.Init;
   CubedOS.Time_Server.Messages.Init;

   -- This loop does nothing at the lowest priority. It spends most of its time sleeping.
   loop
      delay until Next_Release;
      Next_Release := Next_Release + Ada.Real_Time.Milliseconds(1000);
      return;
   end loop;
end Main;
