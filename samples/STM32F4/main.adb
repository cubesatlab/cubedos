--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program of the Ping/Pong CubedOS demonstration program.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Real_Time;
with System;

-- Bring in the necessary modules, both from CubedOS and from this application.

with CubedOS.Time_Server.Messages;
with Control.Messages;
with LED_Driver.Messages;
with Button_Driver.Messages;
with Button;
with CubedOS.Publish_Subscribe_Server.Messages;

pragma Unreferenced(CubedOS.Time_Server.Messages);
pragma Unreferenced(Control.Messages);
pragma Unreferenced(LED_Driver.Messages);
pragma Unreferenced(Button_Driver.Messages);
pragma Unreferenced(Button);
pragma Unreferenced(CubedOS.Publish_Subscribe_Server.Messages);


procedure Main is

   pragma Priority(System.Priority'First);
   use type Ada.Real_Time.Time;
   Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(1000);
begin
   -- This loop does nothing at the lowest priority. It spends most of its time sleeping.
   loop
      delay until Next_Release;
      Next_Release := Next_Release + Ada.Real_Time.Milliseconds(1000);
   end loop;
end Main;
