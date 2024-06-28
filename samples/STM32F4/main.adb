--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program of the STM32F4 demonstration.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with Ada.Real_Time;
with System;

-- Bring in the necessary modules, both from CubedOS and from this application.
with CubedOS.Log_Server.Messages;
with CubedOS.Publish_Subscribe_Server.Messages;
with CubedOS.Time_Server.Messages;

with LED_Driver.Messages;
with Button_Driver.Messages;
with Control.Messages;

pragma Unreferenced(CubedOS.Log_Server.Messages);
pragma Unreferenced(CubedOS.Publish_Subscribe_Server.Messages);
pragma Unreferenced(CubedOS.Time_Server.Messages);

pragma Unreferenced(LED_Driver.Messages);
pragma Unreferenced(Button_Driver.Messages);
pragma Unreferenced(Control.Messages);


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
