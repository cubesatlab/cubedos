--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program of the echo client/server CubedOS demonstration program.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Real_Time;
with System;

-- Bring in the necessary modules, both from CubedOS and from this application.
with DomainB_Server.Messages;
with CubedOS.Transport_UDP.Messages;

pragma Unreferenced(CubedOS.Transport_UDP.Messages);
pragma Unreferenced(DomainB_Server.Messages);

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
