--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program of the echo client/server CubedOS demonstration program.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

-- Bring in the necessary modules, both from CubedOS and from this application.
with Ping_Client.Messages;

procedure Main is
   pragma Priority(System.Priority'First);
begin
   Ping_Client.Messages.Init;
   loop
      delay 1.0;
   end loop;
end Main;
