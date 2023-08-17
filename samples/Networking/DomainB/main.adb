--------------------------------------------------------------------------------
-- FILE   : main.adb
-- SUBJECT: Main program of the echo client/server CubedOS demonstration program.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with System;

-- Bring in the necessary modules, both from CubedOS and from this application.
with Ping_Server.Messages;

procedure Main is
   pragma Priority(System.Priority'First);
begin
   Ping_Server.Messages.Init;
end Main;
