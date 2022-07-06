--------------------------------------------------------------------------------
-- FILE   : system_bus.ads
-- SUBJECT: Top level package of a CubedOS SAMPLE MODULE.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
-- System_Bus is intended to be the main interface for the Pathfinder sample program.
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with System;
with System.Multiprocessors;
with Fibonacci;

package System_Bus is

   R_ID    : constant Message_Manager.Request_ID_Type  := 104;
   FSeed   : constant Fibonacci.Fib_Seed               := 37;
   Pri     : constant System.Priority                  := 40;
   CPU_Num : constant System.Multiprocessors.CPU_Range := System.Multiprocessors.CPU'First;

end System_Bus;
