--------------------------------------------------------------------------------
-- FILE   : Telemetry.ads
-- SUBJECT: Top level package of a CubedOS Random Number Generator.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with System;
with System.Multiprocessors;
with Fibonacci; use Fibonacci;
with System_Bus;

package Telemetry is

   R_ID    : constant Message_Manager.Request_ID_Type  := 102;
   FSeed   : constant Fibonacci.Fib_Seed               := System_Bus.FSeed + 2;
   Pri     : constant System.Priority                  := 20;
   CPU_Num : constant System.Multiprocessors.CPU_Range := System.Multiprocessors.CPU'First;

end Telemetry;
