--------------------------------------------------------------------------------
-- FILE   : random_number_generator.ads
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

package Random_Number_Generator is

   R_ID    : constant Message_Manager.Request_ID_Type  := 101;
   FSeed   : constant Fibonacci.Fib_Seed               := System_Bus.FSeed + 3;
   Pri     : constant System.Priority                  := 10;
   CPU_Num : constant System.Multiprocessors.CPU_Range := System.Multiprocessors.CPU'First;

end Random_Number_Generator;
