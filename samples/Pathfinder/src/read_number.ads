--------------------------------------------------------------------------------
--
--  FILE   : Read_Number.ads
--  SUBJECT: Top level package of a CubedOS Random Number Generator.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--  This module is a skeleton, with comments, showing how to set up a
--  standard CubedOS module.  CubedOS modules do not need to follow
--  this structure exactly, but it is recommended to at least start
--  here. Note that many of the comments should be edited/erased in a
--  real application.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

--  All CubedOS applications should instantiate Generic_Message_Manager
--  using some suitable name.  The name Message_Manager is recommended
--  but not required.  In fact, some CubedOS applications may wish to
--  create multiple "communication domains" using multiple message
--  managers with different instantiation arguments.  However, this
--  sample does not demonstrate that feature.
--
--  The message manager package contains the Mailboxes array that is
--  used by CubedOS for inter-module communication.
with Message_Manager;
with System;
with System.Multiprocessors;
with Fibonacci;

package Read_Number is

   --  Every module has an ID number. CubedOS core modules have "well
   --  known" ID numbers that should not be used in an application
   --  specific module. ID numbers are statically allocated.  We
   --  recommend creating a file, module_map.txt, listing these
   --  allocations. The value 1 below is just an example.
   ID      : constant Message_Manager.Module_ID_Type   := 2;
   R_ID    : constant Message_Manager.Request_ID_Type  := 5;
   FSeed   : constant Fibonacci.Fib_Seed               := 40;
   Pri     : constant System.Priority                  := 30;
   CPU_Num : constant System.Multiprocessors.CPU_Range := 1;

end Read_Number;
