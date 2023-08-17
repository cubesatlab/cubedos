--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;
with CubedOS.Interpreter.API;

package CubedOS.Interpreter.Messages is
   use Message_Manager;
   use CubedOS.Interpreter.API;

   procedure Init
     with Global => (In_Out => (Mailboxes, Lock)),
     Pre => not Module_Registered(This_Module),
     Post => Module_Registered(This_Module);

   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Interpreter.Messages;
