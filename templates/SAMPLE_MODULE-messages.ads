--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;
with Message_Manager; use Message_Manager;
with CubedOS.SAMPLE_MODULE.API; use CubedOS.SAMPLE_MODULE.API;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package Sample_Module.Messages is

   -- This procedure is called from the main procedure at startup.
   -- It is responsible for registering the module with the messaging
   -- system.
   procedure Init
     with Global => (In_Out => (Mailboxes, Lock)),
     Pre => not Module_Registered(This_Module)
     and Has_Module(This_Domain, This_Module),
     Post => Module_Registered(This_Module);

   -- Every module contains a message loop that receives messages from the module's mailbox and
   -- processes them. That loop is declared here with its attributes. Be mindful of priorities.
   -- We recommend giving every module a unique priority; the correct assignments need to be
   -- considered carefully to meet timing requirements. Similar comments apply to the stack size
   -- setting. We recommend analyzing the code in the module to find the most appropriate stack
   -- size.
   --
   -- This package is a public child because users of the module need to 'with' this package to
   -- ensure it is part of the program (otherwise messages sent to a module will never be
   -- processed! Module users also need to 'with' the API package. However, if the API package
   -- tries to bring in this package circular elaboration loops result. We recommend that the
   -- compilation unit containing the main procedure 'with' the message loop packages of all
   -- modules the program is trying to use. That also serves as documentation about the modules
   -- required by the program. For example in the main compilation unit include something like:
   --
   -- with CubedOS.Sample_Module.Messages;
   -- pragma Unreferenced(CubedOS.Sample_Module.Messages);
   --
   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end Sample_Module.Messages;
