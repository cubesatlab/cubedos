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
   pragma Elaborate_Body;

   use Message_Manager;
   use CubedOS.Interpreter.API;

   Public : Public_Mailbox_Owner with Constant_After_Elaboration;

   This_Receives: aliased constant Message_Type_Array := (Clear_Request_Msg,
                                                          Set_Request_Msg,
                                                          Add_Request_Msg);

   procedure Init;

   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end CubedOS.Interpreter.Messages;
