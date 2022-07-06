--------------------------------------------------------------------------------
-- FILE   : SAMPLE_MODULE-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Message_Manager; use Message_Manager;
with System_Bus.API;
with Random_Number_Generator.API;

package System_Bus.Messages is
   pragma Annotate
     (GNATprove, Intentional,
      "multiple tasks might suspend on protected object",
      "Every module has a unique ID");

   task type Message_Loop with
      Priority => Pri,
      CPU      => CPU_Num
   is
      pragma Storage_Size (4 * 1_024);
   end Message_Loop;

private
   procedure Initialize;
   procedure Process (Message : in Message_Record);
   procedure Handle_Random_Number_Request (Message : in Message_Record) with
      Pre => System_Bus.API.Is_Random_Number_Request (Message);
   procedure Handle_Random_Number_Reply (Message : in Message_Record) with
      Pre => Random_Number_Generator.API.Is_Generate_Number_Reply (Message);
   procedure Handle_Telemetry (Message : in Message_Record) with
      Pre => System_Bus.API.Is_Telemetry (Message);
end System_Bus.Messages;
