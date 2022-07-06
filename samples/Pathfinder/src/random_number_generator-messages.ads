--------------------------------------------------------------------------------
--
--  FILE   : random_number_generator-messages.ads
--  SUBJECT: Specification of a package that implements the main part
--  of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Message_Manager; use Message_Manager;
with Random_Number_Generator.API;

package Random_Number_Generator.Messages is

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
   procedure Handle_Generate_Number_Request (Message : in Message_Record) with
      Pre => Random_Number_Generator.API.Is_Generate_Number_Request (Message);

end Random_Number_Generator.Messages;
