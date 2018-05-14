--------------------------------------------------------------------------------
-- FILE   : cubedos-cfdp-messages.adb
-- SUBJECT: A package containing the CFDP message loop.
-- AUTHOR : (C) Copyright 2016 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with System;

package CubedOS.CFDP.Messages is

   task Message_Loop is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

   pragma Annotate
     (GNATprove,
      Intentional,
      "multiple tasks might queue on protected entry",
      "Every module has a unique ID");

end CubedOS.CFDP.Messages;
