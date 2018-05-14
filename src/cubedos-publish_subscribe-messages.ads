--------------------------------------------------------------------------------
-- FILE   : cubedos-publish_subscribe-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with System;

package CubedOS.Publish_Subscribe.Messages
  with
    Abstract_State => Database,
    Initializes => Database
is

   task Message_Loop
     with Global => (In_Out => Database)
   is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

   pragma Annotate
     (GNATprove,
      Intentional,
      "multiple tasks might queue on protected entry",
      "Every module has a unique ID");

end CubedOS.Publish_Subscribe.Messages;
