--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server-messages.ads
-- SUBJECT: Specification of a package that implements the main part of the file server.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Ravenscar);
pragma Partition_Elaboration_Policy(Sequential);

with System;

package CubedOS.File_Server.Messages is

   task Message_Loop is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

   pragma Annotate
     (GNATprove,
      Intentional,
      "multiple tasks might queue on protected entry",
      "Every module has a unique ID");

end CubedOS.File_Server.Messages;
