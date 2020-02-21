--------------------------------------------------------------------------------
-- FILE   : control.ads
-- SUBJECT: Specification of a package that implements the controller message loop.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;

package Control.Messages is

   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

   -- No longer needed?
   --
   --pragma Annotate
   --  (GNATprove,
   --   Intentional,
   --   "multiple tasks might suspend on protected object",
   --   "Every module has a unique ID");

end Control.Messages;
