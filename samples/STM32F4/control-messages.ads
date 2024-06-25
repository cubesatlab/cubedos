--------------------------------------------------------------------------------
-- FILE   : control.ads
-- SUBJECT: Specification of a package that implements the controller message loop.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;

package Control.Messages is

   task Message_Loop is
      -- pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end Control.Messages;
