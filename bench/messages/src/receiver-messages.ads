--------------------------------------------------------------------------------
-- FILE   : receiver-messages.ads
-- SUBJECT: Specification of the receiving message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
-- pragma SPARK_Mode(On);

with System;

package Receiver.Messages is

   task Message_Loop is
      pragma Storage_Size(8 * 1024);
      pragma Priority(System.Default_Priority + 1);
   end Message_Loop;

end Receiver.Messages;
