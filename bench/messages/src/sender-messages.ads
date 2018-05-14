--------------------------------------------------------------------------------
-- FILE   : sender-messages.ads
-- SUBJECT: Specification of the sender message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
-- pragma SPARK_Mode(On);

with System;

package Sender.Messages is

   task Message_Loop is
      pragma Storage_Size(8 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end Sender.Messages;
