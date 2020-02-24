--------------------------------------------------------------------------------
-- FILE   : led_driver-messages.ads
-- SUBJECT: Specification of the LED driver message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

package LED_Driver.Messages is

   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end LED_Driver.Messages;
