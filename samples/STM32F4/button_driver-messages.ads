--------------------------------------------------------------------------------
-- FILE   : button_driver-messages.ads
-- SUBJECT: Specification of a package that implements the Button driver.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System;


package Button_Driver.Messages is

   pragma Elaborate_Body;

   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;


end Button_Driver.Messages;
