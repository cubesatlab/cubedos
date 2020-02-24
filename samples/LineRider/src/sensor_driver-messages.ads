--------------------------------------------------------------------------------
-- FILE   : sensor_driver-messages.ads
-- SUBJECT: Specification of the sensor driver message handler package.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with System;

private package Sensor_Driver.Messages is

   task Message_Loop is
      pragma Storage_Size(4 * 1024);
      pragma Priority(System.Default_Priority);
   end Message_Loop;

end Sensor_Driver.Messages;
