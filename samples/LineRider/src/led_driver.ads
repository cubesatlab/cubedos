--------------------------------------------------------------------------------
-- FILE   : led_driver.ads
-- SUBJECT: Top level package of a CubedOS driver for the LEDs.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package LED_Driver is

   ID : constant Message_Manager.Module_ID := 2;

end LED_Driver;
