--------------------------------------------------------------------------------
-- FILE   : led_driver.ads
-- SUBJECT: Top level package of a CubedOS LED driver for the STM32F4
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package LED_Driver is

   ID : constant Message_Manager.Module_ID_Type := 4;

   type LED_Type is (Green, Orange, Red, Blue);

end LED_Driver;
