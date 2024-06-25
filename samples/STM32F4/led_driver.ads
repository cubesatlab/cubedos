--------------------------------------------------------------------------------
-- FILE   : led_driver.ads
-- SUBJECT: Top level package of a CubedOS LED driver for the STM32F4
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

pragma Warnings (Off, "unit ""Message_Manager"" is not referenced");
with Message_Manager;

package LED_Driver is
   type LED_Type is (Green, Orange, Red, Blue);
end LED_Driver;
