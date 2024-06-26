--------------------------------------------------------------------------------
-- FILE   : led_driver.ads
-- SUBJECT: Top level package of a CubedOS driver for the LEDs.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

pragma Warnings (Off, "unit ""Message_Manager"" is not referenced");
with Message_Manager;

package LED_Driver is

   ID : constant Message_Manager.Module_ID := 5;

end LED_Driver;
