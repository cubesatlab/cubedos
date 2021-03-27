--------------------------------------------------------------------------------
-- FILE   : sensor_driver.ads
-- SUBJECT: Top level package of a CubedOS driver for the sensors.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package Sensor_Driver is

   ID : constant Message_Manager.Module_ID := 6;

end Sensor_Driver;
