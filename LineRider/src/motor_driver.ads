--------------------------------------------------------------------------------
-- FILE   : motor_driver.ads
-- SUBJECT: Top level package of a CubedOS driver for the motors.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package Motor_Driver is

   ID : constant Message_Manager.Module_ID := 4;

end Motor_Driver;
