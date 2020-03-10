--------------------------------------------------------------------------------
-- FILE   : button_driver.ads
-- SUBJECT: Top level package of a CubedOS Button driver for the STM32F4
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package Button_Driver is

   ID : constant Message_Manager.Module_ID_Type := 3;

end Button_Driver;
