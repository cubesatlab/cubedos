--------------------------------------------------------------------------------
-- FILE   : sensor_driver-api.ads
-- SUBJECT: Specification of a package that simplifies use of the sensor driver.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;

package Sensor_Driver.API is

   type State_Type is (Off, On);

   -- Return a message containing sensor data.
   function Sensor_Data_Message(Sender : Module_ID_Type) return Message_Record
     with Global => null;

end Sensor_Driver.API;
