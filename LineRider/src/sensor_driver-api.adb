--------------------------------------------------------------------------------
-- FILE   : sensor_driver-api.adb
-- SUBJECT: Body of a package that simplifies use of the sensor driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Sensor_Driver.Messages;
pragma Unreferenced(Sensor_Driver.Messages);

package body Sensor_Driver.API is

   function Sensor_Data_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      return Message;
   end Sensor_Data_Message;

end Sensor_Driver.API;
