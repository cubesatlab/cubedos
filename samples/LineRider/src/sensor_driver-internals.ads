--------------------------------------------------------------------------------
-- FILE   : sensor_driver-internals.ads
-- SUBJECT: Specification of a package that implements the main part of the sensor driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;

private package Sensor_Driver.Internals is
   
   procedure Initialize;
   
   procedure Process_Message(Incoming_Message : in Message_Manager.Message_Record);

end Sensor_Driver.Internals;
