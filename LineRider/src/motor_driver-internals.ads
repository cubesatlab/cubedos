--------------------------------------------------------------------------------
-- FILE   : motor_driver-internals.ads
-- SUBJECT: Specification of a package that implements the main part of the motor driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;

private package Motor_Driver.Internals is
   
   procedure Initialize;
   
   procedure Process_Message(Incoming_Message : in Message_Manager.Message_Record);

end Motor_Driver.Internals;
