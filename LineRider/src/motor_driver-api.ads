--------------------------------------------------------------------------------
-- FILE   : motor_driver-api.ads
-- SUBJECT: Specification of a package that simplifies use of the motor driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;  use Message_Manager;

package Motor_Driver.API is

   type State_Type is (Off, On);

   -- Return a message that commands the motors to go straight data.
   function Drive_Straight_Message(Sender : Module_ID) return Message_Record
     with Global => null;

   -- Return a message that commands the motors to turn left.
   function Turn_Left_Message(Sender : Module_ID) return Message_Record
     with Global => null;

   -- Return a message that commands the motors to turn right.
   function Turn_Right_Message(Sender : Module_ID) return Message_Record
     with Global => null;

end Motor_Driver.API;
