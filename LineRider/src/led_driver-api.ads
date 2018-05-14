--------------------------------------------------------------------------------
-- FILE   : led_driver-api.ads
-- SUBJECT: Specification of a package that simplifies use of the LED driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
-- All the subprograms in this package must be task safe. They can be simultaneously called
-- from multiple tasks. If possible, make every subprogram here a pure function.
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

use Message_Manager;

package LED_Driver.API is

   type LED_Type is (Green, Orange, Red, Blue);

   -- Return a message for turning on the given LED.
   function Turn_On_Message(Sender : Module_ID; LED : LED_Type) return Message_Record
     with Global => null;

   -- Return a message for turning off the given LED.
   function Turn_Off_Message(Sender : Module_ID; LED : LED_Type) return Message_Record
     with Global => null;

   -- Return a message for turning on all LEDs.
   function Turn_On_All_Message(Sender : Module_ID) return Message_Record
     with Global => null;

   -- Return a message for turning off all LEDs.
   function Turn_Off_All_Message(Sender : Module_ID) return Message_Record
     with Global => null;

end LED_Driver.API;
