--------------------------------------------------------------------------------
-- FILE   : led_driver-internals.ads
-- SUBJECT: Specification of a package that implements the main part of the LED driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;
with Registers;

use Message_Manager;

private package LED_Driver.Internals is

   On_Message : constant Octet := 1;
   Off_Message : constant Octet := 2;
   All_On_Message : constant Octet := 3;
   All_Off_Message : constant Octet := 4;

   procedure Initialize
     with Global => (In_Out => (Registers.RCC, Registers.GPIOD));

   procedure Process_Message(Incoming_Message : in Message_Record)
     with Global => (In_Out => Registers.GPIOD);

end LED_Driver.Internals;
