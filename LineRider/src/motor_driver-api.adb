--------------------------------------------------------------------------------
-- FILE   : motor_driver-api.adb
-- SUBJECT: Body of a package that simplifies use of the motor driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Motor_Driver.Messages;
pragma Unreferenced(Motor_Driver.Messages);

package body Motor_Driver.API is

   function Drive_Straight_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      return Message;
   end Drive_Straight_Message;


   function Turn_Left_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      return Message;
   end Turn_Left_Message;


   function Turn_Right_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      return Message;
   end Turn_Right_Message;

end Motor_Driver.API;
