--------------------------------------------------------------------------------
-- FILE   : led_driver-api.adb
-- SUBJECT: Body of a package that simplifies use of the LED driver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with LED_Driver.Internals;

package body LED_Driver.API is

   function Turn_On_Message (Sender : Module_ID; LED : LED_Type) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      Message.Data := (others => 0);
      Message.Data(1) := Internals.On_Message;
      Message.Data(2) := LED_Type'Pos(LED);
      Message.Size := 2;
      return Message;
   end Turn_On_Message;


   function Turn_Off_Message(Sender : Module_ID; LED : LED_Type) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      Message.Data := (others => 0);
      Message.Data(1) := Internals.Off_Message;
      Message.Data(2) := LED_Type'Pos(LED);
      Message.Size := 2;
      return Message;
   end Turn_Off_Message;


   function Turn_On_All_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      Message.Data := (others => 0);
      Message.Data(1) := Internals.All_On_Message;
      Message.Size := 1;
      return Message;
   end Turn_On_All_Message;


   function Turn_Off_All_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      Message.Data := (others => 0);
      Message.Data(1) := Internals.All_Off_Message;
      Message.Size := 1;
      return Message;
   end Turn_Off_All_Message;

end LED_Driver.API;
