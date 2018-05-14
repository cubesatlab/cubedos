--------------------------------------------------------------------------------
-- FILE   : receiver-internals.adb
-- SUBJECT: Body of a package that implements the main part of the receiver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with LEDs;

package body Receiver.Internals is

   Message_Count : Natural := 0;


   procedure Process_Message(Incoming_Message : in Message_Record) is
      pragma Unreferenced(Incoming_Message);
   begin
      Message_Count := Message_Count + 1;
      if Message_Count = 1_000_000 then
         LEDs.On(LEDs.Green);
      end if;
   end Process_Message;


end Receiver.Internals;

