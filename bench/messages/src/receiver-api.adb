--------------------------------------------------------------------------------
-- FILE   : receiver-api.adb
-- SUBJECT: Body of a package that simplifies use of the receiver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Receiver.API is

   function Make_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      Message.Data := (others => 0);
      Message.Size := Message.Data'Length;
      return Message;
   end Make_Message;

end Receiver.API;
