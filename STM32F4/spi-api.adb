--------------------------------------------------------------------------------
-- FILE   : spi-api.adb
-- SUBJECT: Body of a package that simplifies use of the SPI driver module.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Lib.XDR;

use CubedOS.Lib;

package body SPI.API is

   function Operation_Message(Sender : Module_ID) return Message_Record is
      Message : Message_Record;
   begin
      Message.Sender := Sender;
      -- Fill in the message by encoding the parameters as required.
      return Message;
   end Operation_Message;
   
   
   function Is_Operation_Reply(Message : Message_Record) return Boolean is
   begin
      -- Check (as quickly as possible) if Message is a reply from 'Operation'.
      return False;
   end Is_Operation_Reply;

   
   procedure Operation_Reply
     (Message : in  Message_Record;
      Status  : out Message_Status_Type)
   is
   begin
      -- Decode the given message and return via out parameters (not shown) the fields.
      null;
   end Operation_Reply;

end SPI.API;
