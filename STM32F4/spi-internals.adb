--------------------------------------------------------------------------------
-- FILE   : spi-internals.adb
-- SUBJECT: Body of a package that implements the main part of the SPI driver module.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------

package body SPI.Internals is

   procedure Initialize is
   begin
      null;
   end Initialize;
   
   
   procedure Process_Message(Incoming_Message : in Message_Record) is
   begin
      -- Decode the incoming message using API functions if necessary and act on it. When this
      -- procedure returns the message loop will immediately try to receive the next message.
      -- Note that all CubedOS send operations are non-blocking so sending an outgoing message
      -- here will not delay execution.
      null;
   end Process_Message;
   

end SPI.Internals;
