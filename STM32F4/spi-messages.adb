--------------------------------------------------------------------------------
-- FILE   : spi-messages.adb
-- SUBJECT: Body of the message handler package for the SPI driver module.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;
with SPI.Internals;

package body SPI.Messages is

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      -- Initialize the internal workings of the module before processing any of its messages.
      -- It may instead be appropriate for the Internals package to initialize itself at
      -- elaboration time. We recommend that all CubedOS applications use the configuration
      -- pragma of Partition_Elaboration_Policy set to Sequential. This ensures that all
      -- packages are elaborated before any library level tasks start.
      Internals.Initialize;

      -- Process messages as they arrive. This simple loop may be all that is needed.
      loop
         Message_Manager.Fetch_Message(ID, Incoming_Message);
         Internals.Process_Message(Incoming_Message);
      end loop;
   end Message_Loop;

end SPI.Messages;
