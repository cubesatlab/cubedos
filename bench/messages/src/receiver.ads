--------------------------------------------------------------------------------
-- FILE   : receiver.ads
-- SUBJECT: Top level package of a CubedOS driver for the receiver.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package Receiver is

   ID : Message_Manager.Module_ID := 2;

end Receiver;
