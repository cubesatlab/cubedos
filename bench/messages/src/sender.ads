--------------------------------------------------------------------------------
-- FILE   : sender.ads
-- SUBJECT: Top level package of a CubedOS message sender.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package Sender is

   ID : Message_Manager.Module_ID := 1;

end Sender;
