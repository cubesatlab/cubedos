--------------------------------------------------------------------------------
-- FILE   : cubedos-time_server.ads
-- SUBJECT: Specification of a package for a time server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.Time_Server is

   ID : constant Message_Manager.Module_ID_Type := 4;

end CubedOS.Time_Server;
