--------------------------------------------------------------------------------
-- FILE   : cubedos-log_server.ads
-- SUBJECT: Specification of a package that logs flight software activity.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.Log_Server is

   ID : constant Message_Manager.Module_ID_Type := 2;

end CubedOS.Log_Server;
