--------------------------------------------------------------------------------
-- FILE   : cubedos-logger.ads
-- SUBJECT: Specification of a package that logs flight software activity.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.Logger is

   ID : constant Message_Manager.Module_ID_Type := 2;

end CubedOS.Logger;
