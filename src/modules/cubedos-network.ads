--------------------------------------------------------------------------------
-- FILE   : cubedos-network.ads
-- SUBJECT: Specification of a package that logs flight software activity.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.Network is

   ID : constant Message_Manager.Module_ID_Type := 8;

end CubedOS.Network;
