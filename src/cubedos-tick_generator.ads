--------------------------------------------------------------------------------
-- FILE   : cubedos-tick_generator.ads
-- SUBJECT: Specification of a package for a tick generator module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.Tick_Generator is

   ID : constant Message_Manager.Module_ID_Type := 1;

end CubedOS.Tick_Generator;
