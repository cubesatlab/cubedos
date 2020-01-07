--------------------------------------------------------------------------------
-- FILE   : spi.ads
-- SUBJECT: Top level package of the SPI driver module.
-- AUTHOR : (C) Copyright 2015 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package SPI is

   ID : constant Message_Manager.Module_ID_Type := 5;

end SPI;
