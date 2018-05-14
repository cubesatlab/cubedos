--------------------------------------------------------------------------------
-- FILE   : cfdp.ads
-- SUBJECT: Top level package of a CFDP implementation for IceCube.
-- AUTHOR : (C) Copyright 2016 by Vermont Technical College
--
-- This module implements the CCSDS File Delivery Protocol (CFDP).
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.CFDP is

   ID : constant Message_Manager.Module_ID_Type := 4;

end CubedOS.CFDP;
