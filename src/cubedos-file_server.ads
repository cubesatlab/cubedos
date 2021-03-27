--------------------------------------------------------------------------------
-- FILE   : cubedos-file_server.ads
-- SUBJECT: Specification of a package for a file server module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Message_Manager;

package CubedOS.File_Server is

    ID : constant Message_Manager.Module_ID_Type := 6;

end CubedOS.File_Server;
