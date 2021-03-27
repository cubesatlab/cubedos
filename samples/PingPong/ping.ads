--------------------------------------------------------------------------------
-- FILE   : ping.ads
-- SUBJECT: Top level package of the Ping module.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;

package Ping is

   ID : constant Message_Manager.Module_ID_Type := 5;

   R_ID : constant Message_Manager.Request_ID_Type := 2;

end Ping;
