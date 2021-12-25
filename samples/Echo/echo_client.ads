--------------------------------------------------------------------------------
-- FILE   : echo_client.ads
-- SUBJECT: Top level package of the echo client module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;

package Echo_Client is
   ID : constant Message_Manager.Module_ID_Type := 5;
end Echo_Client;
