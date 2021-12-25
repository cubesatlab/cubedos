--------------------------------------------------------------------------------
-- FILE   : echo_server.ads
-- SUBJECT: Top level package of the echo server module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------

with Message_Manager;

package Echo_Server is
   ID : constant Message_Manager.Module_ID_Type := 6;
end Echo_Server;
