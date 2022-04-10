--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package Name_Resolver is

    -- Core Modules
    -- Log_Server               : constant Message_Address := (0, 2);
    -- Publish_Subscribe_Server : constant Message_Address := (0, 3);
    -- Time_Server              : constant Message_Address := (0,4);
    -- Event_Server             : constant Message_Address := (0,5);
    -- File_Server              : constant Message_Address := (0,6);
    -- Table_Server             : constant Message_Address := (0,7);
    -- Interpreter              : constant Message_Address := (0,10);
    Network_Server : constant Module_ID_Type := 1;

    -- Application-Specific Modules
    DomainA_Client : constant Message_Address  := (1,2);
    DomainB_Server : constant Message_Address  := (2,2);

end Name_Resolver;
