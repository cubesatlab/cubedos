--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package Name_Resolver is

    -- Core Modules
    Log_Server               : constant Message_Address := (0, 2);
    Publish_Subscribe_Server : constant Message_Address := (0, 3);
    Time_Server              : constant Message_Address := (0,4);
    Event_Server             : constant Message_Address := (0,5);
    File_Server              : constant Message_Address := (0,6);
    Table_Server             : constant Message_Address := (0,7);

    -- Application-Specific Modules
    Echo_Client             : constant Message_Address := (0,8);
    Echo_Server             : constant Message_Address := (0,9);
    
end Name_Resolver;