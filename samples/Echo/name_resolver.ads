--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package Name_Resolver is

    -- Core Modules

    -- Application-Specific Modules
    Echo_Client             : constant Message_Address := (0,1);
    Echo_Server             : constant Message_Address := (0,2);

end Name_Resolver;
