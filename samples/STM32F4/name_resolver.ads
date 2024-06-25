--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package Name_Resolver is

    -- Core Modules
    Publish_Subscribe_Server : constant Message_Address := (0, 1);
    Time_Server              : constant Message_Address := (0, 2);

    -- Application-Specific Modules
    LED_Driver    : constant Message_Address := (0, 3);
    Button_Driver : constant Message_Address := (0, 4);
    Control       : constant Message_Address := (0, 5);

end Name_Resolver;
