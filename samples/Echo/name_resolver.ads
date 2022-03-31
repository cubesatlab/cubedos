--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Message_Manager;

package Name_Resolver is

   type Message_Address is record
      Domain_ID : MEssage_Manager.Domain_ID_Type;
      Module_ID : MEssage_Manager.Module_ID_Type;
   end record;

   -- Core Modules Should have the same Module IDs on each domain
   -- Where do we define the number of domains?

    -- 1. Name resolver (NOT IMPLEMENTED. Used for dynamic module ID assignments) do we need this?
    Log_Server               : constant Message_Address := (0, 2);
    Publish_Subscribe_Server : constant Message_Address := (0, 3);
    Time_Server              : constant Message_Address := (0,4);
    Event_Server             : constant Message_Address := (0,5);
    File_Server              : constant Message_Address := (0,6);
    Table_Server             : constant Message_Address := (0,7);

    -- Application-Specific Modules
    -- 5. Echo_Client
    -- 6. Echo_Server
    Echo_Client             : constant Message_Address := (0,8);
    Echo_Server             : constant Message_Address := (0,9);
end Name_Resolver;