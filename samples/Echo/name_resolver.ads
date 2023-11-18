--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Message_Types; use CubedOS.Message_Types;

package Name_Resolver is

    -- Core Modules

    -- Application-Specific Modules
    Echo_Client             : constant Module_ID_Type := 10;
    Echo_Server             : constant Module_ID_Type := 11;    
                            
   Domain : aliased constant Domain_Metadata := (2, 1, (Echo_Client, Echo_Server));
end Name_Resolver;
