--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with Message_Manager; use Message_Manager;

package Name_Resolver is

   -- Core Modules
   Name_Resolver  : constant Module_ID_Type := 1;
   Network_Server : constant Module_ID_Type := 2;

   -- Application-Specific Modules
   DomainA_Client : constant Message_Address  := (1, 3);
   DomainB_Server : constant Message_Address  := (2, 3);

end Name_Resolver;
