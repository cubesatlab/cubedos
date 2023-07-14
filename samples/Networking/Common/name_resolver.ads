--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Message_Types; use CubedOS.Message_Types;

package Name_Resolver is

   -- Core Modules
   Name_Resolver  : constant Module_ID_Type := 1;
   Network_Server : constant Module_ID_Type := 2;

   -- Application-Specific Modules
   Ping_Server : constant Module_ID_Type := 3;
   Ping_Client : constant Module_ID_Type  := 4;

   Domain_A : constant Domain_Metadata := (Module_Count => 1,
                                           ID => 1,
                                           Module_IDs => (1 => Ping_Client));
   Domain_B : constant Domain_Metadata := (Module_Count => 1,
                                           ID => 2,
                                           Module_IDs => (1 => Ping_Server));

end Name_Resolver;
