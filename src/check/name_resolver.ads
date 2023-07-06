--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification of a package holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
-- Each CubedOS application must provide a version of this file that maps "well-
-- known" names to (Domain_ID, Module_ID) pairs. The precise ID asssigments are
-- arbitrary. CubedOS applications should use the names to get the application-
-- specific ID assignments.
--
-- Each domain must instantiate the generic message manager to include enough
-- mailboxes to hold the highest numbered Module_ID in that domain. That instantiation
-- *must* be called Message_Manager (this name is hard-coded in the modules). One
-- consequence of this is that each domain must be in a separate executable.
--
-- Copy this file to your CubedOS application code base and edit it to mention
-- only the modules you need, including your application-specific modules. Assign
-- IDs "compactly" (with no gaps in the numbering) so that you can create a
-- Message_Manager instantiation with the minimum number of mailboxes.
--------------------------------------------------------------------------------
with CubedOS.Message_Types; use CubedOS.Message_Types;

package Name_Resolver is

   -- Core Modules.
   -- Declarations that are commented out are for currently unimplemented core modules.

   -- Note that the "Name_Resolver" module is reserved for a dynamic name resolver,
   -- which may never get implemented depending on how well this static system works!
   -- Network_Server is also has a reserved module ID (for now).

   Name_Resolver            : constant Module_ID_Type := 1;
   Network_Server           : constant Module_ID_Type := 2;

   Log_Server               : constant Module_ID_Type := 3;
   Publish_Subscribe_Server : constant Module_ID_Type := 4;
   Time_Server              : constant Module_ID_Type := 5;
   --Time_Server              : constant Module_ID_Type := 5;
   -- Event_Server             : constant Module_ID_Type := 6;
   File_Server              : constant Module_ID_Type := 7;
   -- Table_Server             : constant Module_ID_Type := 8;
   Interpreter              : constant Module_ID_Type := 9;

   Domain : constant Domain_Declaration := (5, 0,
                                            (Log_Server,
                                            Publish_Subscribe_Server,
                                            Time_Server,
                                            File_Server,
                                            Interpreter));


   -- Application-Specific Modules.
   -- Make up names as you see fit (typically the same as your module's top level package).
   -- Be sure there are no duplicate (Domain_ID, Module_ID) pairs.
   -- The names below are examples.

   -- Echo_Client             : constant Message_Address := (0, 3);
   -- Echo_Server             : constant Message_Address := (0, 4);

end Name_Resolver;
