--------------------------------------------------------------------------------
-- FILE    : network_configuration.ads
-- SUBJECT : Specification of a package holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
--
-- Each distributed CubedOS application must provide a version of this file that maps "well-
-- known" network addresses and ports to domains in the distributed CubedOS application.
-- The precise ID asssigments are arbitrary.
--
-- Copy this file to your CubedOS application code base and edit it to mention
-- only the address, ports, and domains you need
--------------------------------------------------------------------------------
with GNAT.Sockets;   use GNAT.Sockets;
with Message_Manager; use Message_Manager;

package Network_Configuration is

   Local_Host_Addr : in Inet_Addr_Type := Inet_Addr("127.0.0.1");
   Default_Port : Port_Type := 50000;

   function Get_Port
     (Domain_ID   : in Domain_ID_Type) return Port_Type;

   function Get_Address
     (Domain_ID   : in Domain_ID_Type) return Inet_Addr_Type;

end Network_Configuration;
