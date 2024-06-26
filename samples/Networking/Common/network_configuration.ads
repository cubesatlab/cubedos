--------------------------------------------------------------------------------
-- FILE    : network_configuration.ads
-- SUBJECT :
-- AUTHOR  : (C) Copyright 2024 by State University
--
--------------------------------------------------------------------------------
with GNAT.Sockets;   use GNAT.Sockets;
with Message_Manager; use Message_Manager;

package Network_Configuration is

   DomainA_Addr : Inet_Addr_Type := Inet_Addr ("127.0.0.1");
   DomainB_Addr : Inet_Addr_Type := Inet_Addr("127.0.0.1");
   Local_Host_Addr : Inet_Addr_Type := Inet_Addr("127.0.0.1");

   DomainA_Port : Port_Type := 50000;
   DomainB_Port : Port_Type := 50001;
   Default_Port : Port_Type := 50000;

   function Get_Port(Domain_ID : in Domain_ID_Type) return Port_Type;

   function Get_Address(Domain_ID : in Domain_ID_Type) return Inet_Addr_Type;

end Network_Configuration;
