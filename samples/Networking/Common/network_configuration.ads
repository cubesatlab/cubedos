--------------------------------------------------------------------------------
-- FILE    : udp_service.ads
-- AUTHOR  : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with GNAT.Sockets;   use GNAT.Sockets;
with CubedOS.Message_Types; use CubedOS.Message_Types;

package Network_Configuration is

	DomainA_Addr : constant Inet_Addr_Type := Inet_Addr ("127.0.1.1");
	DomainB_Addr : constant Inet_Addr_Type := Inet_Addr("127.0.1.1");
	Local_Host_Addr : constant Inet_Addr_Type := Inet_Addr("127.0.1.1");

	DomainA_Port : constant Port_Type := 50000;
	DomainB_Port : constant Port_Type := 50001;
	Default_Port : constant Port_Type := 50000;

	function Get_Port
	 (Domain_ID   : Domain_ID_Type) return Port_Type;

	function Get_Address
  	(Domain_ID   : Domain_ID_Type) return Inet_Addr_Type;

end Network_Configuration;
