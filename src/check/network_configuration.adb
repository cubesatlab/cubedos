--------------------------------------------------------------------------------
-- FILE    : network_configuration.adb
-- SUBJECT : Package for retrieving addresses and ports for Domains in a distributed CubedOS application
-- AUTHOR  : (C) Copyright 2022 by Vermont Technical College
----------------------------------------------------------------------------------
package body Network_Configuration is 

	function Get_Address
		(Domain_ID   : Domain_ID_Type) return Inet_Addr_Type
	is
	begin
		return Local_Host_Addr;
	end Get_Address;
	
	function Get_Port
		(Domain_ID   : Domain_ID_Type) return Port_Type
	is
	begin
		return Default_Port;
	end Get_Port;

end Network_Configuration;