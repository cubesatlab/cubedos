package body Network_Configuration is 

	function Get_Address
		(Domain_ID   : Domain_ID_Type) return Inet_Addr_Type
	is
		Domain_Addr : Inet_Addr_Type;
	begin
		if Domain_ID = 1 then
			Domain_Addr := DomainA_Addr;
		elsif Domain_ID = 2 then
			Domain_Addr := DomainB_Addr;
		else
			Domain_Addr := Local_Host_Addr;
		end if;
		return Domain_Addr;
	end Get_Address;
	
	function Get_Port
		(Domain_ID   : Domain_ID_Type) return Port_Type
	is
		Port : Port_Type;
	begin
		if Domain_ID = 1 then
			Port := DomainA_Port;
		elsif Domain_ID = 2 then
			Port := DomainB_Port;
		else
			Port := Default_Port;
		end if;
		return Port;
	end Get_Port;

end Network_Configuration;