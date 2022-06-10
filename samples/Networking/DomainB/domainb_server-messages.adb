--------------------------------------------------------------------------------
-- FILE   : networking_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--------------------------------------------------------------------------------
with DomainB_Server.API;
with Name_Resolver;
with Ada.Text_IO;

package body DomainB_Server.Messages is
   use Message_Manager;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ping_Request(Message : in Message_Record)
	 with
	   Pre => DomainB_Server.API.Is_Ping_Request(Message)
   is
	  Outgoing_Message : Message_Record;
	  Decode_Status    : Message_Status_Type;
   begin
	  DomainB_Server.API.Ping_Request_Decode(Message, Decode_Status);

	  -- Just ignore messages that don't decode properly (decoding Ping_Requests can't fail anyway).
	  -- Report a failed request	  
	  if Decode_Status = Message_Manager.Success and Message.Request_ID <= 10 then
		Outgoing_Message :=
		  DomainB_Server.API.Ping_Reply_Encode
			(Receiver_Address => Message.Sender_Address,
			 Request_ID       => Message.Request_ID,
			 Status           => DomainB_Server.API.Success);
		 Message_Manager.Route_Message(Outgoing_Message);
      else
		Ada.Text_IO.Put_Line("Report Failure");
	    Outgoing_Message :=
	      DomainB_Server.API.Ping_Reply_Encode
		    (Receiver_Address => Message.Sender_Address,
			 Request_ID       => Message.Request_ID,
			 Status           => DomainB_Server.API.Failure); 
		Message_Manager.Route_Message(Outgoing_Message);
	  end if;
   end Handle_Ping_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record) is
   begin
	  if DomainB_Server.API.Is_Ping_Request(Message) then
		 Handle_Ping_Request(Message);
	  else
		 -- An unknown message type has been received. What should be done about that?
		 null;
	  end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
	  Incoming_Message : Message_Manager.Message_Record;
   begin
	  loop
		 Message_Manager.Fetch_Message(Name_Resolver.DomainB_Server.Module_ID, Incoming_Message);
		 Process(Incoming_Message);
	  end loop;
   end Message_Loop;

end DomainB_Server.Messages;
