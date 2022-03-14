--------------------------------------------------------------------------------
-- FILE   : networking_server-api.adb
-- SUBJECT: Body of a package that simplifies use of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with CubedOS.Lib.XDR;
use  CubedOS.Lib;

package body DomainB_Server.API is
   use type XDR.XDR_Unsigned;

   function Ping_Request_Encode
	 (Sender_Domain   : Domain_ID_Type;
	  Receiver_Domain : Domain_ID_Type;
	  Sender          : Module_ID_Type;
	  Request_ID      : Request_ID_Type;
	  Priority        : System.Priority := System.Default_Priority) return Message_Record
   is
	  Message : constant Message_Record :=
		Make_Empty_Message
		  (Sender_Domain   => Sender_Domain,
		   Receiver_Domain => Receiver_Domain,
		   Sender          => Sender,
		   Receiver        => ID,
		   Request_ID      => Request_ID,
		   Message_ID      => Message_Type'Pos(Ping_Request),
		   Priority        => Priority);
   begin
	  return Message;
   end Ping_Request_Encode;


   function Ping_Reply_Encode
	 (Receiver_Domain : Domain_ID_Type;
	  Receiver   : Module_ID_Type;
	  Request_ID : Request_ID_Type;
	  Status     : Status_Type;
	  Priority   : System.Priority := System.Default_Priority) return Message_Record
   is
	  Message : Message_Record :=
		Make_Empty_Message
		  (Sender_Domain   => Domain_ID,
		   Receiver_Domain => Receiver_Domain,
		   Sender          => ID,
		   Receiver        => Receiver,
		   Request_ID      => Request_ID,
		   Message_ID      => Message_Type'Pos(Ping_Reply),
		   Priority        => Priority);

	  Position : Data_Index_Type;
	  Last     : Data_Index_Type;
   begin
	  Position := 0;
	  XDR.Encode(XDR.XDR_Unsigned(Status_Type'Pos(Status)), Message.Payload, Position, Last);
	  Position := Last + 1;

	  Message.Size := Last + 1;
	  return Message;
   end Ping_Reply_Encode;


   procedure Ping_Request_Decode
	 (Message : in Message_Record;
	  Decode_Status : out Message_Status_Type)
   is
   begin
	  -- Decoding can never fail in this case because there are no message parameters.
	  Decode_Status := Success;
   end Ping_Request_Decode;


   procedure Ping_Reply_Decode
	 (Message : in Message_Record;
	  Status : out Status_Type;
	  Decode_Status : out Message_Status_Type)
   is
	  Position  : Data_Index_Type;
	  Last      : Data_Index_Type;
	  Raw_Value : XDR.XDR_Unsigned;
   begin
	  Position := 0;

	  XDR.Decode(Message.Payload, Position, Raw_Value, Last);
	  Position := Last + 1;

	  if Raw_Value > Status_Type'Pos(Status_Type'Last) then
		 Decode_Status := Malformed;
	  else
		 Status := Status_Type'Val(Raw_Value);
		 Decode_Status := Success;
	  end if;
   end Ping_Reply_Decode;


end DomainB_Server.API;
