--------------------------------------------------------------------------------
-- FILE   : ping_client-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2023 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Text_IO;
with Ada.Real_Time;
with Name_Resolver;
with CubedOS.Message_Types;
with Ping_Server.API;

package body Ping_Client.Messages is
   use type Ada.Real_Time.Time;
   use CubedOS.Message_Types;

   package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
   package Request_IO  is new Ada.Text_IO.Modular_IO(Request_ID_Type);

   Mailbox : constant Module_Mailbox := Make_Module_Mailbox(This_Module, Mail_Target);

   Send_Time      : Ada.Real_Time.Time;
   Receive_Time   : Ada.Real_Time.Time;
   Request_Number : Request_ID_Type := 0;

   Outgoing_Message : Message_Record;

   procedure Init is
   begin
      Register_Module(Mailbox, 8);
   end Init;

   procedure Initialize is
   begin
	  -- Send the first message!
	  Send_Time := Ada.Real_Time.Clock;
	  Ping_Server.API.Send_Ping_Request
        (Sender => Mailbox,
         Receiver_Address => (Name_Resolver.Domain_B.ID, Name_Resolver.Ping_Server),
		 Request_ID    => Request_Number);
   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ping_Reply(Message : in Message_Record) with
	 Pre => Ping_Server.API.Is_Ping_Reply(Message)
   is
	  Decode_Status    : Message_Status_Type;
	  Round_Trip_Time  : Ada.Real_Time.Time_Span;
   begin
	  Ping_Server.API.Ping_Reply_Decode(Message, Decode_Status);
	  Receive_Time := Ada.Real_Time.Clock;
	  Round_Trip_Time := Receive_Time - Send_Time;

	  if Decode_Status /= Success then
		 Ada.Text_IO.Put_Line("ERROR: Unable to decode a Ping_Reply message!");
	  else
		 Ada.Text_IO.Put("+++ Reply #");
		 Request_IO.Put(Request_ID(Message));
		 Ada.Text_IO.Put(" (RTT = ");
		 Duration_IO.Put(Ada.Real_Time.To_Duration(Round_Trip_Time));
		 Ada.Text_IO.Put("s)");
		 Ada.Text_IO.New_Line;

		 delay 1.0;  -- Delay for a while so the human can read the above messages.
		 Request_Number := Request_Number + 1;

		 -- Send the next message!
		 Send_Time := Ada.Real_Time.Clock;
		 Ping_Server.API.Send_Ping_Request
           (Sender => Mailbox,
            Receiver_Address => (Name_Resolver.Domain_B.ID, Name_Resolver.Ping_Server),
                    Request_ID    => Request_Number);
		 Route_Message(Outgoing_Message);
	  end if;

	  -- Do math on time spent on message (possibly faster by saving till end?)
	  --Relative_Time     := Ada.Real_Time.Clock - Start_Time;
	  --Relative_Duration := Ada.Real_Time.To_Duration (Relative_Time);
	  --Total_Time := Total_Time + Ada.Real_Time.To_Duration (Relative_Time);

	  -- Print information about the ping
	  --Ada.Text_IO.Put ("+++ Ping ");
	  --Ada.Integer_Text_IO.Put (Item => i, Width => 0, Base => 10);

	  --Put (" | Request ID: " & Request_ID_Type'Image (Message.Request_ID));

	  --Ada.Text_IO.Put_Line (" | Received PINGED");
	  --Put ("Ping");
	  --Put (i'Image);
	  --Put (" Time Duration:   ");
	  --Put (Relative_Duration);
	  --New_Line;
	  -- Wait a bit.
	  -- delay(2.5);

   end Handle_Ping_Reply;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
	  if Ping_Server.API.Is_Ping_Reply(Message) then
		 Handle_Ping_Reply(Message);
	  end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Record;
   begin
      Message_Manager.Wait;
      Initialize;
      loop
         Read_Next(Mailbox, Incoming_Message);
         Process(Incoming_Message);
         Delete(Incoming_Message);
         pragma Loop_Invariant(Payload(Incoming_Message) = null);
      end loop;
   end Message_Loop;

end Ping_Client.Messages;
