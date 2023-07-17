--------------------------------------------------------------------------------
-- FILE   : networking_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2022 by Vermont Technical College
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Text_IO;

package body Ping_Server.Messages is

   Mailbox : constant Module_Mailbox := Make_Module_Mailbox(This_Module, Mail_Target);

   procedure Init is
   begin
      Register_Module(Mailbox, 8);
   end Init;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ping_Request(Message : in Message_Record)
	 with
       Pre => Is_Ping_Request(Message)
       and Payload(Message) /= null
   is
	  Decode_Status    : Message_Status_Type;
   begin
      Ping_Request_Decode(Message, Decode_Status);

	  -- Just ignore messages that don't decode properly (decoding Ping_Requests can't fail anyway).
	  -- Report a failed request
      if Decode_Status /= Success then
         Ada.Text_IO.Put_Line("Report Failure");
      else
         Ada.Text_IO.Put_Line("Received Ping Message");
      end if;

      Send_Ping_Reply
        (Sender => Mailbox,
         Receiver_Address => Sender_Address(Message),
         Request_ID       => Request_ID(Message));
   end Handle_Ping_Request;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record)
     with Pre => Payload(Message) /= null
   is
   begin
	  if Is_Ping_Request(Message) then
         Handle_Ping_Request(Message);
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Record;
   begin
      Message_Manager.Wait;

      loop
         Read_Next(Mailbox, Incoming_Message);
         Process(Incoming_Message);
         Delete(Incoming_Message);
         pragma Loop_Invariant(Payload(Incoming_Message) = null);
      end loop;
   end Message_Loop;

end Ping_Server.Messages;
