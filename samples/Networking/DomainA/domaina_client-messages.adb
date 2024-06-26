--------------------------------------------------------------------------------
-- FILE   : echo_client-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Real_Time;
with Name_Resolver;
with DomainB_Server.API;

package body DomainA_Client.Messages is
   use Message_Manager;
   use type Ada.Real_Time.Time;
   use type DomainB_Server.API.Status_Type;

   package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
   package Request_IO  is new Ada.Text_IO.Modular_IO(Request_ID_Type);

   Send_Time        : Ada.Real_Time.Time;
   Receive_Time     : Ada.Real_Time.Time;
   Request_Number   : Request_ID_Type := 0;
   Outgoing_Message : Message_Record;


   procedure Initialize is
   begin
      -- Send the first message!
      Send_Time := Ada.Real_Time.Clock;
      Outgoing_Message := DomainB_Server.API.Ping_Request_Encode
        (Sender_Address => Name_Resolver.DomainA_Client,
         Request_ID     => Request_Number);
      Route_Message(Outgoing_Message);
   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ping_Reply(Message : in Message_Record) with
     Pre => DomainB_Server.API.Is_Ping_Reply(Message)
   is
      Status           : DomainB_Server.API.Status_Type;
      Decode_Status    : Message_Status_Type;
      Round_Trip_Time  : Ada.Real_Time.Time_Span;
   begin
      DomainB_Server.API.Ping_Reply_Decode(Message, Status, Decode_Status);
      Receive_Time    := Ada.Real_Time.Clock;
      Round_Trip_Time := Receive_Time - Send_Time;

      if Decode_Status /= Success then
         Ada.Text_IO.Put_Line("ERROR: Unable to decode a Ping_Reply message!");
      elsif Status /= DomainB_Server.API.Success then
         Ada.Text_IO.Put_Line("ERROR: Networking server reported a ping failure!");
      else
         Ada.Text_IO.Put("+++ Reply #");
         Request_IO.Put(Message.Request_ID);
         Ada.Text_IO.Put(" (RTT = ");
         Duration_IO.Put(Ada.Real_Time.To_Duration(Round_Trip_Time));
         Ada.Text_IO.Put("s)");
         Ada.Text_IO.New_Line;

         delay 2.0;  -- Delay so the human can read the above messages.
         Request_Number := Request_Number + 1;

         -- Send the next message!
         Send_Time := Ada.Real_Time.Clock;
         Outgoing_Message := DomainB_Server.API.Ping_Request_Encode
           (Sender_Address => Name_Resolver.DomainA_Client,
            Request_ID     => Request_Number);
         Route_Message(Outgoing_Message);
      end if;
   end Handle_Ping_Reply;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if DomainB_Server.API.Is_Ping_Reply(Message) then
         Handle_Ping_Reply(Message);
      else
         -- An unknown message type has been received. What should be done about that?
         null;
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Record;
   begin
      Initialize;
      loop
         Message_Manager.Fetch_Message(Name_Resolver.DomainA_Client.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end DomainA_Client.Messages;
