--------------------------------------------------------------------------------
-- FILE   : echo_client-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Real_Time;

with Echo_Server.API;
with Name_Resolver;
with CubedOS.Log_Server.API;

package body Echo_Client.Messages is
   use Message_Manager;
   use type Ada.Real_Time.Time;
   use type Echo_Server.API.Status_Type;

   Send_Time      : Ada.Real_Time.Time;
   Receive_Time   : Ada.Real_Time.Time;
   Request_Number : Request_ID_Type := 0;

   Outgoing_Message : Message_Record;


   procedure Initialize is
   begin
      -- Send the first message!
     Send_Time := Ada.Real_Time.Clock;
     Outgoing_Message := Echo_Server.API.Ping_Request_Encode
        (Sender_Address => Name_Resolver.Echo_Client,
         Request_ID    => Request_Number);
      Route_Message(Outgoing_Message);
   end Initialize;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Ping_Reply(Message : in Message_Record) with
      Pre => Echo_Server.API.Is_Ping_Reply(Message)
   is
      Status           : Echo_Server.API.Status_Type;
      Decode_Status    : Message_Status_Type;
      Round_Trip_Time  : Ada.Real_Time.Time_Span;
   begin
      Echo_Server.API.Ping_Reply_Decode(Message, Status, Decode_Status);
      Receive_Time := Ada.Real_Time.Clock;
      Round_Trip_Time := Receive_Time - Send_Time;

      if Decode_Status /= Success then
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Echo_Client,
                                            CubedOS.Log_Server.API.Error,
                                            "Unable to decode a Ping_Reply message!");
      elsif Status /= Echo_Server.API.Success then
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Echo_Client,
                                            CubedOS.Log_Server.API.Error,
                                            "Echo server reported a ping failure!");
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Echo_Client,
                                            CubedOS.Log_Server.API.Informational,
                                            "+++ Reply #" &
                                              Request_ID_Type'Image(Message.Request_ID) &
                                              " (RTT = " &
                                              Duration'Image(Ada.Real_Time.To_Duration(Round_Trip_Time)) &
                                              "s)");

         delay 1.0;  -- Delay for a while so the human can read the above messages.
         Request_Number := Request_Number + 1;

         -- Send the next message!
         Send_Time := Ada.Real_Time.Clock;
         Outgoing_Message := Echo_Server.API.Ping_Request_Encode
           (Sender_Address => Name_Resolver.Echo_Client,
            Request_ID    => Request_Number);
         Route_Message(Outgoing_Message);
      end if;

  end Handle_Ping_Reply;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message at a time.
   procedure Process(Message : in Message_Record) is
   begin
      if Echo_Server.API.Is_Ping_Reply(Message) then
         Handle_Ping_Reply(Message);
      else
         CubedOS.Log_Server.API.Log_Message(Name_Resolver.Echo_Client,
                                            CubedOS.Log_Server.API.Error,
                                            "An unknown message type has been received!");
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
         Message_Manager.Fetch_Message(Name_Resolver.Echo_Client.Module_ID, Incoming_Message);
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end Echo_Client.Messages;
