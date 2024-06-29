--------------------------------------------------------------------------------
-- FILE   : cubedos-log_server-messages.adb
-- SUBJECT: Body of a package that implements the main part of the log server.
-- AUTHOR : (C) Copyright 2024 by Vermont State University
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with CubedOS.Log_Server.API;
with Name_Resolver;

package body CubedOS.Log_Server.Messages is
   use Message_Manager;

   -------------------
   -- Message Handling
   -------------------

   procedure Handle_Log_Text(Message : in Message_Record)
     with
       Global => (Input => (Boot_Time, Current_Time),
                  In_Out => Ada.Text_IO.File_System),
       Pre => Log_Server.API.Is_A_Log_Text(Message)
   is
      use type Ada.Real_Time.Time;

      Log_Level : Log_Server.API.Log_Level_Type;
      Text      : Log_Server.API.Log_Message_Type;
      Size      : Log_Server.API.Log_Message_Size_Type;
      Status    : Message_Status_Type;

      Level_Strings : constant array(Log_Server.API.Log_Level_Type) of String(1 .. 4) :=
        ["DBUG", "INFO", "NOTC", "WARN", "ERRO", "CRIT", "ALRT", "EMRG"];

      Now : constant Ada.Real_Time.Time_Span := Current_Time - Boot_Time;
   begin
      Log_Server.API.Log_Text_Decode(Message, Log_Level, Text, Size, Status);

      -- Ignore log messages that don't decode properly.
      -- TODO: We should also time stamp the messages.
      if Status = Success then
         Ada.Text_IO.Put_Line
           (Level_Strings(Log_Level) & ": " & Duration'Image(Ada.Real_Time.To_Duration(Now)) &
            ", FROM: ("  & Domain_ID_Type'Image(Message.Sender_Address.Domain_ID) &
            ", "   & Module_ID_Type'Image(Message.Sender_Address.Module_ID) &
            "): " & Text(1 .. Size));
      end if;
   end Handle_Log_Text;

   -----------------------------------
   -- Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process(Message : in Message_Record)
     with Global => (Input => (Boot_Time, Current_Time),
                     In_Out => Ada.Text_IO.File_System)
   is
   begin
      if Log_Server.API.Is_A_Log_Text(Message) then
         Handle_Log_Text(Message);
      else
         -- An unknown message type has been received. What should be done about that?
         -- It seems like this should be logged somehow, but do we log to the logger while in the logger?
         null;
      end if;
   end Process;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
   begin
      -- No messages will arrive before this time, since no messages are yet fetched.
      Boot_Time := Ada.Real_Time.Clock;
      loop
         Message_Manager.Fetch_Message(Name_Resolver.Log_Server.Module_ID, Incoming_Message);

         -- This time includes message passing overhead, but not processing overhead in this module.
         Current_Time := Ada.Real_Time.Clock;
         Process(Incoming_Message);
      end loop;
   end Message_Loop;

end CubedOS.Log_Server.Messages;
